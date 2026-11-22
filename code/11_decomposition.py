"""County x party accounting decomposition of the registered electorate (plan Section 7).

  N_up(f) - N_up(b) = In - Out + SwitchIn - SwitchOut + Entry - Exit
Convention: move first, then switch. In/Out are counted with the BASELINE party, switches at
the FOLLOW-UP county. Entry = NCID active/inactive at f and not at b; Exit = active/inactive at
b and not at f. N(f) and Entry come from the follow-up snapshot, all other terms from the
statewide cohort file, so the identity is a real check, not true by construction.
Also: mobility-only counterfactual (baseline party held fixed; composition of the voters seen
at both dates under follow-up vs baseline county).

Usage: py -I code/11_decomposition.py <event>      (florence: 20180508 -> 20190910;
                                                    helene:   20240514 -> 20250909)
Outputs (aggregates only; cells below 10 suppressed in the county file):
  output/tables/decomp_county_<event>.csv, decomp_identity_<event>.csv,
  decomp_studyarea_<event>.csv, decomp_mobility_<event>.csv
"""
import sys
from pathlib import Path
import duckdb, pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC, OUT

EV = {"florence": ("20180508", "20190910", "fl"), "helene": ("20240514", "20250909", "he")}
ev = sys.argv[1]; b, f, e = EV[ev]
TAB = OUT / "tables"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / f"decomp_{ev}.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='2500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
PG = "CASE WHEN {c} IN ('DEM','REP','UNA','LIB') THEN {c} ELSE 'OTH' END"
snap = lambda d: f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"
coh = f"read_parquet('{(PROC / f'cohort_{ev}_{b}.parquet').as_posix()}')"
ap = f"read_parquet('{(INTER / 'geocode' / 'addresspoints_flood.parquet').as_posix()}')"

# follow-up electorate: one A/I record per NCID (same rule as 06_build_cohort.py), flagged entrant
con.execute(f"""CREATE TABLE fu AS
  SELECT ncid, county_id, {PG.format(c='party_cd')} AS pg FROM (
    SELECT ncid, county_id, party_cd,
           row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) AS rn
    FROM {snap(f)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') WHERE rn = 1""")
con.execute(f"CREATE TABLE bn AS SELECT DISTINCT ncid FROM {snap(b)} WHERE trim(status_cd) IN ('A','I') AND ncid <> ''")
con.execute("""CREATE TABLE nf AS SELECT fu.county_id AS u, fu.pg AS p, count(*) AS n_f,
                 sum((bn.ncid IS NULL)::INT) AS entry
               FROM fu LEFT JOIN bn ON bn.ncid = fu.ncid GROUP BY 1,2""")
con.execute("DROP TABLE fu; DROP TABLE bn;")
# cohort flows
con.execute(f"""CREATE TABLE c AS SELECT county_id AS ub, {PG.format(c='party_cd')} AS pb, obs_{f} AS obs,
                  dcounty_{f} AS uf, {PG.format(c=f'dparty_{f}')} AS pf FROM {coh}""")
con.execute("""CREATE TABLE comp AS
  SELECT u, p, sum(n_b) n_b, sum(outm) outm, sum(inm) inm, sum(sw_out) sw_out, sum(sw_in) sw_in, sum(exits) exits FROM (
    SELECT ub u, pb p, count(*) n_b, sum((obs AND uf <> ub)::INT) outm, 0 inm, 0 sw_out, 0 sw_in, sum((NOT obs)::INT) exits
      FROM c GROUP BY 1,2
    UNION ALL SELECT uf, pb, 0, 0, count(*), 0, 0, 0 FROM c WHERE obs AND uf <> ub GROUP BY 1,2
    UNION ALL SELECT uf, pb, 0, 0, 0, count(*), 0, 0 FROM c WHERE obs AND pf <> pb GROUP BY 1,2
    UNION ALL SELECT uf, pf, 0, 0, 0, 0, count(*), 0 FROM c WHERE obs AND pf <> pb GROUP BY 1,2) GROUP BY 1,2""")
d = con.execute("""SELECT coalesce(comp.u, nf.u) u, coalesce(comp.p, nf.p) p, coalesce(n_b,0) n_b, coalesce(n_f,0) n_f,
    coalesce(inm,0) inm, coalesce(outm,0) outm, coalesce(sw_in,0) sw_in, coalesce(sw_out,0) sw_out,
    coalesce(entry,0) entry, coalesce(exits,0) exits
  FROM comp FULL JOIN nf ON nf.u = comp.u AND nf.p = comp.p ORDER BY 1,2""").df()
d["resid"] = d.n_f - (d.n_b + d.inm - d.outm + d.sw_in - d.sw_out + d.entry - d.exits)
ident = pd.DataFrame([{"event": ev, "baseline": b, "followup": f, "cells": len(d), "N_b": int(d.n_b.sum()),
                       "N_f": int(d.n_f.sum()), "cells_resid_nonzero": int((d.resid != 0).sum()),
                       "max_abs_resid": int(d.resid.abs().max()), "sum_abs_resid": int(d.resid.abs().sum())}])
print(ident.to_string()); ident.to_csv(TAB / f"decomp_identity_{ev}.csv", index=False)

study = set(con.execute(f"SELECT DISTINCT county_id FROM {ap} WHERE {e}_fl > 0").df().county_id)
names = con.execute(f"SELECT county_id, any_value(county_desc) nm FROM {snap(b)} GROUP BY 1").df() \
    if "county_desc" in con.execute(f"DESCRIBE SELECT * FROM {snap(b)}").df().column_name.values else None
d["study_area"] = d.u.isin(study)
flows = ["inm", "outm", "sw_in", "sw_out", "entry", "exits"]
pub = d.copy()
for cname in flows:  # suppression rule: cells below 10
    pub[cname] = pub[cname].where(pub[cname] >= 10)
pub.drop(columns="resid").to_csv(TAB / f"decomp_county_{ev}.csv", index=False)

# study-area and rest-of-state totals by party: change in party share and its components
rows = []
for area, g in d.groupby("study_area"):
    t = g.groupby("p")[["n_b", "n_f"] + flows].sum()
    Nb, Nf = t.n_b.sum(), t.n_f.sum()
    t["share_b_pct"] = 100 * t.n_b / Nb; t["share_f_pct"] = 100 * t.n_f / Nf
    t["d_share_pp"] = t.share_f_pct - t.share_b_pct
    # share change if only one channel had operated (others off), in percentage points
    for nm, net in {"migration": t.inm - t.outm, "switching": t.sw_in - t.sw_out, "entry": t.entry, "exit": -t.exits}.items():
        t[f"d_share_{nm}_pp"] = 100 * ((t.n_b + net) / (Nb + net.sum()) - t.n_b / Nb)
    t.insert(0, "area", "study counties" if area else "rest of NC"); t.insert(0, "event", ev)
    rows.append(t.reset_index())
sa = pd.concat(rows); print(sa.round(3).to_string()); sa.to_csv(TAB / f"decomp_studyarea_{ev}.csv", index=False)

# mobility-only counterfactual: voters observed at both dates, baseline party fixed
m = con.execute("""SELECT u, p, sum(nb) nb, sum(nf) nf FROM (
     SELECT ub u, pb p, count(*) nb, 0 nf FROM c WHERE obs GROUP BY 1,2
     UNION ALL SELECT uf, pb, 0, count(*) FROM c WHERE obs GROUP BY 1,2) GROUP BY 1,2""").df()
m["sb"] = 100 * m.nb / m.groupby("u").nb.transform("sum"); m["sf"] = 100 * m.nf / m.groupby("u").nf.transform("sum")
m["d_share_pp"] = m.sf - m.sb; m["study_area"] = m.u.isin(study)
w = m.pivot(index="u", columns="p", values="d_share_pp")
tot = m.groupby("u").agg(n_base=("nb", "sum"), n_follow=("nf", "sum"), study_area=("study_area", "first"))
mob = tot.join(w.add_prefix("d_share_pp_")).reset_index()
mob["d_dem_minus_rep_pp"] = mob.d_share_pp_DEM - mob.d_share_pp_REP
if names is not None:
    mob = mob.merge(names, left_on="u", right_on="county_id", how="left").drop(columns="county_id")
mob.insert(0, "event", ev)
mob.sort_values("d_dem_minus_rep_pp").to_csv(TAB / f"decomp_mobility_{ev}.csv", index=False)
s = mob[mob.study_area]
print("mobility-only change in DEM-minus-REP share, study counties (pp): min %.3f median %.3f max %.3f; n=%d" % (
    s.d_dem_minus_rep_pp.min(), s.d_dem_minus_rep_pp.median(), s.d_dem_minus_rep_pp.max(), len(s)))
print("rest of NC: min %.3f median %.3f max %.3f" % tuple(
    mob[~mob.study_area].d_dem_minus_rep_pp.agg(["min", "median", "max"])))
con.close()
for s_ in ("", ".wal"):
    Path(str(dbf) + s_).unlink(missing_ok=True)
