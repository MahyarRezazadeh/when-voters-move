"""Implied change in party composition of origin counties and house districts from the
flood-induced out-moves (exploratory, 2026-11-18).

For each unit u: excess leavers of party p = beta_p x (flooded baseline voters in u), where
beta_p is the DiD coefficient on (moved to another unit AND baseline party p), estimated on the
500 m sample and applied to all voters at flooded addresses (unambiguous geocode). The change in
a party's share of the unit's registered voters is computed as if these voters left and nobody
arrived, which is the largest change the out-moves can produce in the origin unit.

Usage: py -I code/19_composition_implied.py     -> output/tables/composition_implied.csv
"""
import sys
from pathlib import Path
import duckdb, pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from config import PROC, OUT

TAB = OUT / "tables"
# DiD coefficients in percentage points of flooded voters (output/tables/decomp_beta_*, dmargin_did_*)
BETA = {("florence", "county"): dict(ALL=0.2529, DEM=0.0474, REP=0.1383),
        ("helene", "county"): dict(ALL=0.9814, DEM=0.3505, REP=0.2356),
        ("florence", "district"): dict(ALL=0.2190, DEM=0.0861, REP=0.1269),
        ("helene", "district"): dict(ALL=1.1142, DEM=0.4413, REP=0.2700)}
EV = {"florence": ("20180508", "fl"), "helene": ("20240514", "he")}
con = duckdb.connect()
con.execute("PRAGMA memory_limit='1GB'; PRAGMA threads=2")
rows = []
for (ev, unit), bt in BETA.items():
    b, e = EV[ev]
    c = f"read_parquet('{(PROC / f'cohort_{ev}_{b}.parquet').as_posix()}') c"
    if unit == "county":
        src, u = c, "c.county_id"
    else:
        src, u = c + f" JOIN read_parquet('{(PROC / f'dist_{ev}_{b}.parquet').as_posix()}') d USING (pid)", "d.hd_o"
    d = con.execute(f"""SELECT {u} AS u, count(*) n, sum((party_cd='DEM')::INT) dem, sum((party_cd='REP')::INT) rep,
          sum((tier IS NOT NULL AND tier <> 'unmatched' AND NOT ambiguous AND {e}_fl > 0.5)::INT) nfl
        FROM {src} WHERE {u} IS NOT NULL GROUP BY 1""").df()
    d = d[d.nfl > 0].copy()
    for p in ("ALL", "DEM", "REP"):
        d["x_" + p] = bt[p] / 100 * d.nfl
    d["d_dem_pp"] = 100 * ((d.dem - d.x_DEM) / (d.n - d.x_ALL) - d.dem / d.n)
    d["d_rep_pp"] = 100 * ((d.rep - d.x_REP) / (d.n - d.x_ALL) - d.rep / d.n)
    d["d_margin_pp"] = d.d_rep_pp - d.d_dem_pp          # Republican minus Democratic share
    d["pct_flooded"] = 100 * d.nfl / d.n
    t = d[["n", "dem", "rep", "nfl", "x_ALL", "x_DEM", "x_REP"]].sum()
    tot_dem = 100 * ((t.dem - t.x_DEM) / (t.n - t.x_ALL) - t.dem / t.n)
    tot_rep = 100 * ((t.rep - t.x_REP) / (t.n - t.x_ALL) - t.rep / t.n)
    m = d.loc[d.d_margin_pp.abs().idxmax()]
    rows.append(dict(event=ev, unit=unit, units_with_flooded_voters=len(d), registered=int(t.n), flooded=int(t.nfl),
                     excess_leavers=round(t.x_ALL, 1), excess_dem=round(t.x_DEM, 1), excess_rep=round(t.x_REP, 1),
                     all_units_d_dem_pp=tot_dem, all_units_d_rep_pp=tot_rep,
                     median_abs_d_margin_pp=d.d_margin_pp.abs().median(),
                     max_abs_d_margin_pp=m.d_margin_pp, max_unit_pct_flooded=m.pct_flooded,
                     max_unit_excess_leavers=round(m.x_ALL, 1), max_unit_registered=int(m.n),
                     max_pct_flooded_any_unit=d.pct_flooded.max()))
out = pd.DataFrame(rows)
pd.set_option("display.width", 250); pd.set_option("display.max_columns", 30)
print(out.T.to_string())
out.to_csv(TAB / "composition_implied.csv", index=False)
