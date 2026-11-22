"""How the Helene sample differs from the Florence sample and from the rest of North Carolina,
from the project's own data (exploratory, 2026-11-19). Registered adults at the storm-cohort
baseline, plus ordinary mobility in the last pre-storm window. Aggregates only.
Output: output/tables/region_compare.csv
Usage: py -I code/28_region_compare.py
"""
import sys
from pathlib import Path
import duckdb, pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC, OUT

EV = {"florence": ("fl", "20180508", "20170101", "20180508"), "helene": ("he", "20240514", "20230101", "20240514")}
con = duckdb.connect()
con.execute("PRAGMA memory_limit='1200MB'; PRAGMA threads=2")
ap = f"read_parquet('{(INTER / 'geocode' / 'addresspoints_flood.parquet').as_posix()}')"
rows = []
for ev, (e, b, pb, pf) in EV.items():
    study = ",".join(f"'{r[0]}'" for r in con.execute(f"SELECT DISTINCT county_id FROM {ap} WHERE {e}_fl > 0").fetchall())
    geo_ok = "tier IS NOT NULL AND tier <> 'unmatched' AND NOT ambiguous"
    grp = f"""CASE WHEN {geo_ok} AND {e}_fl > 0.5 AND abs({e}_dist) <= 500 AND county_id IN ({study}) THEN '1 flooded addresses'
                   WHEN {geo_ok} AND {e}_fl >= 0 AND abs({e}_dist) <= 500 AND county_id IN ({study}) THEN '2 unflooded within 500 m'
                   WHEN county_id IN ({study}) THEN '3 rest of study counties' ELSE '4 rest of North Carolina' END"""
    c = lambda x: f"read_parquet('{(PROC / f'cohort_{ev}_{x}.parquet').as_posix()}')"
    h = lambda x: f"read_parquet('{(PROC / f'hh_{ev}_{x}.parquet').as_posix()}')"
    a = con.execute(f"""SELECT {grp} AS grp, count(*) voters,
          avg(CASE WHEN age BETWEEN 17 AND 110 THEN age END) mean_age,
          100.0*avg(CASE WHEN age BETWEEN 17 AND 110 THEN (age >= 65)::INT END) pct_65plus,
          100.0*avg(CASE WHEN age BETWEEN 17 AND 110 THEN (age < 35)::INT END) pct_under35,
          100.0*avg((race_code = 'W')::INT) pct_white, 100.0*avg((race_code = 'B')::INT) pct_black,
          100.0*avg((party_cd = 'DEM')::INT) pct_dem, 100.0*avg((party_cd = 'REP')::INT) pct_rep, 100.0*avg((party_cd = 'UNA')::INT) pct_una,
          100.0*avg(has_unit::INT) pct_unit_number, 100.0*avg((trim(status_cd) = 'I')::INT) pct_inactive,
          100.0*avg((hh.hh_n = 1)::INT) pct_alone, 100.0*avg((hh.hh_n BETWEEN 2 AND 6)::INT) pct_hh_2_6,
          avg(CASE WHEN hh.hh_n BETWEEN 1 AND 6 THEN hh.hh_n END) mean_adults_at_address,
          100.0*avg(hh.couple::INT) pct_in_couple_household,
          100.0*avg((hh.hh_n BETWEEN 3 AND 6)::INT) pct_hh_3_6
        FROM {c(b)} c JOIN {h(b)} hh USING (pid) GROUP BY 1""").df()
    m = con.execute(f"""SELECT {grp} AS grp, 100.0*avg(addr_{pf}::INT) pre_any_address_change_pct,
          100.0*avg(cc_{pf}::INT) pre_other_county_pct, 100.0*avg(exit_{pf}::INT) pre_exit_pct,
          100.0*avg(CASE WHEN hh.hh_n BETWEEN 2 AND 6 THEN hh.split END) pre_split_pct,
          100.0*avg(CASE WHEN hh.hh_n BETWEEN 2 AND 6 THEN hh.together END) pre_moved_together_pct
        FROM {c(pb)} c JOIN {h(pb)} hh USING (pid) GROUP BY 1""").df()
    d = a.merge(m, on="grp"); d.insert(0, "event", ev); rows.append(d)
o = pd.concat(rows, ignore_index=True).sort_values(["event", "grp"])
o.to_csv(OUT / "tables" / "region_compare.csv", index=False)
pd.set_option("display.width", 250); pd.set_option("display.max_columns", 40)
print(o.set_index(["event", "grp"]).T.round(1).to_string())
