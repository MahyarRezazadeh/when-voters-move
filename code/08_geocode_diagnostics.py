"""Does address matching depend on flood exposure? (feasibility diagnostic, aggregates only)

A street is (county, standardized street name, street type). A street is 'flooded' for storm e
if at least one AddressNC point on it is flooded under the primary map. Unmatched voters have
no location, so I compare the match rate of voters on flooded vs unflooded streets in the
storm's study counties (counties with >= 1 flooded point), for each geocoded baseline.

Reading: AddressNC is a current file. If homes destroyed by Florence were dropped from it,
the flooded-minus-unflooded match gap should be more negative for pre-Florence baselines
(2015-2018) than for the post-storm 2024 baseline (when those homes no longer hold voters).

Output: output/diagnostics/geocode_match_by_flood_street.csv (cells < 10 suppressed).
Usage: py code/08_geocode_diagnostics.py 20150101 20160607 20170101 20180508 ... 20240514
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, OUT

geo_dir = INTER / "geocode"
ap = (geo_dir / "addresspoints_flood.parquet").as_posix()
con = duckdb.connect()
con.execute("PRAGMA memory_limit='2GB'; PRAGMA threads=3; SET enable_progress_bar=false;")
con.execute(f"PRAGMA temp_directory='{(INTER / 'duck_tmp').as_posix()}'")
con.execute(f"""CREATE TEMP TABLE streets AS
  SELECT county_id, sn, st, max(fl_fl > 0)::INT AS fl_street, max(mt_fl > 0)::INT AS mt_street,
         max(he_fl > 0)::INT AS he_street
  FROM read_parquet('{ap}') GROUP BY 1, 2, 3""")
rows = []
for d in sys.argv[1:]:
    snap = (INTER / "snapshots" / f"snap_{d}.parquet").as_posix()
    geo = (geo_dir / f"geo_{d}.parquet").as_posix()
    con.execute(f"""CREATE OR REPLACE TEMP TABLE v AS
      SELECT s.county_id, regexp_replace(upper(trim(s.street_name)),'\\s+',' ','g') AS sn,
             nullif(upper(trim(s.street_type_cd)),'') AS st,
             (g.tier <> 'unmatched')::INT AS matched,
             (g.tier <> 'unmatched' AND NOT coalesce(g.ambiguous, false))::INT AS usable
      FROM read_parquet('{snap}') s
      JOIN read_parquet('{geo}') g USING (county_id, voter_reg_num)
      WHERE s.status_cd IN ('A','I')""")
    for e in ("fl", "mt", "he"):
        res = con.execute(f"""
          WITH sc AS (SELECT DISTINCT county_id FROM streets WHERE {e}_street = 1)
          SELECT coalesce(x.{e}_street, -1) AS street_flooded, count(*) AS voters,
                 avg(matched) AS match_rate, avg(usable) AS usable_rate
          FROM v LEFT JOIN streets x USING (county_id, sn, st)
          WHERE v.county_id IN (SELECT county_id FROM sc)
          GROUP BY 1 ORDER BY 1""").fetchall()
        for f, n, m, u in res:
            rows.append((d, e, {1: "flooded street", 0: "unflooded street", -1: "street not in AddressNC"}[f],
                         n if n >= 10 else None, round(m, 4) if n >= 10 else None, round(u, 4) if n >= 10 else None))
    print(d, "done", flush=True)
out = OUT / "diagnostics" / "geocode_match_by_flood_street.csv"
with open(out, "w") as fh:
    fh.write("baseline,storm,street_group,voters,match_rate,usable_rate\n")
    for r in rows:
        fh.write(",".join("" if x is None else str(x) for x in r) + "\n")
print("wrote", out)
