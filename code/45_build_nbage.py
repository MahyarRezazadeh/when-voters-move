"""Helene: age composition of neighbourhoods (Census block groups) and each adult's neighbourhood at baseline
and follow-up (exploratory, 2026-11-20).

(1) PROC/bgage_<d>.parquet  aggregate counts of geocoded registered (A/I) adults by block group, county and
    municipality at snapshot d: n, n_u35 (under 35), n_65 (65 or over), age_sum.
(2) PROC/nb_helene_<b>.parquet  one row per baseline adult of the estimation cohort (pid): bg_b (block group of the
    baseline address), bg_f (block group of the geocoded follow-up address; missing if not observed or not geocoded).
Local disk only, never printed. Usage: py -I code/45_build_nbage.py
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC

DATES = ["20210101", "20220517", "20230101", "20230912", "20240514", "20250909"]
WIN = [("20210101", "20220517"), ("20220517", "20230912"), ("20230101", "20240514"), ("20240514", "20250909")]
snap = lambda d: f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"
geo = lambda d: f"read_parquet('{(INTER / 'geocode' / f'geo_{d}.parquet').as_posix()}')"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / "nbage_helene.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")

for d in DATES:
    out = PROC / f"bgage_{d}.parquet"
    con.execute(f"""CREATE OR REPLACE TABLE sa AS
      SELECT county_id, voter_reg_num, trim(municipality_abbrv) AS muni,
             CASE WHEN try_cast(age AS INT) BETWEEN 17 AND 110 THEN try_cast(age AS INT) END AS age
      FROM {snap(d)} WHERE trim(status_cd) IN ('A','I')""")
    con.execute(f"""COPY (
      SELECT substr(g.geoid, 1, 12) AS bg, g.county_id, sa.muni, count(*) AS n, sum((sa.age < 35)::INT) AS n_u35,
             sum((sa.age >= 65)::INT) AS n_65, sum(sa.age) AS age_sum
      FROM {geo(d)} g JOIN sa ON sa.county_id = g.county_id AND sa.voter_reg_num = g.voter_reg_num
      WHERE g.tier <> 'unmatched' AND NOT g.ambiguous AND sa.age IS NOT NULL AND g.geoid IS NOT NULL
      GROUP BY 1, 2, 3) TO '{out.as_posix()}' (FORMAT parquet)""")
    print("bg", d, con.execute(f"SELECT count(DISTINCT bg), sum(n) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)
con.execute("DROP TABLE sa")
con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")
for b, f in WIN:
    coh = f"read_parquet('{(PROC / f'cohort_helene_{b}.parquet').as_posix()}')"
    con.execute(f"""CREATE OR REPLACE TABLE s AS
      SELECT c.pid, p.ncid, substr(c.geoid, 1, 12) AS bg_b, c.dcounty_{f} AS dcounty
      FROM {coh} c JOIN pid p USING (pid) WHERE c.he_fl >= 0 AND abs(c.he_dist) <= 500""")
    con.execute(f"""CREATE OR REPLACE TABLE gd AS
      SELECT ncid, county_id, any_value(substr(geoid, 1, 12)) AS bg FROM {geo(f)}
      WHERE ncid IN (SELECT ncid FROM s) AND tier <> 'unmatched' AND NOT ambiguous GROUP BY 1, 2""")
    out = PROC / f"nb_helene_{b}.parquet"
    con.execute(f"""COPY (SELECT s.pid, s.bg_b, gd.bg AS bg_f FROM s LEFT JOIN gd ON gd.ncid = s.ncid AND gd.county_id = s.dcounty)
                    TO '{out.as_posix()}' (FORMAT parquet)""")
    print("nb", b, f, con.execute(f"SELECT count(*), round(avg((bg_f IS NOT NULL)::INT),3), round(avg((bg_f <> bg_b)::INT),3) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
