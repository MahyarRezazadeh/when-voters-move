"""Helene households: where members were before the baseline, and who is at the address afterwards
(exploratory, 2026-11-20). One row per baseline household (hb) for each 16-month window.
Household = registered (A/I) adults sharing one normalised address (with unit) in one county at baseline.
Before the baseline (lookback snapshot about 16 months earlier; 24 months for the 2023 baseline)
  n          members at baseline
  n_lk       members registered at the lookback
  n_lk_home  members registered at the baseline address at the lookback
  n_lk_dest  number of different addresses of the members at the lookback
After the baseline (follow-up of the window; for the storm window also 3 Nov 2026)
  n_arr, n_arr_l    registered adults at the baseline address who were not members at baseline
  n_stay_l          members still at the baseline address on 3 Nov 2026
Output (local disk only, never printed): PROC/hhh_helene_<b>.parquet
Usage: py -I code/35_build_hh_history.py
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC

WIN = [w for w in [("20210101", "20220517", "20190910"), ("20220517", "20230912", "20210101"),
       ("20230101", "20240514", "20210101"), ("20240514", "20250909", "20230101")] if len(sys.argv) < 2 or w[0] in sys.argv[1:]]
LONGF = {"20240514": "20261103"}
ADDR = ("upper(concat_ws('|', ltrim(trim(house_num),'0'), trim(half_code), trim(street_dir), "
        "regexp_replace(trim(street_name),'\\s+',' ','g'), trim(street_type_cd), trim(street_sufx_cd), "
        "trim(unit_num), left(trim(zip_code),5)))")
snap = lambda d: f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / "hhhist_helene.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")


def tab(name, d):
    con.execute(f"""CREATE OR REPLACE TABLE {name} AS SELECT ncid, hash(county_id, addr) AS h FROM (
        SELECT ncid, county_id, {ADDR} AS addr,
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
        FROM {snap(d)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') WHERE rn = 1""")


def arrivals(name):
    return f"""SELECT x.h AS hb, count(*) AS k FROM {name} x LEFT JOIN bh ON bh.ncid = x.ncid AND bh.h = x.h
               WHERE bh.ncid IS NULL AND x.h IN (SELECT DISTINCT h FROM bh) GROUP BY 1"""


for b, f, lk in WIN:
    tab("bh", b); tab("fu", f); tab("lk", lk)
    lf = LONGF.get(b)
    if lf:
        tab("fl", lf)
    con.execute("""CREATE OR REPLACE TABLE a AS
        SELECT bh.h AS hb, count(*) AS n, sum((lk.ncid IS NOT NULL)::INT) AS n_lk, coalesce(sum((lk.h = bh.h)::INT), 0) AS n_lk_home,
               count(DISTINCT lk.h) AS n_lk_dest
        FROM bh LEFT JOIN lk USING (ncid) GROUP BY 1""")
    con.execute("DROP TABLE lk")
    con.execute(f"CREATE OR REPLACE TABLE ar AS {arrivals('fu')}")
    con.execute("DROP TABLE fu")
    if lf:
        con.execute(f"CREATE OR REPLACE TABLE al AS {arrivals('fl')}")
        con.execute("CREATE OR REPLACE TABLE sl AS SELECT bh.h AS hb, count(*) AS k FROM bh JOIN fl ON fl.ncid = bh.ncid AND fl.h = bh.h GROUP BY 1")
        con.execute("DROP TABLE fl")
        con.execute("CREATE OR REPLACE TABLE a AS SELECT a.*, coalesce(al.k, 0) AS n_arr_l FROM a LEFT JOIN al ON al.hb = a.hb")
        con.execute("CREATE OR REPLACE TABLE a AS SELECT a.*, coalesce(sl.k, 0) AS n_stay_l FROM a LEFT JOIN sl ON sl.hb = a.hb")
    else:
        con.execute("CREATE OR REPLACE TABLE a AS SELECT a.*, NULL::BIGINT AS n_arr_l, NULL::BIGINT AS n_stay_l FROM a")
    out = PROC / f"hhh_helene_{b}.parquet"
    con.execute(f"COPY (SELECT a.*, coalesce(ar.k, 0) AS n_arr FROM a LEFT JOIN ar ON ar.hb = a.hb) TO '{out.as_posix()}' (FORMAT parquet)")
    print(b, f, lk, con.execute(f"SELECT count(*) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
