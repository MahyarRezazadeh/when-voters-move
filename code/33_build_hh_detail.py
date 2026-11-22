"""Helene households: detail of the response and baseline characteristics (exploratory, 2026-11-20).

One row per baseline household (hb) for each 16-month window. Household = registered (A/I) adults sharing
one normalised address (with unit) in one county at baseline.
Response at follow-up
  n, nobs      members at baseline, members observed at follow-up
  n_stay       observed members still at the baseline address
  n_move       observed members at another address
  n_dest       number of different addresses among the members who moved
  n_out        movers registered in another county
  age_move, age_stay   mean age of movers and of stayers
Baseline characteristics
  age_min, age_max, n_f, n_m, n_white, n_nc (born in NC), n_bp (birthplace recorded),
  reg_first, reg_last (year of the earliest and latest registration date among members)
Output (local disk only, never printed): PROC/hhd_helene_<b>.parquet
Usage: py -I code/33_build_hh_detail.py
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC

WIN = [("20210101", "20220517"), ("20220517", "20230912"), ("20230101", "20240514"), ("20240514", "20250909")]
ADDR = ("upper(concat_ws('|', ltrim(trim(house_num),'0'), trim(half_code), trim(street_dir), "
        "regexp_replace(trim(street_name),'\\s+',' ','g'), trim(street_type_cd), trim(street_sufx_cd), "
        "trim(unit_num), left(trim(zip_code),5)))")
snap = lambda d: f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / "hhdetail_helene.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")


def dedup(d, cols):
    return f"""SELECT * EXCLUDE rn FROM (
        SELECT ncid, county_id, {cols},
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
        FROM {snap(d)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') WHERE rn = 1"""


for b, f in WIN:
    con.execute(f"""CREATE OR REPLACE TABLE bh AS
      SELECT ncid, county_id, hash(county_id, addr) AS hb, trim(sex_code) AS sex, trim(race_code) AS race, trim(birth_place) AS bp,
             coalesce(year(try_strptime(registr_dt,'%m/%d/%Y')), year(try_strptime(registr_dt,'%Y-%m-%d'))) AS regy,
             CASE WHEN try_cast(age AS INT) BETWEEN 17 AND 110 THEN try_cast(age AS INT) END AS age
      FROM ({dedup(b, f"sex_code, race_code, birth_place, age, registr_dt AS registr_dt, {ADDR} AS addr")})""")
    con.execute(f"CREATE OR REPLACE TABLE fu AS SELECT ncid, county_id AS cf, hash(county_id, addr) AS hf FROM ({dedup(f, f'{ADDR} AS addr')})")
    out = PROC / f"hhd_helene_{b}.parquet"
    con.execute(f"""COPY (
      SELECT bh.hb, count(*) AS n, sum((fu.ncid IS NOT NULL)::INT) AS nobs,
             sum((fu.hf = bh.hb)::INT) AS n_stay, sum((fu.hf <> bh.hb)::INT) AS n_move,
             count(DISTINCT CASE WHEN fu.hf <> bh.hb THEN fu.hf END) AS n_dest,
             sum((fu.hf <> bh.hb AND fu.cf <> bh.county_id)::INT) AS n_out,
             avg(CASE WHEN fu.hf <> bh.hb THEN bh.age END) AS age_move, avg(CASE WHEN fu.hf = bh.hb THEN bh.age END) AS age_stay,
             min(bh.age) AS age_min, max(bh.age) AS age_max, sum((bh.sex='F')::INT) AS n_f, sum((bh.sex='M')::INT) AS n_m,
             sum((bh.race='W')::INT) AS n_white, sum((bh.bp='NC')::INT) AS n_nc, sum((bh.bp<>'')::INT) AS n_bp,
             min(bh.regy) AS reg_first, max(bh.regy) AS reg_last
      FROM bh LEFT JOIN fu USING (ncid) GROUP BY 1) TO '{out.as_posix()}' (FORMAT parquet)""")
    print(b, f, con.execute(f"SELECT count(*) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
