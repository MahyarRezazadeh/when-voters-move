"""Helene households: household type at baseline and long-window outcomes (exploratory, 2026-11-19).

For every baseline of the four 16-month windows: household type of each registered adult's household.
  htype   'couple'   two members, one F and one M, age gap of 15 years or less
          'multigen' age gap between oldest and youngest member of 18 years or more
          'other'    everything else (same-generation members who are not a mixed-sex pair, or age/sex missing)
  old65   oldest member aged 65 or over
For the long windows (about 24-32 months): where members are registered at the later follow-up.
  split_l     members observed at the long follow-up are at more than one address
  together_l  all observed members (two or more) share one new address
  home_l      all observed members (two or more) are at the baseline address
  nobs_l      number of members observed at the long follow-up
Household = registered (A/I) adults sharing one normalised address (with unit) in one county.
Outputs (local disk only, never printed): PROC/hht_helene_<b>.parquet, PROC/hhl_helene_<b>.parquet
Usage: py -I code/31_build_hh_long.py
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC

BASE = ["20210101", "20220517", "20230101", "20240514"]
LONG = [("20210101", "20230912"), ("20220517", "20240514"), ("20240514", "20261103")]
ADDR = ("upper(concat_ws('|', ltrim(trim(house_num),'0'), trim(half_code), trim(street_dir), "
        "regexp_replace(trim(street_name),'\\s+',' ','g'), trim(street_type_cd), trim(street_sufx_cd), "
        "trim(unit_num), left(trim(zip_code),5)))")
snap = lambda d: f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / "hhlong_helene.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")


def dedup(d, cols):
    return f"""SELECT * EXCLUDE rn FROM (
        SELECT ncid, county_id, {cols},
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
        FROM {snap(d)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') WHERE rn = 1"""


def base_table(b):
    con.execute(f"""CREATE OR REPLACE TABLE bh AS
      SELECT ncid, hash(county_id, addr) AS hb, trim(sex_code) AS sex,
             CASE WHEN try_cast(age AS INT) BETWEEN 17 AND 110 THEN try_cast(age AS INT) END AS age
      FROM ({dedup(b, f"sex_code, age, {ADDR} AS addr")})""")


for b in BASE:
    base_table(b)
    out = PROC / f"hht_helene_{b}.parquet"
    con.execute(f"""COPY (
      SELECT p.pid, t.htype, t.old65 FROM bh JOIN (
        SELECT hb, CASE WHEN count(*) = 2 AND sum((sex='F')::INT) = 1 AND sum((sex='M')::INT) = 1 AND max(age) - min(age) <= 15 THEN 'couple'
                        WHEN max(age) - min(age) >= 18 THEN 'multigen' ELSE 'other' END AS htype,
               coalesce(max(age) >= 65, false)::INT AS old65
        FROM bh GROUP BY 1) t USING (hb) JOIN pid p ON p.ncid = bh.ncid) TO '{out.as_posix()}' (FORMAT parquet)""")
    print("type", b, con.execute(f"SELECT count(*) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)

for b, f in LONG:
    base_table(b)
    con.execute(f"CREATE OR REPLACE TABLE fu AS SELECT ncid, hash(county_id, addr) AS hf FROM ({dedup(f, f'{ADDR} AS addr')})")
    con.execute("CREATE OR REPLACE TABLE m AS SELECT bh.ncid, bh.hb, fu.hf, (fu.ncid IS NOT NULL) AS obs FROM bh LEFT JOIN fu USING (ncid)")
    out = PROC / f"hhl_helene_{b}.parquet"
    con.execute(f"""COPY (
      SELECT p.pid, h.nobs_l, (h.n_hf > 1)::INT AS split_l,
             (h.nobs_l >= 2 AND h.n_hf = 1 AND h.any_hf <> m.hb)::INT AS together_l,
             (h.nobs_l >= 2 AND h.n_hf = 1 AND h.any_hf = m.hb)::INT AS home_l
      FROM m JOIN (SELECT hb, sum(obs::INT) nobs_l, count(DISTINCT CASE WHEN obs THEN hf END) n_hf,
                          any_value(CASE WHEN obs THEN hf END) any_hf FROM m GROUP BY 1) h USING (hb)
      JOIN pid p ON p.ncid = m.ncid) TO '{out.as_posix()}' (FORMAT parquet)""")
    print("long", b, f, con.execute(f"SELECT count(*) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
