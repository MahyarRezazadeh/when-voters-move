"""Voter-level household party outcomes for every cohort (exploratory, 2026-11-18), for the
event-study versions of the household party comparisons in 25_hh_party.py.

Household = A/I voters at the same normalized address (with unit) in one county at baseline b,
two to six voters, at least two observed (A/I) at follow-up f. Only observed members are used.
Per observed voter:
  uniform_b  all observed members had the same party at b
  uniform_f  all observed members have the same party at f
  changed    the voter's own party at f differs from b
  type       stayed / together / split (as in 25_hh_party.py)
Party = DEM, REP, UNA, other (party of registration).
Output (local disk): PROC/hhp_<event>_<b>.parquet (pid, uniform_b, uniform_f, changed, type)
Usage: py -I code/26_build_hh_party.py <event>
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC

EV = {"florence": [("20150101", "20160607"), ("20170101", "20180508"), ("20180508", "20190910")],
      "helene": [("20210101", "20220517"), ("20220517", "20230912"), ("20230101", "20240514"), ("20240514", "20250909")]}
ev = sys.argv[1]
ADDR = ("upper(concat_ws('|', ltrim(trim(house_num),'0'), trim(half_code), trim(street_dir), "
        "regexp_replace(trim(street_name),'\\s+',' ','g'), trim(street_type_cd), trim(street_sufx_cd), "
        "trim(unit_num), left(trim(zip_code),5)))")
PG = "CASE WHEN trim(party_cd) IN ('DEM','REP','UNA') THEN trim(party_cd) ELSE 'OTH' END"
snap = lambda d: f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / f"hhp_{ev}.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1200MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")


def dedup(d):
    return f"""SELECT ncid, hash(county_id, addr) AS h, pg FROM (
        SELECT ncid, county_id, {ADDR} AS addr, {PG} AS pg,
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
        FROM {snap(d)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') WHERE rn = 1"""


for b, f in EV[ev]:
    con.execute(f"CREATE OR REPLACE TABLE bh AS {dedup(b)}")
    con.execute(f"CREATE OR REPLACE TABLE fu AS {dedup(f)}")
    con.execute("""CREATE OR REPLACE TABLE m AS
      SELECT bh.ncid, bh.h AS hb, bh.pg AS pb, fu.h AS hf, fu.pg AS pf, (fu.ncid IS NOT NULL) AS obs,
             count(*) OVER (PARTITION BY bh.h) AS hh_n
      FROM bh LEFT JOIN fu ON fu.ncid = bh.ncid""")
    con.execute("""CREATE OR REPLACE TABLE hh AS
      SELECT hb, any_value(hh_n) hh_n, count(*) n_obs, count(DISTINCT hf) n_hf, bool_and(hf = hb) all_home,
             count(DISTINCT pb) np_b, count(DISTINCT pf) np_f
      FROM m WHERE obs GROUP BY 1""")
    out = PROC / f"hhp_{ev}_{b}.parquet"
    con.execute(f"""COPY (
      SELECT p.pid, (hh.np_b = 1)::INT AS uniform_b, (hh.np_f = 1)::INT AS uniform_f, (m.pf <> m.pb)::INT AS changed,
             CASE WHEN hh.n_hf > 1 THEN 'split' WHEN hh.all_home THEN 'stayed' ELSE 'together' END AS type
      FROM m JOIN hh USING (hb) JOIN pid p ON p.ncid = m.ncid
      WHERE m.obs AND hh.hh_n BETWEEN 2 AND 6 AND hh.n_obs >= 2) TO '{out.as_posix()}' (FORMAT parquet)""")
    print(b, f, con.execute(f"""SELECT count(*) voters, 100.0*avg(uniform_b) pct_uniform_b,
        100.0*avg(CASE WHEN uniform_b = 1 THEN 1 - uniform_f END) pct_diverged,
        100.0*avg(CASE WHEN uniform_b = 0 THEN uniform_f END) pct_converged, 100.0*avg(changed) pct_changed
        FROM read_parquet('{out.as_posix()}')""").df().to_string(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
