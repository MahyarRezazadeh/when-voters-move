"""Household identifier for each baseline voter (exploratory, 2026-11-19), so that the household
split outcome can be estimated with the household as the unit of observation.
Household = A/I records sharing the same normalized address (with unit) in one county at baseline.
Output (local disk): PROC/hhid_<event>_<b>.parquet (pid, hb)   -- hb is a hash, not an address
Usage: py -I code/27_build_hhid.py <event>
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC

EV = {"florence": ["20150101", "20170101", "20180508"], "helene": ["20210101", "20220517", "20230101", "20240514"]}
ev = sys.argv[1]
ADDR = ("upper(concat_ws('|', ltrim(trim(house_num),'0'), trim(half_code), trim(street_dir), "
        "regexp_replace(trim(street_name),'\\s+',' ','g'), trim(street_type_cd), trim(street_sufx_cd), "
        "trim(unit_num), left(trim(zip_code),5)))")
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / f"hhid_{ev}.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1200MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")
for b in EV[ev]:
    out = PROC / f"hhid_{ev}_{b}.parquet"
    con.execute(f"""COPY (
      SELECT p.pid, hash(x.county_id, x.addr) AS hb FROM (
        SELECT ncid, county_id, {ADDR} AS addr,
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
        FROM read_parquet('{(INTER / 'snapshots' / f'snap_{b}.parquet').as_posix()}')
        WHERE trim(status_cd) IN ('A','I') AND ncid <> '') x JOIN pid p ON p.ncid = x.ncid WHERE x.rn = 1)
      TO '{out.as_posix()}' (FORMAT parquet)""")
    print(b, con.execute(f"SELECT count(*), count(DISTINCT hb) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
