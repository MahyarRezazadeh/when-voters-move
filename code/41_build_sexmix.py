"""Helene: sex of the registered adults a person lives with, at baseline and at follow-up
(exploratory, 2026-11-20). One row per baseline adult of the estimation cohort (pid) for each 16-month window.
Household = registered (A/I) adults sharing one normalised address (with unit) in one county.
  sex                 the person's recorded sex (F, M, or other/unrecorded)
  b_n, b_w, b_m       registered adults, women and men at the baseline address (including the person)
  f_n, f_w, f_m       the same at the person's follow-up address (missing if the person is not observed)
Output (local disk only, never printed): PROC/sx_helene_<b>.parquet
Usage: py -I code/41_build_sexmix.py [baseline ...]
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC

WIN = [w for w in [("20210101", "20220517"), ("20220517", "20230912"), ("20230101", "20240514"), ("20240514", "20250909")]
       if len(sys.argv) < 2 or w[0] in sys.argv[1:]]
ADDR = ("upper(concat_ws('|', ltrim(trim(house_num),'0'), trim(half_code), trim(street_dir), "
        "regexp_replace(trim(street_name),'\\s+',' ','g'), trim(street_type_cd), trim(street_sufx_cd), "
        "trim(unit_num), left(trim(zip_code),5)))")
snap = lambda d: f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"
AGE = "CASE WHEN try_cast(age AS INT) BETWEEN 17 AND 110 THEN try_cast(age AS INT) END"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / "sexmix_helene.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")


def tab(name, d):
    con.execute(f"""CREATE OR REPLACE TABLE {name} AS
      SELECT ncid, hash(county_id, addr) AS h, trim(sex_code) AS sex FROM (
        SELECT ncid, county_id, sex_code, {ADDR} AS addr,
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
        FROM {snap(d)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') WHERE rn = 1""")


for b, f in WIN:
    tab("bh", b); tab("fu", f)
    coh = f"read_parquet('{(PROC / f'cohort_helene_{b}.parquet').as_posix()}')"
    con.execute(f"""CREATE OR REPLACE TABLE s AS
      SELECT p.pid, bh.sex, bh.h AS hb, fu.h AS hf
      FROM (SELECT pid FROM {coh} WHERE he_fl >= 0 AND abs(he_dist) <= 500) c
      JOIN pid p USING (pid) JOIN bh ON bh.ncid = p.ncid LEFT JOIN fu ON fu.ncid = bh.ncid""")
    con.execute("CREATE OR REPLACE TABLE gb AS SELECT h, count(*) AS n, sum((sex='F')::INT) AS w, sum((sex='M')::INT) AS m FROM bh WHERE h IN (SELECT DISTINCT hb FROM s) GROUP BY 1")
    con.execute("CREATE OR REPLACE TABLE gf AS SELECT h, count(*) AS n, sum((sex='F')::INT) AS w, sum((sex='M')::INT) AS m FROM fu WHERE h IN (SELECT DISTINCT hf FROM s) GROUP BY 1")
    out = PROC / f"sx_helene_{b}.parquet"
    con.execute(f"""COPY (SELECT s.pid, s.sex, gb.n AS b_n, gb.w AS b_w, gb.m AS b_m, gf.n AS f_n, gf.w AS f_w, gf.m AS f_m
                          FROM s LEFT JOIN gb ON gb.h = s.hb LEFT JOIN gf ON gf.h = s.hf) TO '{out.as_posix()}' (FORMAT parquet)""")
    print(b, f, con.execute(f"SELECT count(*), round(avg(b_w/b_n),3), round(avg(f_w/f_n),3) FROM read_parquet('{out.as_posix()}') WHERE b_n <= 6 AND f_n <= 10").fetchone(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
