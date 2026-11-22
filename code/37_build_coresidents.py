"""Helene: new co-residents of each baseline adult at follow-up, and their demographics (exploratory, 2026-11-20).

One row per baseline adult of the estimation cohort (pid) for each 16-month window.
Household = registered (A/I) adults sharing one normalised address (with unit) in one county.
  obs, moved     observed at follow-up; at an address other than the baseline address
  n_f            registered adults at the person's follow-up address (including the person)
  n_new          co-residents at follow-up who were not in the person's baseline household
  n_inc          of these, co-residents who were already registered at that address at baseline (incumbents)
  inc_old18, inc_yng18, inc_sim   incumbents at least 18 years older / younger / within 18 years of the person
  inc_partner    incumbents of the other sex within 15 years of age
  inc_65         incumbents aged 65 or over
  inc_age_sum, inc_age_n, inc_f, inc_white, inc_same   age sum and count, women, white, same race as the person
Addresses with more than 10 registered adults at follow-up (group quarters, large buildings without unit numbers)
are flagged by n_f and get no co-resident detail.
Output (local disk only, never printed): PROC/cor_helene_<b>.parquet
Usage: py -I code/37_build_coresidents.py [baseline ...]
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
dbf = PROC / "duck_tmp" / "cores_helene.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")


def tab(name, d):
    con.execute(f"""CREATE OR REPLACE TABLE {name} AS
      SELECT ncid, hash(county_id, addr) AS h, {AGE} AS age, trim(sex_code) AS sex, trim(race_code) AS race FROM (
        SELECT ncid, county_id, age, sex_code, race_code, {ADDR} AS addr,
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
        FROM {snap(d)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') WHERE rn = 1""")


for b, f in WIN:
    tab("bh", b); tab("fu", f)
    coh = f"read_parquet('{(PROC / f'cohort_helene_{b}.parquet').as_posix()}')"
    # sample persons: estimation cohort within 500 m of the flood boundary
    con.execute(f"""CREATE OR REPLACE TABLE s AS
      SELECT p.pid, bh.ncid, bh.h AS hb, fu.h AS hf, fu.age AS age, fu.sex AS sex, fu.race AS race
      FROM (SELECT pid FROM {coh} WHERE he_fl >= 0 AND abs(he_dist) <= 500) c
      JOIN pid p USING (pid) JOIN bh ON bh.ncid = p.ncid LEFT JOIN fu ON fu.ncid = bh.ncid""")
    con.execute("CREATE OR REPLACE TABLE cnt AS SELECT h, count(*) AS n_f FROM fu WHERE h IN (SELECT DISTINCT hf FROM s) GROUP BY 1")
    # co-residents at follow-up (addresses with at most 10 registered adults), with their baseline address
    con.execute("""CREATE OR REPLACE TABLE co AS
      SELECT s.pid, s.hb, s.hf, s.age AS age_i, s.sex AS sex_i, s.race AS race_i, j.age, j.sex, j.race, bj.h AS hbj
      FROM s JOIN cnt ON cnt.h = s.hf AND cnt.n_f <= 10 JOIN fu j ON j.h = s.hf AND j.ncid <> s.ncid
      LEFT JOIN bh bj ON bj.ncid = j.ncid""")
    con.execute("""CREATE OR REPLACE TABLE ag AS
      SELECT pid, sum((hbj IS NULL OR hbj <> hb)::INT) AS n_new,
             sum(inc) AS n_inc, sum(inc * (age - age_i >= 18)::INT) AS inc_old18, sum(inc * (age_i - age >= 18)::INT) AS inc_yng18,
             sum(inc * (abs(age - age_i) < 18)::INT) AS inc_sim,
             sum(inc * (abs(age - age_i) <= 15 AND sex IN ('F','M') AND sex_i IN ('F','M') AND sex <> sex_i)::INT) AS inc_partner,
             sum(inc * (age >= 65)::INT) AS inc_65, sum(CASE WHEN inc = 1 THEN age END) AS inc_age_sum,
             sum(inc * (age IS NOT NULL)::INT) AS inc_age_n, sum(inc * (sex = 'F')::INT) AS inc_f, sum(inc * (race = 'W')::INT) AS inc_white,
             sum(inc * (race = race_i)::INT) AS inc_same
      FROM (SELECT *, (hbj IS NOT NULL AND hbj = hf AND hbj <> hb)::INT AS inc FROM co) GROUP BY 1""")
    out = PROC / f"cor_helene_{b}.parquet"
    con.execute(f"""COPY (
      SELECT s.pid, (s.hf IS NOT NULL)::INT AS obs, coalesce((s.hf <> s.hb)::INT, 0) AS moved, cnt.n_f, s.age AS age_f,
             coalesce(ag.n_new, 0) AS n_new, coalesce(ag.n_inc, 0) AS n_inc, coalesce(ag.inc_old18, 0) AS inc_old18,
             coalesce(ag.inc_yng18, 0) AS inc_yng18, coalesce(ag.inc_sim, 0) AS inc_sim, coalesce(ag.inc_partner, 0) AS inc_partner,
             coalesce(ag.inc_65, 0) AS inc_65, ag.inc_age_sum, coalesce(ag.inc_age_n, 0) AS inc_age_n, coalesce(ag.inc_f, 0) AS inc_f,
             coalesce(ag.inc_white, 0) AS inc_white, coalesce(ag.inc_same, 0) AS inc_same
      FROM s LEFT JOIN cnt ON cnt.h = s.hf LEFT JOIN ag USING (pid)) TO '{out.as_posix()}' (FORMAT parquet)""")
    print(b, f, con.execute(f"SELECT count(*), round(avg(obs),3), round(avg(moved),3) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
