"""Address-level and household-level outcomes for each cohort (exploratory, 2026-11-18).

Outcome 1, replacement at the address. Unit: baseline address (geocoded location) of the
estimation sample. At follow-up f:
  n_f       registered (A/I) voters geocoded to the location
  n_arr     arrivals: voters at the location at f who were not at it at baseline b
  n_arr_new arrivals with no A/I record anywhere at b (new registrants)
  arr_dem, arr_rep, arr_una, arr_white, arr_black, arr_age_sum, arr_age_n   composition of arrivals
Outcome 2, household splitting. Household = A/I voters sharing the same normalized address
(with unit) in the same county at b. For baseline voter i:
  hh_n      household size at b
  split     i observed at f and the household members observed at f are at more than one address
  together  i observed at f at a new address and all observed members share that address
  couple    two-person household, one F and one M, age gap of 15 years or less
Outputs (voter- and address-level, local disk only):
  PROC/addr_<event>_<b>.parquet, PROC/hh_<event>_<b>.parquet
Usage: py -I code/20_build_addr_hh.py <event>
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC

EV = {"florence": dict(e="fl", ctrl="mt", cohorts=[("20150101", "20160607"), ("20170101", "20180508"), ("20180508", "20190910")]),
      "helene": dict(e="he", ctrl=None, cohorts=[("20210101", "20220517"), ("20220517", "20230912"),
                                                  ("20230101", "20240514"), ("20240514", "20250909")])}
ev = sys.argv[1]; cfg = EV[ev]; e = cfg["e"]; BW = 500
ADDR = ("upper(concat_ws('|', ltrim(trim(house_num),'0'), trim(half_code), trim(street_dir), "
        "regexp_replace(trim(street_name),'\\s+',' ','g'), trim(street_type_cd), trim(street_sufx_cd), "
        "trim(unit_num), left(trim(zip_code),5)))")
snap = lambda d: f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"
geo = lambda d: f"read_parquet('{(INTER / 'geocode' / f'geo_{d}.parquet').as_posix()}')"
coh = lambda b: f"read_parquet('{(PROC / f'cohort_{ev}_{b}.parquet').as_posix()}')"
ap = f"read_parquet('{(INTER / 'geocode' / 'addresspoints_flood.parquet').as_posix()}')"

(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / f"addrhh_{ev}.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")
study = [r[0] for r in con.execute(f"SELECT DISTINCT county_id FROM {ap} WHERE {e}_fl > 0").fetchall()]
inlist = ",".join(f"'{c}'" for c in study)


def dedup(d, cols):
    return f"""SELECT * EXCLUDE rn FROM (
        SELECT ncid, county_id, voter_reg_num, {cols},
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
        FROM {snap(d)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') WHERE rn = 1"""


for b, f in cfg["cohorts"]:
    # ---------------- follow-up records with pid, location and household key ----------------
    con.execute(f"""CREATE OR REPLACE TABLE fu AS
      SELECT p.pid, x.ncid, hash(x.county_id, x.addr) AS hf, x.party_cd, x.race_code, try_cast(x.age AS INT) AS age,
             g.lat, g.lon
      FROM ({dedup(f, f"party_cd, race_code, age, {ADDR} AS addr")}) x
      LEFT JOIN pid p ON p.ncid = x.ncid
      LEFT JOIN (SELECT county_id, voter_reg_num, lat, lon FROM {geo(f)} WHERE tier <> 'unmatched' AND NOT ambiguous) g
        ON g.county_id = x.county_id AND g.voter_reg_num = x.voter_reg_num""")
    # ---------------- outcome 1: address level ----------------
    con.execute(f"""CREATE OR REPLACE TABLE bl AS
      SELECT pid, CASE WHEN tier IS NOT NULL AND tier <> 'unmatched' AND NOT ambiguous THEN lat END AS lat,
                  CASE WHEN tier IS NOT NULL AND tier <> 'unmatched' AND NOT ambiguous THEN lon END AS lon FROM {coh(b)}""")
    con.execute("""CREATE OR REPLACE TABLE agg AS
      SELECT fu.lat, fu.lon, count(*) n_f,
        sum(arr) n_arr, sum(arr * (bl.pid IS NULL)::INT) n_arr_new,
        sum(arr * (party_cd='DEM')::INT) arr_dem, sum(arr * (party_cd='REP')::INT) arr_rep, sum(arr * (party_cd='UNA')::INT) arr_una,
        sum(arr * (race_code='W')::INT) arr_white, sum(arr * (race_code='B')::INT) arr_black,
        sum(CASE WHEN arr = 1 AND age BETWEEN 17 AND 110 THEN age ELSE 0 END) arr_age_sum,
        sum((arr = 1 AND age BETWEEN 17 AND 110)::INT) arr_age_n
      FROM (SELECT fu.*, bl.pid AS bpid,
                   (bl.pid IS NULL OR bl.lat IS NULL OR bl.lat <> fu.lat OR bl.lon <> fu.lon)::INT AS arr
            FROM fu LEFT JOIN bl ON bl.pid = fu.pid WHERE fu.lat IS NOT NULL) fu
      LEFT JOIN bl ON bl.pid = fu.pid
      GROUP BY 1,2""")
    ctrl = f", any_value({cfg['ctrl']}_fl) AS ctrl_fl" if cfg["ctrl"] else ""
    out = PROC / f"addr_{ev}_{b}.parquet"
    con.execute(f"""COPY (
      SELECT a.*, coalesce(g.n_f,0) n_f, coalesce(g.n_arr,0) n_arr, coalesce(g.n_arr_new,0) n_arr_new,
             coalesce(g.arr_dem,0) arr_dem, coalesce(g.arr_rep,0) arr_rep, coalesce(g.arr_una,0) arr_una,
             coalesce(g.arr_white,0) arr_white, coalesce(g.arr_black,0) arr_black,
             coalesce(g.arr_age_sum,0) arr_age_sum, coalesce(g.arr_age_n,0) arr_age_n
      FROM (SELECT lat, lon, any_value(substr(geoid,1,12)) bg, max(({e}_fl > 0.5)::INT) flooded {ctrl}, count(*) n_b,
                   sum((party_cd='DEM')::INT) b_dem, sum((party_cd='REP')::INT) b_rep
            FROM {coh(b)}
            WHERE tier IS NOT NULL AND tier <> 'unmatched' AND NOT ambiguous AND {e}_fl >= 0 AND abs({e}_dist) <= {BW}
              AND county_id IN ({inlist}) GROUP BY 1,2) a
      LEFT JOIN agg g ON g.lat = a.lat AND g.lon = a.lon) TO '{out.as_posix()}' (FORMAT parquet)""")
    print(b, f, con.execute(f"""SELECT count(*) addresses, sum(flooded) flooded, sum(n_b) voters_b, sum(n_f) voters_f,
        sum(n_arr) arrivals, 100.0*avg((n_f=0)::INT) pct_vacant FROM read_parquet('{out.as_posix()}')""").df().to_string(), flush=True)
    # ---------------- outcome 2: households ----------------
    con.execute(f"""CREATE OR REPLACE TABLE bh AS
      SELECT ncid, hash(county_id, addr) AS hb, sex_code, try_cast(age AS INT) AS age
      FROM ({dedup(b, f"sex_code, age, {ADDR} AS addr")})""")
    con.execute("""CREATE OR REPLACE TABLE m AS
      SELECT bh.ncid, bh.hb, bh.sex_code, bh.age, fu.hf, (fu.ncid IS NOT NULL) AS obs
      FROM bh LEFT JOIN fu ON fu.ncid = bh.ncid""")
    con.execute("""CREATE OR REPLACE TABLE hh AS
      SELECT hb, count(*) hh_n, sum(obs::INT) n_obs, count(DISTINCT CASE WHEN obs THEN hf END) n_hf,
             (count(*) = 2 AND sum((trim(sex_code)='F')::INT) = 1 AND sum((trim(sex_code)='M')::INT) = 1
              AND max(age) - min(age) <= 15) AS couple
      FROM m GROUP BY 1""")
    out = PROC / f"hh_{ev}_{b}.parquet"
    con.execute(f"""COPY (
      SELECT p.pid, hh.hh_n, (m.obs AND hh.n_hf > 1)::INT AS split,
             (m.obs AND hh.n_obs >= 2 AND hh.n_hf = 1 AND m.hf <> m.hb)::INT AS together, coalesce(hh.couple, false) AS couple
      FROM m JOIN hh USING (hb) JOIN pid p ON p.ncid = m.ncid) TO '{out.as_posix()}' (FORMAT parquet)""")
    print(b, f, con.execute(f"""SELECT count(*) voters, 100.0*avg((hh_n BETWEEN 2 AND 6)::INT) pct_in_hh_2_6,
        100.0*avg(CASE WHEN hh_n BETWEEN 2 AND 6 THEN split END) pct_split,
        100.0*avg(CASE WHEN hh_n BETWEEN 2 AND 6 THEN together END) pct_together,
        100.0*avg(couple::INT) pct_couple FROM read_parquet('{out.as_posix()}')""").df().to_string(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
