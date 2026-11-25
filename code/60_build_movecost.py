"""Helene: per-person moving-cost measures for each adult who changed address (2026-11-22).

For adults whose baseline address is within 500 m of the Helene flood boundary (study counties, geocoded, matched, not
ambiguous) and who changed address in a 16-month window:
  dist_km    straight-line distance between the geocoded baseline and follow-up addresses (missing if the follow-up
             address is not geocoded)
  n_comove   number of OTHER baseline household members (same normalised address incl. unit at baseline) registered at
             the same new address at follow-up (people who share the move and its fixed costs)
  n_obs_hh   baseline household members observed anywhere at follow-up
Output (local disk only, never printed): PROC/mc_helene_<b>.parquet (pid, dist_km, n_comove, n_obs_hh)
Usage: py -I code/60_build_movecost.py
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
geo = lambda d: f"read_parquet('{(INTER / 'geocode' / f'geo_{d}.parquet').as_posix()}')"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / "movecost_helene.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")
ap = f"read_parquet('{(INTER / 'geocode' / 'addresspoints_flood.parquet').as_posix()}')"
study = ",".join(f"'{r[0]}'" for r in con.execute(f"SELECT DISTINCT county_id FROM {ap} WHERE he_fl > 0").fetchall())

for b, f in WIN:
    coh = f"read_parquet('{(PROC / f'cohort_helene_{b}.parquet').as_posix()}')"
    hid = f"read_parquet('{(PROC / f'hhid_helene_{b}.parquet').as_posix()}')"
    con.execute(f"""CREATE OR REPLACE TABLE mv AS
      SELECT c.pid, c.lat AS lat0, c.lon AS lon0, h.hb
      FROM {coh} c JOIN {hid} h USING (pid)
      WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0 AND abs(c.he_dist) <= 500
        AND c.county_id IN ({study}) AND c.addr_{f}""")
    con.execute(f"""CREATE OR REPLACE TABLE mem AS
      SELECT h.pid, h.hb, p.ncid FROM {hid} h JOIN pid p USING (pid) WHERE h.hb IN (SELECT DISTINCT hb FROM mv)""")
    con.execute(f"""CREATE OR REPLACE TABLE fu AS
      SELECT * EXCLUDE rn FROM (
        SELECT ncid, county_id, voter_reg_num, hash(county_id, {ADDR}) AS hf,
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
        FROM {snap(f)} WHERE trim(status_cd) IN ('A','I') AND ncid IN (SELECT ncid FROM mem)) WHERE rn = 1""")
    con.execute(f"""CREATE OR REPLACE TABLE mf AS
      SELECT mem.pid, mem.hb, fu.hf, g.lat AS lat1, g.lon AS lon1
      FROM mem JOIN fu USING (ncid)
      LEFT JOIN (SELECT county_id, voter_reg_num, lat, lon FROM {geo(f)} WHERE tier <> 'unmatched' AND NOT ambiguous) g
        ON g.county_id = fu.county_id AND g.voter_reg_num = fu.voter_reg_num""")
    out = PROC / f"mc_helene_{b}.parquet"
    con.execute(f"""COPY (
      SELECT mv.pid,
             CASE WHEN me.lat1 IS NOT NULL THEN 2 * 6371 * asin(sqrt(power(sin(radians(me.lat1 - mv.lat0) / 2), 2)
                  + cos(radians(mv.lat0)) * cos(radians(me.lat1)) * power(sin(radians(me.lon1 - mv.lon0) / 2), 2))) END AS dist_km,
             (SELECT count(*) FROM mf o WHERE o.hb = mv.hb AND o.hf = me.hf AND o.pid <> mv.pid) AS n_comove,
             (SELECT count(*) FROM mf o WHERE o.hb = mv.hb) AS n_obs_hh
      FROM mv JOIN mf me ON me.pid = mv.pid) TO '{out.as_posix()}' (FORMAT parquet)""")
    print(b, f, con.execute(f"""SELECT count(*), round(median(dist_km), 2), round(avg((dist_km IS NULL)::INT), 3),
        round(avg(n_comove), 3) FROM read_parquet('{out.as_posix()}')""").fetchone(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
