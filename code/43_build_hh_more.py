"""Helene households: further outcomes at follow-up (exploratory, 2026-11-20).
One row per baseline household (hb) of the estimation cohort for each 16-month window; counts of members with:
  k_cc        registered in another county
  k_dead      removed from the file, reason deceased (RD)
  k_rm, k_rs  removed, reason moved from county (RM) / moved from state (RS)
  k_exit      not registered (A/I) at follow-up, any reason
  k_inact     active at baseline, inactive at follow-up
  k_mail      no mailing address at baseline, a mailing address at follow-up
  k_mailout   mailing address outside NC at follow-up and not at baseline
  k_tounit    moved from an address without unit number to an address with one
  k_near, k_mid, k_far   moved less than 5 km / 5 to 50 km / more than 50 km (geocoded destinations)
  k_destfl    moved to an address inside the Helene flood layer
  k_desthi    moved to an address more than 500 m outside the flood layer
  k_geo       movers with a geocoded destination;  n members
Output (local disk only, never printed): PROC/hhm_helene_<b>.parquet
Usage: py -I code/43_build_hh_more.py [baseline ...]
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
geo = lambda d: f"read_parquet('{(INTER / 'geocode' / f'geo_{d}.parquet').as_posix()}')"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / "hhmore_helene.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")


def rec(name, d):
    con.execute(f"""CREATE OR REPLACE TABLE {name} AS
      SELECT ncid, county_id, hash(county_id, addr) AS h, trim(unit_num) <> '' AS unit, trim(mail_state) AS ms FROM (
        SELECT ncid, county_id, unit_num, mail_state, {ADDR} AS addr,
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) rn
        FROM {snap(d)} WHERE trim(status_cd) IN ('A','I') AND ncid IN (SELECT ncid FROM s)) WHERE rn = 1""")


for b, f in WIN:
    coh = f"read_parquet('{(PROC / f'cohort_helene_{b}.parquet').as_posix()}')"
    hid = f"read_parquet('{(PROC / f'hhid_helene_{b}.parquet').as_posix()}')"
    con.execute(f"""CREATE OR REPLACE TABLE s AS
      SELECT c.pid, p.ncid, i.hb, c.lat, c.lon, trim(c.status_cd) AS st_b, c.cc_{f} AS cc, c.exit_{f} AS ex, c.exitrsn_{f} AS rsn,
             trim(c.dstatus_{f}) AS st_f, c.dcounty_{f} AS dcounty
      FROM {coh} c JOIN pid p USING (pid) JOIN {hid} i USING (pid) WHERE c.he_fl >= 0 AND abs(c.he_dist) <= 500""")
    rec("rb", b); rec("rf", f)
    con.execute(f"""CREATE OR REPLACE TABLE gd AS
      SELECT g.ncid, g.county_id, any_value(g.lat) AS lat, any_value(g.lon) AS lon, any_value(g.he_fl) AS he_fl, any_value(g.he_dist) AS he_dist
      FROM {geo(f)} g WHERE g.ncid IN (SELECT ncid FROM s) AND g.tier <> 'unmatched' AND NOT g.ambiguous GROUP BY 1, 2""")
    con.execute("""CREATE OR REPLACE TABLE m AS
      SELECT s.*, rb.unit AS unit_b, rb.ms AS ms_b, rf.unit AS unit_f, rf.ms AS ms_f, (rf.h IS NOT NULL AND rf.h <> s.hb) AS moved,
             gd.he_fl AS dfl, gd.he_dist AS ddist,
             6371 * 2 * asin(sqrt(pow(sin(radians(gd.lat - s.lat) / 2), 2) + cos(radians(s.lat)) * cos(radians(gd.lat)) * pow(sin(radians(gd.lon - s.lon) / 2), 2))) AS km
      FROM s LEFT JOIN rb ON rb.ncid = s.ncid LEFT JOIN rf ON rf.ncid = s.ncid
             LEFT JOIN gd ON gd.ncid = s.ncid AND gd.county_id = rf.county_id""")
    out = PROC / f"hhm_helene_{b}.parquet"
    con.execute(f"""COPY (
      SELECT hb, count(*) AS n, sum(coalesce(cc, false)::INT) AS k_cc, sum((rsn = 'R:RD')::INT) AS k_dead, sum((rsn = 'R:RM')::INT) AS k_rm,
             sum((rsn = 'R:RS')::INT) AS k_rs, sum(coalesce(ex, false)::INT) AS k_exit, sum((st_b = 'A' AND st_f = 'I')::INT) AS k_inact,
             sum((ms_b = '' AND ms_f <> '')::INT) AS k_mail, sum((ms_f NOT IN ('', 'NC') AND ms_b IN ('', 'NC'))::INT) AS k_mailout,
             sum((moved AND NOT unit_b AND unit_f)::INT) AS k_tounit,
             sum((moved AND km < 5)::INT) AS k_near, sum((moved AND km >= 5 AND km <= 50)::INT) AS k_mid, sum((moved AND km > 50)::INT) AS k_far,
             sum((moved AND dfl > 0.5)::INT) AS k_destfl, sum((moved AND dfl < 0.5 AND ddist > 500)::INT) AS k_desthi,
             sum((moved AND km IS NOT NULL)::INT) AS k_geo, sum(coalesce(moved, false)::INT) AS k_moved
      FROM m GROUP BY 1) TO '{out.as_posix()}' (FORMAT parquet)""")
    print(b, f, con.execute(f"SELECT count(*), sum(k_moved), sum(k_geo), sum(k_dead), sum(k_inact), sum(k_mail) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
