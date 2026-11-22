"""Party registration in households that split, moved together, or stayed (exploratory, 2026-11-18).

Household = A/I voters sharing the same normalized address (with unit) in one county at baseline b,
two to six voters, at least two of them observed (A/I) at follow-up f. Only observed members are
used, so the household's membership is the same at both dates.
  stayed    all observed members at the baseline address at f
  together  all observed members at one new address at f
  split     observed members at more than one address at f
Party is the party of registration (DEM, REP, UNA, other), not a vote.
Statewide, and for the storm's estimation sample (flooded / unflooded within 500 m).
Aggregates only -> output/tables/hh_party_households.csv, hh_party_voters.csv
Usage: py -I code/25_hh_party.py
"""
import sys
from pathlib import Path
import duckdb, pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC, OUT

COH = [("florence", "fl", "20170101", "20180508", "pre-storm"), ("florence", "fl", "20180508", "20190910", "storm"),
       ("helene", "he", "20230101", "20240514", "pre-storm"), ("helene", "he", "20240514", "20250909", "storm")]
ADDR = ("upper(concat_ws('|', ltrim(trim(house_num),'0'), trim(half_code), trim(street_dir), "
        "regexp_replace(trim(street_name),'\\s+',' ','g'), trim(street_type_cd), trim(street_sufx_cd), "
        "trim(unit_num), left(trim(zip_code),5)))")
PG = "CASE WHEN trim(party_cd) IN ('DEM','REP','UNA') THEN trim(party_cd) ELSE 'OTH' END"
snap = lambda d: f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / "hhparty.duckdb"
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


H, V = [], []
for ev, e, b, f, period in COH:
    con.execute(f"CREATE OR REPLACE TABLE bh AS {dedup(b)}")
    con.execute(f"CREATE OR REPLACE TABLE fu AS {dedup(f)}")
    con.execute("""CREATE OR REPLACE TABLE m AS
      SELECT bh.ncid, bh.h AS hb, bh.pg AS pb, fu.h AS hf, fu.pg AS pf, (fu.ncid IS NOT NULL) AS obs,
             count(*) OVER (PARTITION BY bh.h) AS hh_n
      FROM bh LEFT JOIN fu ON fu.ncid = bh.ncid""")
    con.execute("""CREATE OR REPLACE TABLE hh AS
      SELECT hb, any_value(hh_n) hh_n, count(*) n_obs, count(DISTINCT hf) n_hf, bool_and(hf = hb) all_home,
             count(DISTINCT pb) np_b, count(DISTINCT pf) np_f, mode(pb) plur
      FROM m WHERE obs GROUP BY 1""")
    con.execute("""CREATE OR REPLACE TABLE hh2 AS
      SELECT *, CASE WHEN n_hf > 1 THEN 'split' WHEN all_home THEN 'stayed' ELSE 'together' END AS type
      FROM hh WHERE hh_n BETWEEN 2 AND 6 AND n_obs >= 2""")
    # flood status of the household: from the storm sample of this event (any member in the 500 m sample)
    con.execute(f"""CREATE OR REPLACE TABLE fl AS
      SELECT m.hb, max(({e}_fl > 0.5)::INT) flooded
      FROM read_parquet('{(PROC / f'cohort_{ev}_{b}.parquet').as_posix()}') c JOIN pid p USING (pid) JOIN m ON m.ncid = p.ncid
      WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.{e}_fl >= 0 AND abs(c.{e}_dist) <= 500
      GROUP BY 1""")
    for grp, src in (("statewide", "hh2"), ("sample", "hh2 JOIN fl USING (hb)")):
        g = "'all' AS grp" if grp == "statewide" else "CASE WHEN flooded = 1 THEN 'flooded' ELSE 'unflooded within 500 m' END AS grp"
        d = con.execute(f"""SELECT {g}, type, (np_b = 1) AS uniform_b, count(*) households, sum(n_obs) voters,
                 sum((np_f = 1)::INT) uniform_f FROM {src} GROUP BY 1,2,3""").df()
        d.insert(0, "period", period); d.insert(0, "event", ev); H.append(d)
        d = con.execute(f"""SELECT {g}, type,
                 CASE WHEN type = 'split' THEN (CASE WHEN m.hf = m.hb THEN 'remained' ELSE 'moved out' END) ELSE 'all' END AS role,
                 (np_b = 1) AS uniform_b, m.pb AS party_b, count(*) voters, sum((m.pf <> m.pb)::INT) changed_party,
                 sum((m.pb <> plur AND m.pf = plur)::INT) joined_household_plurality,
                 sum((m.pb = plur AND m.pf <> plur)::INT) left_household_plurality,
                 sum((m.pf = 'DEM')::INT) f_dem, sum((m.pf = 'REP')::INT) f_rep, sum((m.pf = 'UNA')::INT) f_una
               FROM {src} JOIN m USING (hb) WHERE m.obs GROUP BY 1,2,3,4,5""").df()
        d.insert(0, "period", period); d.insert(0, "event", ev); V.append(d)
    print(ev, period, "done", flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
H = pd.concat(H, ignore_index=True); V = pd.concat(V, ignore_index=True)
for T, c in ((H, "households"), (V, "voters")):      # suppression rule: cells below 10
    T.drop(T[T[c] < 10].index, inplace=True)
H["pct_uniform_f"] = 100 * H.uniform_f / H.households
V["pct_changed_party"] = 100 * V.changed_party / V.voters
H.sort_values(["event", "period", "grp", "uniform_b", "type"]).to_csv(OUT / "tables" / "hh_party_households.csv", index=False)
V.sort_values(["event", "period", "grp", "type", "role", "uniform_b", "party_b"]).to_csv(OUT / "tables" / "hh_party_voters.csv", index=False)
pd.set_option("display.width", 250); pd.set_option("display.max_rows", 200)
print(H[H.grp == "all"].sort_values(["event", "period", "uniform_b", "type"]).round(2).to_string(index=False))
