"""Build one voter cohort (baseline snapshot b) with outcomes at follow-up snapshots F.

Usage: py code/06_build_cohort.py <event> <baseline> <followup1> [<followup2> ...]
Output: DATA_ROOT/processed/cohort_<event>_<baseline>.parquet (one row per baseline NCID).

Baseline population: every NCID with an ACTIVE (A) or INACTIVE (I) record in snapshot b.
If an NCID has several A/I records (duplicates across counties), the record with the latest
registration date is kept; the number of A/I records is stored (n_ai_base) for diagnostics.

Outcomes at each follow-up f (suffix _<f>), defined for ALL baseline NCIDs:
  obs_f      NCID has an A/I record anywhere in snapshot f
  cc_f       obs_f and county_f != county_b                       (PRIMARY at ~16 months)
  addr_f     obs_f and (county changed or normalized street address changed)
  wcaddr_f   obs_f and same county and normalized address changed (within-county move)
  party_f    obs_f and party_f != party_b
  exit_f     not obs_f
  exitrsn_f  reason code on the baseline-county record in f (R = removed; e.g. moved, death)
  dcounty_f  destination county id (for decomposition)
  dparty_f   follow-up party
  dhouse_f   NC House district of the DESTINATION ADDRESS under BASELINE boundaries, looked up
             from baseline-snapshot voters registered at that same normalized address
             (NULL if no baseline voter lived there); avoids treating redistricting as moves.
Identifiers: the NCID is replaced by a pseudonymous integer pid (stable across cohorts via a
local lookup table DATA_ROOT/processed/pid_map.parquet, never published).
"""
import sys, time, json
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC, LOGS

ADDR = ("upper(concat_ws('|', ltrim(trim(house_num),'0'), trim(half_code), trim(street_dir), "
        "regexp_replace(trim(street_name),'\\s+',' ','g'), trim(street_type_cd), trim(street_sufx_cd), "
        "trim(unit_num), left(trim(zip_code),5)))")


def snap(d):
    return f"read_parquet('{(INTER / 'snapshots' / f'snap_{d}.parquet').as_posix()}')"


def ai_dedup(d, alias):
    """One A/I record per NCID in snapshot d."""
    return f"""
      SELECT * EXCLUDE rn FROM (
        SELECT ncid, county_id, voter_reg_num, party_cd, precinct_abbrv, NC_house_abbrv, {ADDR} AS addr,
               trim(status_cd) AS status_cd, count(*) OVER (PARTITION BY ncid) AS n_ai,
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST,
                                  county_id) AS rn
        FROM {snap(d)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') WHERE rn = 1"""
# status_cd is space-padded ('A ') from the 2025-09-09 snapshot on; trimmed everywhere (fix 2026-11-16).


def main(ev, b, F):
    t0 = time.time()
    # Work in an on-disk database so the buffer manager can evict tables (an in-memory database
    # ran out of memory on the 7.4 M-voter May 2024 baseline). Deleted at the end.
    (PROC / "duck_tmp").mkdir(exist_ok=True)
    dbf = PROC / "duck_tmp" / f"build_{ev}_{b}.duckdb"
    for s in ("", ".wal"):
        Path(str(dbf) + s).unlink(missing_ok=True)
    con = duckdb.connect(str(dbf))
    con.execute("PRAGMA memory_limit='2500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
    con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
    # baseline with demographics
    con.execute(f"""
      CREATE TABLE base AS
      SELECT * EXCLUDE rn FROM (
        SELECT ncid, county_id, voter_reg_num, party_cd, precinct_abbrv, NC_house_abbrv, NC_senate_abbrv,
               cong_dist_abbrv, municipality_abbrv, {ADDR} AS addr, trim(status_cd) AS status_cd, race_code, ethnic_code, sex_code,
               try_cast(age AS INT) AS age, try_strptime(registr_dt,'%m/%d/%Y') AS registr_dt, confidential_ind,
               (trim(unit_num) <> '') AS has_unit,
               count(*) OVER (PARTITION BY ncid) AS n_ai_base,
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST,
                                  county_id) AS rn
        FROM {snap(b)} WHERE trim(status_cd) IN ('A','I') AND ncid <> '') WHERE rn = 1""")
    # address -> baseline NC House district (modal), for fixed-boundary district moves
    con.execute(f"""CREATE TABLE addr_house AS
        SELECT county_id, addr, mode(NC_house_abbrv) AS house_b FROM base GROUP BY 1,2""")
    geo = (INTER / "geocode" / f"geo_{b}.parquet").as_posix()
    con.execute(f"""CREATE TABLE coh AS
        SELECT base.*, g.* EXCLUDE (county_id, voter_reg_num, ncid)
        FROM base LEFT JOIN read_parquet('{geo}') g ON g.county_id = base.county_id AND g.voter_reg_num = base.voter_reg_num""")
    # Outcomes for each follow-up are written to a narrow file keyed by ncid and joined once at
    # the end. (Re-creating one ever-wider table per follow-up ran out of memory at 5 follow-ups.)
    con.execute("CREATE TABLE key AS SELECT ncid, county_id, voter_reg_num, addr, party_cd FROM base")
    con.execute("DROP TABLE base")
    tmpdir = PROC / "duck_tmp"; tmpdir.mkdir(exist_ok=True)
    parts = []
    for f in F:
        con.execute(f"CREATE OR REPLACE TABLE fu AS {ai_dedup(f, 'fu')}")
        con.execute(f"""CREATE OR REPLACE TABLE rm AS
            SELECT county_id, voter_reg_num, any_value(trim(status_cd)) AS st, any_value(reason_cd) AS rsn
            FROM {snap(f)} GROUP BY 1,2""")
        part = (tmpdir / f"out_{ev}_{b}_{f}.parquet").as_posix(); parts.append(part)
        con.execute(f"""
          COPY (SELECT coh.ncid,
            (fu.ncid IS NOT NULL) AS obs_{f},
            (fu.ncid IS NOT NULL AND fu.county_id <> coh.county_id) AS cc_{f},
            (fu.ncid IS NOT NULL AND (fu.county_id <> coh.county_id OR fu.addr <> coh.addr)) AS addr_{f},
            (fu.ncid IS NOT NULL AND fu.county_id = coh.county_id AND fu.addr <> coh.addr) AS wcaddr_{f},
            (fu.ncid IS NOT NULL AND fu.party_cd <> coh.party_cd) AS party_{f},
            (fu.ncid IS NULL) AS exit_{f},
            CASE WHEN fu.ncid IS NULL THEN coalesce(rm.st,'?') || ':' || coalesce(rm.rsn,'?') END AS exitrsn_{f},
            fu.county_id AS dcounty_{f}, fu.party_cd AS dparty_{f}, fu.status_cd AS dstatus_{f},
            CASE WHEN fu.ncid IS NOT NULL AND (fu.county_id <> coh.county_id OR fu.addr <> coh.addr)
                 THEN ah.house_b END AS dhouse_{f}
          FROM key coh
          LEFT JOIN fu ON fu.ncid = coh.ncid
          LEFT JOIN rm ON rm.county_id = coh.county_id AND rm.voter_reg_num = coh.voter_reg_num
          LEFT JOIN addr_house ah ON ah.county_id = fu.county_id AND ah.addr = fu.addr)
          TO '{part}' (FORMAT parquet)""")
        con.execute("DROP TABLE fu; DROP TABLE rm;")
    joins = " ".join(f"LEFT JOIN read_parquet('{p}') o{k} ON o{k}.ncid = coh.ncid" for k, p in enumerate(parts))
    outs = ", ".join(f"o{k}.* EXCLUDE (ncid)" for k in range(len(parts)))
    # pseudonymous ids
    pm = PROC / "pid_map.parquet"
    if pm.exists():
        con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{pm.as_posix()}')")
    else:
        con.execute("CREATE TABLE pid (ncid VARCHAR, pid BIGINT)")
    # random order is drawn in a subquery: DuckDB rejects volatile window ORDER BY expressions
    con.execute("""INSERT INTO pid SELECT ncid, (SELECT coalesce(max(pid),0) FROM pid) + row_number() OVER (ORDER BY r)
                   FROM (SELECT ncid, random() AS r
                         FROM (SELECT DISTINCT ncid FROM coh WHERE ncid NOT IN (SELECT ncid FROM pid)))""")
    con.execute(f"COPY pid TO '{pm.as_posix()}' (FORMAT parquet)")
    out = PROC / f"cohort_{ev}_{b}.parquet"
    con.execute("DROP TABLE key; DROP TABLE addr_house;")
    con.execute(f"""COPY (SELECT pid.pid, coh.* EXCLUDE (ncid, voter_reg_num, addr), {outs}
                    FROM coh JOIN pid ON pid.ncid = coh.ncid {joins}) TO '{out.as_posix()}' (FORMAT parquet)""")
    for p in parts:
        Path(p).unlink()
    n = con.execute(f"SELECT count(*) FROM read_parquet('{out.as_posix()}')").fetchone()[0]
    rec = {"event": ev, "baseline": b, "followups": F, "n": n, "seconds": round(time.time() - t0)}
    print(json.dumps(rec))
    with open(LOGS / "cohort_log.jsonl", "a") as fh:
        fh.write(json.dumps(rec) + "\n")
    con.close()
    for s in ("", ".wal"):
        Path(str(dbf) + s).unlink(missing_ok=True)


if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2], sys.argv[3:])
