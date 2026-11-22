"""Cross-precinct outcome (plan Section 4, secondary outcome 2, fallback definition).

xprec = 1 if the voter is active/inactive at follow-up and either (a) is registered in another
county, or (b) is in the same county, the normalized street address changed, and the precinct
label differs from baseline. Requiring an address change excludes pure precinct relabels.
Computed only for voters in the 500 m estimation sample.
Output: DATA_ROOT/processed/xprec_<event>_<baseline>.parquet (pid, xprec).
Usage: py -I code/12_followup_precinct.py <event>
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC

DES = {"florence": ("fl", [("20150101", "20160607"), ("20170101", "20180508"), ("20180508", "20190910")]),
       "helene": ("he", [("20210101", "20220517"), ("20220517", "20230912"), ("20230101", "20240514"),
                         ("20240514", "20250909")])}
ev = sys.argv[1]; e, cohorts = DES[ev]
(PROC / "duck_tmp").mkdir(exist_ok=True)
for b, f in cohorts:
    dbf = PROC / "duck_tmp" / f"xprec_{ev}_{b}.duckdb"
    for s in ("", ".wal"):
        Path(str(dbf) + s).unlink(missing_ok=True)
    con = duckdb.connect(str(dbf))
    con.execute("PRAGMA memory_limit='2000MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
    con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
    coh = (PROC / f"cohort_{ev}_{b}.parquet").as_posix()
    con.execute(f"""CREATE TABLE s AS
        SELECT c.pid, m.ncid, c.precinct_abbrv AS prec_b, c.cc_{f} AS cc, c.wcaddr_{f} AS wc
        FROM read_parquet('{coh}') c JOIN read_parquet('{(PROC / 'pid_map.parquet').as_posix()}') m ON m.pid = c.pid
        WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.{e}_fl >= 0
          AND abs(c.{e}_dist) <= 500""")
    con.execute(f"""CREATE TABLE fu AS SELECT ncid, precinct_abbrv AS prec_f FROM (
        SELECT ncid, precinct_abbrv,
               row_number() OVER (PARTITION BY ncid ORDER BY try_strptime(registr_dt,'%m/%d/%Y') DESC NULLS LAST, county_id) AS rn
        FROM read_parquet('{(INTER / 'snapshots' / f'snap_{f}.parquet').as_posix()}')
        WHERE trim(status_cd) IN ('A','I') AND ncid IN (SELECT ncid FROM s)) WHERE rn = 1""")
    out = (PROC / f"xprec_{ev}_{b}.parquet").as_posix()
    con.execute(f"""COPY (SELECT s.pid,
          (s.cc OR (s.wc AND fu.prec_f IS NOT NULL AND trim(fu.prec_f) <> trim(s.prec_b)))::INT AS xprec
        FROM s LEFT JOIN fu ON fu.ncid = s.ncid) TO '{out}' (FORMAT parquet)""")
    print(ev, b, f, con.execute(f"SELECT count(*), avg(xprec) FROM read_parquet('{out}')").fetchone(), flush=True)
    con.close()
    for s in ("", ".wal"):
        Path(str(dbf) + s).unlink(missing_ok=True)
