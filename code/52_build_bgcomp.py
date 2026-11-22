"""Helene: race, ethnicity and party composition of block groups at each window start (aggregate counts only; 2026-11-21).
PROC/bgcomp_<d>.parquet: bg, n, n_white, n_black, n_hisp, n_dem, n_rep, n_una (geocoded registered A/I adults).
Usage: py -I code/52_build_bgcomp.py
"""
import sys
from pathlib import Path
import duckdb
sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC
con = duckdb.connect(); con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
for d in ["20210101", "20220517", "20230101", "20240514"]:
    s = (INTER / "snapshots" / f"snap_{d}.parquet").as_posix(); g = (INTER / "geocode" / f"geo_{d}.parquet").as_posix()
    con.execute(f"""CREATE OR REPLACE TABLE sa AS SELECT county_id, voter_reg_num, trim(race_code) r, trim(ethnic_code) e, trim(party_cd) p
                    FROM read_parquet('{s}') WHERE trim(status_cd) IN ('A','I')""")
    out = PROC / f"bgcomp_{d}.parquet"
    con.execute(f"""COPY (SELECT substr(g.geoid,1,12) bg, count(*) n, sum((r='W')::INT) n_white, sum((r='B')::INT) n_black,
        sum((e='HL')::INT) n_hisp, sum((p='DEM')::INT) n_dem, sum((p='REP')::INT) n_rep, sum((p='UNA')::INT) n_una
        FROM read_parquet('{g}') g JOIN sa USING (county_id, voter_reg_num)
        WHERE g.tier <> 'unmatched' AND NOT g.ambiguous AND g.geoid IS NOT NULL GROUP BY 1) TO '{out.as_posix()}' (FORMAT parquet)""")
    print(d, con.execute(f"SELECT count(*), sum(n) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)
