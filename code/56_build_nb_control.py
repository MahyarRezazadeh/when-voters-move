"""Helene: comparison counties outside the storm area, and each mover's origin and destination block group there (2026-11-22).

Comparison counties = North Carolina counties with no FEMA major disaster declaration for Helene (DR-4827), Potential
Tropical Cyclone Eight (DR-4837) or Chantal (DR-4889), not adjacent to any DR-4827 county (housing markets that took in
displaced households), and not in the Chantal state disaster list (Granville). Sources (local copies):
  C:/voting_data/raw/fema/nc_declarations_2024_2026.json  (OpenFEMA DisasterDeclarationsSummaries v2, NC, FY2024+)
  C:/voting_data/raw/fema/county_adjacency2024.txt         (Census county adjacency file 2024)
Output (local disk only): PROC/nbc_helene_<b>.parquet  pid, bg_b, bg_f for adults in comparison counties who changed
address in the window (geocoded, matched, not ambiguous); PROC/control_counties.json.
Usage: py -I code/56_build_nb_control.py
"""
import json, sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC, RAW

WIN = [("20210101", "20220517"), ("20220517", "20230912"), ("20230101", "20240514"), ("20240514", "20250909")]
d = json.load(open(RAW / "fema" / "nc_declarations_2024_2026.json", encoding="utf-8"))["DisasterDeclarationsSummaries"]
dis = lambda n: {r["fipsCountyCode"] for r in d if r["disasterNumber"] == n and r["fipsCountyCode"] != "000"}
hel = dis(4827); other = dis(4837) | dis(4889) | {"077"}  # 077 Granville: Chantal state Type I disaster
adj = {}
for line in open(RAW / "fema" / "county_adjacency2024.txt", encoding="latin-1").read().splitlines()[1:]:
    p = line.split("|")
    if p[1].startswith("37") and p[3].startswith("37"):
        adj.setdefault(p[1][2:], set()).add(p[3][2:])
nbr = set().union(*[adj[c] for c in hel]) - hel
ctrl = sorted({f"{2 * i - 1:03d}" for i in range(1, 101)} - hel - other - nbr)
ids = [str((int(c) + 1) // 2) for c in ctrl]
json.dump({"fips": ctrl, "county_id": ids}, open(PROC / "control_counties.json", "w"))
print("comparison counties:", len(ctrl), flush=True)

geo = lambda x: f"read_parquet('{(INTER / 'geocode' / f'geo_{x}.parquet').as_posix()}')"
(PROC / "duck_tmp").mkdir(exist_ok=True)
dbf = PROC / "duck_tmp" / "nbc_helene.duckdb"
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
con = duckdb.connect(str(dbf))
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
con.execute(f"CREATE TABLE pid AS SELECT * FROM read_parquet('{(PROC / 'pid_map.parquet').as_posix()}')")
inl = ",".join(f"'{i}'" for i in ids)
for b, f in WIN:
    coh = f"read_parquet('{(PROC / f'cohort_helene_{b}.parquet').as_posix()}')"
    con.execute(f"""CREATE OR REPLACE TABLE s AS
      SELECT c.pid, p.ncid, substr(c.geoid, 1, 12) AS bg_b, c.dcounty_{f} AS dcounty
      FROM {coh} c JOIN pid p USING (pid)
      WHERE c.county_id IN ({inl}) AND c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.addr_{f}""")
    con.execute(f"""CREATE OR REPLACE TABLE gd AS
      SELECT ncid, county_id, any_value(substr(geoid, 1, 12)) AS bg FROM {geo(f)}
      WHERE ncid IN (SELECT ncid FROM s) AND tier <> 'unmatched' AND NOT ambiguous GROUP BY 1, 2""")
    out = PROC / f"nbc_helene_{b}.parquet"
    con.execute(f"""COPY (SELECT s.pid, s.bg_b, gd.bg AS bg_f FROM s LEFT JOIN gd ON gd.ncid = s.ncid AND gd.county_id = s.dcounty)
                    TO '{out.as_posix()}' (FORMAT parquet)""")
    print("nbc", b, f, con.execute(f"SELECT count(*), round(avg((bg_f IS NOT NULL)::INT),3), round(avg((bg_f <> bg_b)::INT),3) FROM read_parquet('{out.as_posix()}')").fetchone(), flush=True)
con.close()
for s in ("", ".wal"):
    Path(str(dbf) + s).unlink(missing_ok=True)
