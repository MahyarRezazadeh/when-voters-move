"""Local geocoding of voter residential addresses to AddressNC address points (DuckDB),
carrying the address points' flood exposure (computed by 05_flood_assign.py on AddressNC).

Input : DATA_ROOT/intermediate/snapshots/snap_<d>.parquet (baseline snapshot)
        DATA_ROOT/raw/addressnc/addr_*.parquet
        DATA_ROOT/intermediate/flood/flood_<event>_addressnc[_raw].parquet
Output: DATA_ROOT/intermediate/geocode/geo_<d>.parquet, one row per A/I record
        (county_id, voter_reg_num, ncid): lat, lon, block geoid, match tier, n candidate
        points, spread (m), ambiguous flag, and per event e in {fl, mt, he}:
        <e>_fl (share of candidate points flooded), <e>_dist (mean signed distance, m),
        <e>_sh (mean flooded share within 100 m); raw-map versions <e>r_fl, <e>r_dist.
Nothing is sent outside the machine.

Normalization: upper-case, collapse spaces, map full street types / directions to USPS
abbreviations on the AddressNC side (voter file already uses abbreviations), strip
leading zeros from house numbers, 5-digit ZIP.
Match tiers (first hit wins; each tier only for still-unmatched records):
 T1 county + house number + predir + street name + street type + ZIP5
 T2 county + house number + street name + street type + ZIP5
 T3 county + house number + street name + ZIP5
 T4 county + house number + street name + street type (any ZIP in county)
Keys mapping to several points (multi-unit buildings, duplicate points) use the centroid;
spread = max distance of a candidate from the centroid; spread > 150 m => ambiguous
(excluded from treatment assignment in the primary sample).
Usage: py code/04_geocode.py 20240514 [more baselines]
"""
import sys, time, json
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import RAW, INTER, LOGS

TYPES = {"ALLEY": "ALY", "AVENUE": "AVE", "BOULEVARD": "BLVD", "BYPASS": "BYP", "CIRCLE": "CIR", "COURT": "CT",
         "COVE": "CV", "CROSSING": "XING", "DRIVE": "DR", "EXPRESSWAY": "EXPY", "EXTENSION": "EXT", "FREEWAY": "FWY",
         "HIGHWAY": "HWY", "LANE": "LN", "LOOP": "LOOP", "PARKWAY": "PKWY", "PLACE": "PL", "PLAZA": "PLZ", "POINT": "PT",
         "ROAD": "RD", "RUN": "RUN", "SQUARE": "SQ", "STREET": "ST", "TERRACE": "TER", "TRAIL": "TRL", "TRACE": "TRCE",
         "TURNPIKE": "TPKE", "WAY": "WAY", "BEND": "BND", "BRANCH": "BR", "CREEK": "CRK", "ESTATES": "EST",
         "GLEN": "GLN", "GROVE": "GRV", "HEIGHTS": "HTS", "HILL": "HL", "HOLLOW": "HOLW", "LANDING": "LNDG",
         "MOUNTAIN": "MTN", "PASS": "PASS", "PATH": "PATH", "PIKE": "PIKE", "POINTE": "PTE", "RIDGE": "RDG",
         "ROW": "ROW", "SPUR": "SPUR", "VIEW": "VW", "VILLAGE": "VLG", "WALK": "WALK", "CROSSROADS": "XRDS",
         "COMMONS": "CMNS", "CENTER": "CTR", "MEADOWS": "MDWS", "PARK": "PARK", "CAUSEWAY": "CSWY", "ISLAND": "IS",
         "HARBOR": "HBR", "SHORES": "SHRS", "CLOSE": "CLOS", "GATE": "GATE", "GREEN": "GRN", "KNOLL": "KNL",
         "LAKE": "LK", "MANOR": "MNR", "OVAL": "OVAL", "RANCH": "RNCH", "SHOAL": "SHL", "STATION": "STA",
         "TRAFFICWAY": "TRFY", "VALLEY": "VLY", "VISTA": "VIS", "WOODS": "WDS", "CUTOFF": "CUTOFF", "CONNECTOR": "CONN"}
DIRS = {"NORTH": "N", "SOUTH": "S", "EAST": "E", "WEST": "W", "NORTHEAST": "NE", "NORTHWEST": "NW",
        "SOUTHEAST": "SE", "SOUTHWEST": "SW"}
EVENTS = [("fl", "florence", ""), ("mt", "matthew", ""), ("he", "helene", ""),
          ("flr", "florence", "_raw"), ("mtr", "matthew", "_raw")]


def case_map(col, m):
    whens = " ".join(f"WHEN '{k}' THEN '{v}'" for k, v in m.items())
    return f"(CASE upper(trim({col})) {whens} ELSE nullif(upper(trim({col})),'') END)"


def build_points(con):
    """AddressNC points + flood attributes, cached as Parquet."""
    cache = INTER / "geocode" / "addresspoints_flood.parquet"
    if cache.exists():
        return cache
    fl = INTER / "flood"
    joins, cols = [], []
    for e, ev, sfx in EVENTS:
        f = fl / f"flood_{ev}_addressnc{sfx}.parquet"
        if not f.exists():
            raise FileNotFoundError(f)
        joins.append(f"LEFT JOIN read_parquet('{f.as_posix()}') {e} ON {e}.lat = a.ddlat AND {e}.lon = a.ddlong")
        cols.append(f"{e}.flooded AS {e}_fl, {e}.dist_m AS {e}_dist, {e}.share100 AS {e}_sh")
    con.execute(f"""
      COPY (SELECT CAST((CAST(a.countyfips AS INT)+1)//2 AS VARCHAR) AS county_id,
             ltrim(trim(a.add_number),'0') AS hn,
             {case_map('a.st_predir', DIRS)} AS pd,
             regexp_replace(upper(trim(a.st_name)),'\\s+',' ','g') AS sn,
             {case_map('a.st_postyp', TYPES)} AS st,
             left(trim(a.post_code),5) AS zip5,
             a.ddlat AS lat, a.ddlong AS lon, a.geoid, {', '.join(cols)}
      FROM read_parquet('{(RAW/'addressnc').as_posix()}/addr_*.parquet') a {' '.join(joins)}
      WHERE a.add_number IS NOT NULL AND a.st_name IS NOT NULL AND a.ddlat IS NOT NULL
        AND CAST(a.countyfips AS INT) BETWEEN 1 AND 199) TO '{cache.as_posix()}' (FORMAT parquet)""")
    return cache


def run(d):
    t0 = time.time()
    gdir = INTER / "geocode"; gdir.mkdir(parents=True, exist_ok=True)
    con = duckdb.connect()
    con.execute("PRAGMA memory_limit='2GB'; PRAGMA threads=3; SET enable_progress_bar=false;")
    con.execute(f"PRAGMA temp_directory='{(INTER / 'duck_tmp').as_posix()}'")
    ap = build_points(con).as_posix()
    flcols = [f"{e}_{v}" for e, _, _ in EVENTS for v in ("fl", "dist", "sh")]
    con.execute(f"""
      CREATE TEMP TABLE todo AS
      SELECT county_id, voter_reg_num, ncid,
             ltrim(trim(house_num),'0') AS hn,
             nullif(upper(trim(street_dir)),'') AS pd,
             regexp_replace(upper(trim(street_name)),'\\s+',' ','g') AS sn,
             nullif(upper(trim(street_type_cd)),'') AS st,
             left(trim(zip_code),5) AS zip5
      FROM read_parquet('{(INTER/'snapshots'/f'snap_{d}.parquet').as_posix()}')
      WHERE trim(status_cd) IN ('A','I')""")
    n_v = con.execute("SELECT count(*) FROM todo").fetchone()[0]
    tiers = [("T1", ["county_id", "hn", "pd", "sn", "st", "zip5"]),
             ("T2", ["county_id", "hn", "sn", "st", "zip5"]),
             ("T3", ["county_id", "hn", "sn", "zip5"]),
             ("T4", ["county_id", "hn", "sn", "st"])]
    agg = ", ".join(f"avg({c}) AS {c}" for c in flcols)
    first = True
    for name, keys in tiers:
        k = ", ".join(keys)
        on = " AND ".join(f"t.{c} IS NOT DISTINCT FROM g.{c}" if c in ("pd", "st") else f"t.{c} = g.{c}" for c in keys)
        con.execute(f"""
          CREATE OR REPLACE TEMP TABLE g AS
          SELECT {k}, avg(lat) AS lat, avg(lon) AS lon, mode(geoid) AS geoid, count(*) AS npts,
                 max(111320*sqrt(power(lat-alat,2)+power((lon-alon)*cos(radians(alat)),2))) AS spread_m, {agg}
          FROM (SELECT *, avg(lat) OVER (PARTITION BY {k}) AS alat, avg(lon) OVER (PARTITION BY {k}) AS alon
                FROM read_parquet('{ap}') WHERE county_id IN (SELECT DISTINCT county_id FROM todo))
          GROUP BY {k}""")
        sel = f"""SELECT t.county_id, t.voter_reg_num, t.ncid, '{name}' AS tier, g.lat, g.lon, g.geoid, g.npts,
                         g.spread_m, {', '.join('g.' + c for c in flcols)}
                  FROM todo t JOIN g ON {on}"""
        con.execute(f"{'CREATE TEMP TABLE res AS' if first else 'INSERT INTO res'} {sel}")
        first = False
        con.execute("""CREATE OR REPLACE TEMP TABLE todo AS SELECT t.* FROM todo t
                       ANTI JOIN res r ON r.county_id = t.county_id AND r.voter_reg_num = t.voter_reg_num""")
        print(name, "remaining", con.execute("SELECT count(*) FROM todo").fetchone()[0], flush=True)
    out = gdir / f"geo_{d}.parquet"
    con.execute(f"""COPY (SELECT *, (spread_m > 150) AS ambiguous FROM res
                         UNION ALL BY NAME
                         SELECT county_id, voter_reg_num, ncid, 'unmatched' AS tier FROM todo)
                    TO '{out.as_posix()}' (FORMAT parquet)""")
    stats = con.execute(f"""SELECT tier, count(*) n, avg(CASE WHEN ambiguous THEN 1 ELSE 0 END) amb
                            FROM read_parquet('{out.as_posix()}') GROUP BY 1 ORDER BY 1""").fetchall()
    rec = {"snapshot": d, "n_active_inactive_records": n_v, "tiers": stats, "seconds": round(time.time() - t0)}
    print(json.dumps(rec, indent=1))
    with open(LOGS / "geocode_log.jsonl", "a") as f:
        f.write(json.dumps(rec) + "\n")


if __name__ == "__main__":
    for d in sys.argv[1:]:
        run(d)
