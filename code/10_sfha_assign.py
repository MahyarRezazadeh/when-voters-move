"""Assign FEMA floodplain status to the matched locations of Helene-cohort voters.

Works on UNIQUE matched locations (lat, lon) within 2,100 m of the Helene flood boundary, so no
voter attribute is involved. Output DATA_ROOT/intermediate/flood/sfha_helene_locs.parquet:
 lat, lon, sfha100 (inside a Special Flood Hazard Area, SFHA_TF = 'T'), floodway (inside a
 regulatory floodway), sfha500 (inside a 0.2%-annual-chance zone and not in the SFHA).
Usage: py -I code/10_sfha_assign.py
"""
import sys
from pathlib import Path
import numpy as np, pandas as pd, duckdb, geopandas as gpd
from pyproj import Transformer
from shapely import STRtree, points

sys.path.insert(0, str(Path(__file__).parent))
from config import RAW, INTER, PROC

BASES = ["20210101", "20220517", "20230101", "20240514"]
con = duckdb.connect()
con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2")
q = " UNION ".join(
    f"SELECT lat, lon FROM read_parquet('{(PROC / f'cohort_helene_{b}.parquet').as_posix()}') "
    "WHERE tier IS NOT NULL AND tier <> 'unmatched' AND NOT ambiguous AND he_fl >= 0 AND abs(he_dist) <= 2100"
    for b in BASES)
pts = con.execute(q).df()
print("unique locations:", len(pts))

g = gpd.read_file(RAW / "flood" / "sfha" / "sfha_helene_area.gpkg")
g = g[g.geometry.notna() & ~g.geometry.is_empty].copy()
g["geometry"] = g.geometry.make_valid()
sub = g.ZONE_SUBTY.fillna("").str.upper()
is100 = (g.SFHA_TF == "T").values
isfw = is100 & sub.str.contains("FLOODWAY").values
is500 = (~is100) & sub.str.contains("0.2 PERCENT", regex=False).values
print("polygons: 100-year", int(is100.sum()), "floodway", int(isfw.sum()), "500-year", int(is500.sum()))

x, y = Transformer.from_crs(4326, 32617, always_xy=True).transform(pts.lon.values, pts.lat.values)
P = points(x, y)


def inside(mask):
    geoms = g.geometry.values[mask]
    hit = STRtree(geoms).query(P, predicate="within")
    out = np.zeros(len(P), np.int8); out[np.unique(hit[0])] = 1
    return out


pts["sfha100"] = inside(is100)
pts["floodway"] = inside(isfw)
pts["sfha500"] = inside(is500) * (1 - pts["sfha100"])
pts.to_parquet(INTER / "flood" / "sfha_helene_locs.parquet", index=False)
print(pts[["sfha100", "floodway", "sfha500"]].sum().to_string())
# agreement with the Helene layer at AddressNC-point level is checked in 07_estimate.R (fp crosstab)
