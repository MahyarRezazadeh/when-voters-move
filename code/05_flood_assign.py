"""Assign realized flood exposure to geocoded baseline addresses.

Works on UNIQUE geocoded locations (lat, lon) from geo_<baseline>.parquet, so no voter
attributes are involved. Outputs DATA_ROOT/intermediate/flood/flood_<event>_<baseline>.parquet:
 lat, lon, flooded (0/1), dist_m (signed: + outside flood, - inside, distance to nearest
 pixel/polygon of the other class, capped at CAP m), share100 (flooded share within 100 m
 radius), in_map (inside raster footprint/county study area).

Florence/Matthew: Schaffer-Smith et al. (2020) Sentinel-1 RF rasters, EPSG:2264 (US ft), 65.6 ft px.
  Distances via blockwise Euclidean distance transform with overlap margins.
Helene: NCEM estimated inundation polygons (EPSG:32617); point-in-polygon + boundary distance.
Usage: py code/05_flood_assign.py florence 20180508
"""
import sys
from pathlib import Path
import numpy as np, pandas as pd
from pyproj import Transformer

sys.path.insert(0, str(Path(__file__).parent))
from config import RAW, INTER

CAP = 3000.0  # metres
FT = 0.3048


def unique_points(baseline):
    if baseline == "addressnc":  # all public AddressNC points (no voter data involved)
        import glob
        g = pd.concat([pd.read_parquet(f, columns=["ddlat", "ddlong"]) for f in glob.glob(str(RAW / "addressnc" / "addr_*.parquet"))])
        g = g.rename(columns={"ddlat": "lat", "ddlong": "lon"}).dropna()
    else:
        g = pd.read_parquet(INTER / "geocode" / f"geo_{baseline}.parquet", columns=["lat", "lon"]).dropna()
    return g.drop_duplicates().reset_index(drop=True)


PERM_THRESHOLD = 50  # JRC GSW occurrence (% of valid observations 1984-2021 with water)


def perm_water(r, r0, r1, c0, c1):
    """Permanent-water mask on the flood-raster window: JRC Global Surface Water v1.4 occurrence
    >= PERM_THRESHOLD, nearest-neighbour resampled from EPSG:4326 to the window grid."""
    import rasterio
    from rasterio.warp import reproject, Resampling
    from rasterio.windows import Window, transform as wtransform
    dst = np.zeros((r1 - r0, c1 - c0), np.uint8)
    dt = wtransform(Window(c0, r0, c1 - c0, r1 - r0), r.transform)
    for t in ("80W_40N", "90W_40N"):
        with rasterio.open(RAW / "jrc_gsw" / f"occurrence_{t}.tif") as s:
            tmp = np.zeros_like(dst)
            reproject(rasterio.band(s, 1), tmp, src_transform=s.transform, src_crs=s.crs,
                      dst_transform=dt, dst_crs=r.crs, resampling=Resampling.nearest, src_nodata=255, dst_nodata=0)
            dst = np.maximum(dst, np.where(tmp <= 100, tmp, 0).astype(np.uint8))
    return dst >= PERM_THRESHOLD


def raster_assign(pts, tif):
    import rasterio
    from scipy import ndimage
    r = rasterio.open(tif)
    x, y = Transformer.from_crs(4326, 2264, always_xy=True).transform(pts.lon.values, pts.lat.values)
    rows, cols = rasterio.transform.rowcol(r.transform, x, y)
    rows, cols = np.asarray(rows), np.asarray(cols)
    inb = (rows >= 0) & (rows < r.height) & (cols >= 0) & (cols < r.width)
    px_m = r.res[0] * FT
    margin = int(np.ceil(CAP / px_m)) + 2
    rad = int(round(100 / px_m))
    yy, xx = np.ogrid[-rad:rad + 1, -rad:rad + 1]
    disk = (xx ** 2 + yy ** 2) <= rad ** 2
    T = 4096
    flooded = np.full(len(pts), -1, np.int8); dist = np.full(len(pts), np.nan); share = np.full(len(pts), np.nan)
    tiles = pd.DataFrame({"i": np.where(inb)[0], "tr": rows[inb] // T, "tc": cols[inb] // T})
    for (tr, tc), grp in tiles.groupby(["tr", "tc"]):
        r0, c0 = max(tr * T - margin, 0), max(tc * T - margin, 0)
        r1, c1 = min((tr + 1) * T + margin, r.height), min((tc + 1) * T + margin, r.width)
        a = r.read(1, window=((r0, r1), (c0, c1))) == 1
        a &= ~perm_water(r, r0, r1, c0, c1)  # raster includes permanent water; remove it
        if MINPX > 1:  # remove speckle: 8-connected flooded components smaller than MINPX pixels
            lab, _ = ndimage.label(a, structure=np.ones((3, 3)))
            sz = np.bincount(lab.ravel()); sz[0] = 0
            a = sz[lab] >= MINPX
            del lab
        idx = grp.i.values; rr, cc = rows[idx] - r0, cols[idx] - c0
        f = a[rr, cc]
        flooded[idx] = f
        d_out = ndimage.distance_transform_edt(~a) if a.any() else np.full(a.shape, np.inf)   # dist to nearest flooded px
        d_in = ndimage.distance_transform_edt(a) if (~a).any() else np.full(a.shape, np.inf)  # dist to nearest dry px
        d = np.where(f, -d_in[rr, cc], d_out[rr, cc]) * px_m
        dist[idx] = np.clip(d, -CAP, CAP)
        sh = ndimage.convolve(a.astype(np.float32), disk.astype(np.float32) / disk.sum(), mode="constant")
        share[idx] = sh[rr, cc]
        del a, d_out, d_in, sh
    return flooded, dist, share, inb


def helene_assign(pts):
    import geopandas as gpd
    from shapely import STRtree, points, distance
    from shapely.ops import unary_union
    poly = gpd.read_file(RAW / "flood" / "helene" / "helene_inundation.gpkg").to_crs(32617)
    u = unary_union(poly.geometry.values)
    x, y = Transformer.from_crs(4326, 32617, always_xy=True).transform(pts.lon.values, pts.lat.values)
    P = points(x, y)
    parts = list(u.geoms) if hasattr(u, "geoms") else [u]
    tree = STRtree(parts)
    inside = np.zeros(len(P), bool)
    hit = tree.query(P, predicate="within")
    inside[hit[0]] = True
    bnd = STRtree([g.boundary for g in parts])
    near = bnd.query_nearest(P, max_distance=CAP, all_matches=False, return_distance=True)
    d = np.full(len(P), CAP); d[near[0][0]] = near[1]
    dist = np.where(inside, -d, d)
    # flooded share within 100 m: area of buffer ∩ flood / buffer area (only for points within 100 m of a boundary)
    share = inside.astype(float)
    close = np.where(np.abs(dist) < 100)[0]
    buf = [P[i].buffer(100, 16) for i in close]
    ptree = STRtree(parts)
    for k, i in enumerate(close):
        cand = ptree.query(buf[k])
        share[i] = sum(buf[k].intersection(parts[j]).area for j in cand) / buf[k].area
    return inside.astype(np.int8), dist, share, np.ones(len(P), bool)


MINPX = 25  # primary (plan amendment 2026-11-14, pre-outcome); raw map: pass third arg "raw"

if __name__ == "__main__":
    ev, baseline = sys.argv[1], sys.argv[2]
    suffix = ""
    if len(sys.argv) > 3 and sys.argv[3] == "raw":
        MINPX, suffix = 1, "_raw"
    pts = unique_points(baseline)
    if ev in ("florence", "matthew"):
        tif = RAW / "flood" / ev / f"FloodExtent{ev.capitalize()}.tif"
        f, d, s, inb = raster_assign(pts, tif)
    else:
        f, d, s, inb = helene_assign(pts)
    out = pts.assign(flooded=f, dist_m=d, share100=s, in_map=inb)
    od = INTER / "flood"; od.mkdir(parents=True, exist_ok=True)
    out.to_parquet(od / f"flood_{ev}_{baseline}{suffix}.parquet", index=False)
    print(ev, baseline, len(out), "flooded:", int((out.flooded == 1).sum()),
          "within500:", int((out.dist_m.abs() <= 500).sum()))
