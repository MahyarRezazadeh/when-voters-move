"""Helene: assessed value of the home at each baseline address of an adult who moved (2026-11-21).

Source: NC OneMap statewide parcels (NC1Map_Parcels FeatureServer layer 1), public. Only value and structure fields
are requested (no owner names). Parcels are downloaded county by county (paged, 2,000 per request) for the Helene
study counties, then each baseline address of an adult who changed address (address within 500 m of the flood
boundary, four windows) is matched to the parcel containing it (point-in-polygon). Units per parcel = number of
AddressNC address points inside the parcel (value of apartment/condo parcels is divided by units).
Output (local disk only): PROC/parcel_value_helene.parquet  lat, lon, pid_parcel, parval, improvval, landval,
gisacres, structyear, parusecode, parusedesc, units.   Raw county files: C:/voting_data/raw/parcels/county_<fips>.parquet
Usage: py -I code/51_parcels.py
"""
import json, sys, time, urllib.parse, urllib.request
from pathlib import Path
import duckdb
import geopandas as gpd
import pandas as pd
from shapely.geometry import shape

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC

URL = "https://services.nconemap.gov/secure/rest/services/NC1Map_Parcels/FeatureServer/1/query"
FIELDS = "objectid,parval,improvval,landval,gisacres,structyear,parusecode,parusedesc,structno"
RAW = Path("C:/voting_data/raw/parcels"); RAW.mkdir(parents=True, exist_ok=True)
PAGE = 2000

con = duckdb.connect(); con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2")
pts = con.execute(" UNION ".join(
    f"""SELECT DISTINCT round(lat, 6) AS lat, round(lon, 6) AS lon, county_id
        FROM read_parquet('{(PROC / f'cohort_helene_{b}.parquet').as_posix()}')
        WHERE tier IS NOT NULL AND tier <> 'unmatched' AND NOT ambiguous AND he_fl >= 0 AND abs(he_dist) <= 500 AND addr_{f}"""
    for b, f in [("20210101", "20220517"), ("20220517", "20230912"), ("20230101", "20240514"), ("20240514", "20250909")])).df()
pts = pts.drop_duplicates(["lat", "lon"])
pts["fips"] = pts.county_id.astype(int).map(lambda c: f"{2 * c - 1:03d}")
print("unique mover baseline addresses:", len(pts), "counties:", pts.fips.nunique(), flush=True)


def get(params):
    q = URL + "?" + urllib.parse.urlencode(params)
    for k in range(6):
        try:
            with urllib.request.urlopen(urllib.request.Request(q, headers={"User-Agent": "research-client"}), timeout=180) as r:
                return json.load(r)
        except Exception as e:
            print("retry", k, e, flush=True); time.sleep(5 * (k + 1))
    raise RuntimeError("parcel query failed")


out_rows = []
for fp in sorted(pts.fips.unique()):
    cf = RAW / f"county_{fp}.parquet"
    if not cf.exists():
        where = f"stcntyfips='37{fp}'"
        n = get({"where": where, "returnCountOnly": "true", "f": "json"})["count"]
        pd_dir = RAW / f"pages_{fp}"; pd_dir.mkdir(exist_ok=True)  # one file per page keeps memory low
        for off in range(0, n, PAGE):
            pf = pd_dir / f"p_{off:07d}.parquet"
            if pf.exists():
                continue
            js = get({"where": where, "outFields": FIELDS, "returnGeometry": "true", "outSR": 4326, "geometryPrecision": 6,
                      "orderByFields": "objectid", "resultOffset": off, "resultRecordCount": PAGE, "f": "geojson"})
            feats = [{**f["properties"], "geometry": shape(f["geometry"])} for f in js.get("features", []) if f.get("geometry")]
            gpd.GeoDataFrame(feats, crs=4326).to_parquet(pf); del js, feats
        # keep only parcels that contain a mover address (page by page, so a large county never sits in memory)
        p0 = pts[pts.fips == fp]
        g0 = gpd.GeoDataFrame(p0, geometry=gpd.points_from_xy(p0.lon, p0.lat), crs=4326)
        keep = []
        for x in sorted(pd_dir.glob("p_*.parquet")):
            pg = gpd.read_parquet(x)
            ids = gpd.sjoin(g0, pg[["objectid", "geometry"]], how="inner", predicate="within").objectid.unique()
            keep.append(pg[pg.objectid.isin(ids)]); del pg
        gpd.GeoDataFrame(pd.concat(keep), crs=4326).to_parquet(cf)
        for x in pd_dir.glob("p_*.parquet"):
            x.unlink()
        pd_dir.rmdir()
        print(f"county {fp}: {n} parcels downloaded", time.strftime("%H:%M:%S"), flush=True)
    mf = RAW / f"match_{fp}.parquet"
    if mf.exists():
        out_rows.append(pd.read_parquet(mf)); continue
    par = gpd.read_parquet(cf).to_crs(32617)
    p = pts[pts.fips == fp]
    g = gpd.GeoDataFrame(p, geometry=gpd.points_from_xy(p.lon, p.lat), crs=4326).to_crs(32617)
    j = gpd.sjoin(g, par, how="left", predicate="within").drop_duplicates(["lat", "lon"])
    af = Path(f"C:/voting_data/raw/addressnc/addr_{fp}.parquet")
    ap = pd.read_parquet(af, columns=["ddlat", "ddlong"]).dropna()
    ap = gpd.GeoDataFrame(geometry=gpd.points_from_xy(ap.ddlong, ap.ddlat), crs=4326).to_crs(32617)
    hit = par[par.objectid.isin(j.objectid.dropna())][["objectid", "geometry"]]
    u = gpd.sjoin(ap, hit, how="inner", predicate="within").groupby("objectid").size().rename("units")
    j = j.merge(u, left_on="objectid", right_index=True, how="left")
    r = pd.DataFrame(j[["lat", "lon", "objectid", "parval", "improvval", "landval", "gisacres", "structyear", "parusecode", "parusedesc", "units"]])
    r.to_parquet(mf); out_rows.append(r)
    print(f"county {fp}: {len(p)} addresses, matched {int(j.objectid.notna().sum())}", flush=True)
    del par, g, j, ap, hit

res = pd.concat(out_rows).rename(columns={"objectid": "pid_parcel"})
res["units"] = res["units"].fillna(1).clip(lower=1)
res.to_parquet(PROC / "parcel_value_helene.parquet")
m = res.pid_parcel.notna()
print("addresses matched to a parcel:", int(m.sum()), "of", len(res), "; with value > 0:", int((res.parval > 0).sum()), flush=True)
print("residential use share among matched:", round(res.loc[m, "parusedesc"].fillna("").str.contains("RES", case=False).mean(), 3), flush=True)
print("DONE", flush=True)
