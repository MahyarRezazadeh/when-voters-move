"""Download FEMA flood hazard zones (100-year SFHA and 0.2% zones) for the Helene study area.

Source: Esri Living Atlas "USA Flood Hazard Reduced Set" (public hosted copy of FEMA's National
Flood Hazard Layer). The NC Floodplain Mapping Program server (spartagis.ncem.org) and FEMA's own
server (hazards.fema.gov) both refused connections from this machine on 2026-11-17.
Only a bounding box is sent to the service; no voter or address data leave the machine.
Output: DATA_ROOT/raw/flood/sfha/sfha_helene_area.gpkg (EPSG:32617).
"""
import json, sys, time
from pathlib import Path
import requests, geopandas as gpd, pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from config import RAW, LOGS

BASE = ("https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/"
        "USA_Flood_Hazard_Reduced_Set_gdb/FeatureServer/0")
BBOX = "-84.05,34.95,-80.70,36.62"   # lon/lat envelope of the Helene study counties, padded
out = RAW / "flood" / "sfha"; out.mkdir(parents=True, exist_ok=True)
meta = requests.get(BASE, params={"f": "json"}, timeout=60).json()
(out / "layer_metadata.json").write_text(json.dumps(meta, indent=1))
# Only the zones the analysis uses: the SFHA (1% annual chance) and the 0.2% zone. NC's "1 Percent
# Future Conditions" polygons (about 23,000 in the box) are not downloaded.
q = {"where": "DFIRM_ID LIKE '37%' AND (SFHA_TF = 'T' OR ZONE_SUBTY LIKE '0.2 Percent%')", "geometry": BBOX, "geometryType": "esriGeometryEnvelope", "inSR": 4326,
     "spatialRel": "esriSpatialRelIntersects"}
ids = requests.post(BASE + "/query", data={**q, "returnIdsOnly": "true", "f": "json"}, timeout=300).json()["objectIds"]
ids.sort(); print("features:", len(ids), flush=True)
STEP = 100


def fetch(i):
    chunk = ids[i:i + STEP]
    for a in range(6):
        try:
            r = requests.post(BASE + "/query", data={
                "objectIds": ",".join(map(str, chunk)), "outSR": 32617, "f": "geojson",
                "outFields": "OBJECTID,DFIRM_ID,FLD_ZONE,ZONE_SUBTY,SFHA_TF,SOURCE_CIT"}, timeout=600)
            g = gpd.read_file(r.text)
            if len(g) != len(chunk):
                raise RuntimeError(f"got {len(g)} of {len(chunk)}")
            return i, g
        except Exception as e:
            print("retry", i, a, str(e)[:120], flush=True); time.sleep(10)
    return i, None


from concurrent.futures import ThreadPoolExecutor
frames, failed = [], []
with ThreadPoolExecutor(4) as ex:   # four requests at a time
    for n, (i, g) in enumerate(ex.map(fetch, range(0, len(ids), STEP))):
        (frames.append(g) if g is not None else failed.append(i))
        if n % 20 == 0:
            print(i, time.strftime("%H:%M:%S"), flush=True)
g = gpd.GeoDataFrame(pd.concat(frames, ignore_index=True), crs=32617)
g.to_file(out / "sfha_helene_area.gpkg", driver="GPKG")
print(g.shape, "failed chunks:", failed)
print(g.groupby(["SFHA_TF", "FLD_ZONE", "ZONE_SUBTY"], dropna=False).size().to_string())
with open(LOGS / "provenance_flood.jsonl", "a") as f:
    f.write(json.dumps({"dataset": "FEMA NFHL flood hazard zones (Esri Living Atlas USA_Flood_Hazard_Reduced_Set)",
                        "url": BASE, "bbox": BBOX, "n_features": len(g), "failed_chunks": failed,
                        "data_last_edit_ms": meta.get("editingInfo", {}).get("dataLastEditDate"),
                        "downloaded_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())}) + "\n")
