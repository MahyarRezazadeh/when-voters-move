"""Download the NCEM 'Helene Estimated Flood Inundation (Jan 2025)' polygons (public ArcGIS
FeatureServer) with pagination, save as GeoPackage in DATA_ROOT/raw/flood/helene."""
import json, sys, time
from pathlib import Path
import requests, geopandas as gpd

sys.path.insert(0, str(Path(__file__).parent))
from config import RAW, LOGS

BASE = ("https://services1.arcgis.com/YBWrN5qiESVpqi92/arcgis/rest/services/"
        "HeleneEstimatedFloodInundation_Jan2025/FeatureServer/0")
out = RAW / "flood" / "helene"; out.mkdir(parents=True, exist_ok=True)
meta = requests.get(BASE, params={"f": "json"}, timeout=60).json()
(out / "layer_metadata.json").write_text(json.dumps(meta, indent=1))
ids = requests.get(BASE + "/query", params={"where": "1=1", "returnIdsOnly": "true", "f": "json"}, timeout=120).json()["objectIds"]
ids.sort(); print("features:", len(ids))
frames = []
for i in range(0, len(ids), 200):
    chunk = ids[i:i + 200]
    for a in range(5):
        try:
            r = requests.post(BASE + "/query", data={"objectIds": ",".join(map(str, chunk)), "outFields": "*",
                                                     "outSR": 32617, "f": "geojson"}, timeout=300)
            frames.append(gpd.read_file(r.text)); break
        except Exception as e:
            print("retry", i, e); time.sleep(10)
    print(i, flush=True)
g = gpd.GeoDataFrame(__import__("pandas").concat(frames, ignore_index=True), crs=32617)
g.to_file(out / "helene_inundation.gpkg", driver="GPKG")
print(g.shape, g.columns.tolist(), g.total_bounds)
with open(LOGS / "provenance_flood.jsonl", "a") as f:
    f.write(json.dumps({"dataset": "Helene estimated inundation Jan2025", "url": BASE, "n_features": len(g),
                        "downloaded_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())}) + "\n")
