"""Download AddressNC statewide address points (NC 911 Board / NC CGIA, public FeatureServer)
for LOCAL geocoding of voter residential addresses. Only outbound queries for public
address points are made; no voter information is sent anywhere.
Pagination: objectid ranges per county, 4 parallel workers. Output: Parquet per county."""
import json, sys, time
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path
import pandas as pd, requests

sys.path.insert(0, str(Path(__file__).parent))
from config import RAW, LOGS

URL = "https://services.nconemap.gov/secure/rest/services/AddressNC/NC1Map_Addresses/FeatureServer/0/query"
FIELDS = ("objectid,county,countyfips,add_number,addnum_suf,st_predir,st_pretyp,st_name,st_postyp,st_posdir,"
          "post_code,post_comm,inc_muni,unit,building,ddlat,ddlong,block,tract,geoid,elev,placement,source,dateupdate,effective")
OUT = RAW / "addressnc"; OUT.mkdir(parents=True, exist_ok=True)
S = requests.Session()


def q(params):
    for a in range(8):
        try:
            r = S.get(URL, params={**params, "f": "json"}, timeout=180)
            j = r.json()
            if "error" in j: raise RuntimeError(j["error"])
            return j
        except Exception as e:
            time.sleep(5 * (a + 1))
    raise RuntimeError(f"failed {params}")


def county(fips):
    out = OUT / f"addr_{fips}.parquet"
    if out.exists(): return fips, -1
    last, rows = -1, []
    while True:
        j = q({"where": f"countyfips='{fips}' AND objectid>{last}", "outFields": FIELDS, "orderByFields": "objectid",
               "returnGeometry": "false", "resultRecordCount": 2000})
        feats = [f["attributes"] for f in j.get("features", [])]
        if not feats: break
        rows += feats; last = feats[-1]["objectid"]
        if len(feats) < 2000 and not j.get("exceededTransferLimit"): break
    pd.DataFrame(rows).to_parquet(out, index=False)
    return fips, len(rows)


if __name__ == "__main__":
    fips = q({"where": "1=1", "returnDistinctValues": "true", "outFields": "countyfips", "returnGeometry": "false"})
    fl = sorted({f["attributes"]["countyfips"] for f in fips["features"] if f["attributes"]["countyfips"]})
    print(len(fl), "counties", flush=True)
    with ThreadPoolExecutor(4) as ex:
        for f, n in ex.map(county, fl):
            print(f, n, flush=True)
    with open(LOGS / "provenance_addressnc.json", "w") as f:
        json.dump({"url": URL, "fields": FIELDS, "counties": fl,
                   "downloaded_utc": time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())}, f, indent=1)
