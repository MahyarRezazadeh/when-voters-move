"""Download NCSBE voter-registration snapshots and convert to column-reduced Parquet.

For each snapshot date: stream the zip to DATA_ROOT/raw/snapshots, decode the UTF-16LE
tab-delimited file with pyarrow in blocks, keep analysis columns, replace names with a
salted hash (diagnostics only), write Parquet to DATA_ROOT/intermediate/snapshots,
log row counts and checksums, then delete the zip (disk constraint: ~24 GB free).

Usage: py code/01_download_snapshots.py 20240514 20241105 ...
"""
import hashlib, json, secrets, sys, time, zipfile
from pathlib import Path

import pyarrow as pa
import pyarrow.compute as pc
import pyarrow.csv as pcsv
import pyarrow.parquet as pq
import requests

sys.path.insert(0, str(Path(__file__).parent))
from config import RAW, INTER, LOGS, SNAP_URL, SALT_FILE

KEEP = [
    "snapshot_dt", "county_id", "voter_reg_num", "ncid", "status_cd", "reason_cd",
    "house_num", "half_code", "street_dir", "street_name", "street_type_cd",
    "street_sufx_cd", "unit_num", "res_city_desc", "state_cd", "zip_code", "mail_state",
    "race_code", "ethnic_code", "party_cd", "sex_code", "age", "birth_place", "registr_dt",
    "precinct_abbrv", "municipality_abbrv", "cong_dist_abbrv", "NC_senate_abbrv",
    "NC_house_abbrv", "confidential_ind", "cancellation_dt", "vtd_abbrv", "load_dt",
]
NAME_COLS = ["last_name", "first_name", "midl_name"]

if not SALT_FILE.exists():
    SALT_FILE.write_text(secrets.token_hex(16))
SALT = SALT_FILE.read_text().strip().encode()

SNAPDIR_RAW = RAW / "snapshots"; SNAPDIR_RAW.mkdir(parents=True, exist_ok=True)
SNAPDIR = INTER / "snapshots"; SNAPDIR.mkdir(parents=True, exist_ok=True)
MANIFEST = LOGS / "snapshot_manifest.jsonl"


def download(d):
    url = SNAP_URL.format(d=d)
    dest = SNAPDIR_RAW / f"VR_Snapshot_{d}.zip"
    h = hashlib.sha256()
    total = int(requests.head(url, timeout=60).headers.get("content-length", -1))
    if dest.exists() and dest.stat().st_size == total:  # reuse complete earlier download
        with open(dest, "rb") as f:
            for chunk in iter(lambda: f.read(8 << 20), b""):
                h.update(chunk)
        return dest, h.hexdigest(), total
    for attempt in range(5):
        try:
            with requests.get(url, stream=True, timeout=120) as r:
                r.raise_for_status()
                total = int(r.headers.get("content-length", 0))
                with open(dest, "wb") as f:
                    for chunk in r.iter_content(8 << 20):
                        f.write(chunk); h.update(chunk)
            if dest.stat().st_size == total:
                return dest, h.hexdigest(), total
        except Exception as e:  # retry on network errors
            print(f"  download attempt {attempt} failed: {e}", flush=True)
            time.sleep(30)
        h = hashlib.sha256()
    raise RuntimeError(f"download failed {d}")


def name_hash(tbl):
    """Keyed (salted) SipHash of upper-cased last|first|middle name, vectorized via pandas."""
    import pandas as pd
    cols = [pc.utf8_upper(pc.utf8_trim_whitespace(pc.fill_null(tbl[c], ""))) for c in NAME_COLS if c in tbl.column_names]
    joined = pc.binary_join_element_wise(*cols, "|").to_pandas()
    h = pd.util.hash_pandas_object(joined, index=False, hash_key=SALT.decode()[:16])
    return pa.array(h.map("{:016x}".format).values)


def convert(zpath, d):
    final = SNAPDIR / f"snap_{d}.parquet"
    out = SNAPDIR / f"snap_{d}.parquet.tmp"
    zf = zipfile.ZipFile(zpath)
    inner = [n for n in zf.namelist() if n.lower().endswith(".txt")][0]
    stream = zf.open(inner)
    ro = pcsv.ReadOptions(encoding="utf-16", block_size=64 << 20)
    bad = {"n": 0}
    def skip(row):
        bad["n"] += 1
        return "skip"
    po = pcsv.ParseOptions(delimiter="\t", quote_char=False, invalid_row_handler=skip)
    reader = pcsv.open_csv(stream, read_options=ro, parse_options=po,
                           convert_options=pcsv.ConvertOptions(column_types={}, strings_can_be_null=False,
                                                                auto_dict_encode=False))
    header = reader.schema.names
    keep = [c for c in KEEP if c in header]
    missing = [c for c in KEEP if c not in header]
    writer = None; n = 0
    for batch in reader:
        tbl = pa.Table.from_batches([batch])
        sub = tbl.select(keep)
        sub = sub.cast(pa.schema([(c, pa.string()) for c in keep]))
        sub = sub.append_column("name_hash", name_hash(tbl))
        if writer is None:
            writer = pq.ParquetWriter(out, sub.schema, compression="zstd")
        writer.write_table(sub); n += sub.num_rows
    writer.close(); stream.close(); zf.close()
    out.replace(final)
    return final, n, bad["n"], header, missing


if __name__ == "__main__":
    for d in sys.argv[1:]:
        if (SNAPDIR / f"snap_{d}.parquet").exists():
            print(d, "exists, skip"); continue
        t0 = time.time()
        print(d, "downloading", flush=True)
        z, sha, size = download(d)
        print(d, "converting", flush=True)
        out, n, nbad, header, missing = convert(z, d)
        rec = dict(snapshot=d, url=SNAP_URL.format(d=d), zip_bytes=size, sha256=sha, rows=n,
                   skipped_rows=nbad, n_header_cols=len(header), missing_keep_cols=missing,
                   header=header, parquet=str(out), parquet_bytes=out.stat().st_size,
                   seconds=round(time.time() - t0), processed_utc=time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime()))
        with open(MANIFEST, "a") as f:
            f.write(json.dumps(rec) + "\n")
        z.unlink()
        print(d, f"done rows={n} skipped={nbad} missing={missing} {rec['seconds']}s", flush=True)
