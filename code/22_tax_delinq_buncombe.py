"""Outcome 3 (partial): property tax delinquency, Buncombe County, one post-Helene list.

Source: "Delinquent current year real property tax" list reported to the Buncombe County Board
of Commissioners on 4 Feb 2025 (unpaid 2024 bills as of 3 Feb 2025; bills were due 6 Jan 2025,
about three months after Helene):
  https://www.buncombenc.gov/common/Commissioners/20250204/Outstanding-REI-Bills.pdf
No list from a year before the storm is online, so this is a single cross-section: no
difference-in-differences and no event study are possible.

Situs addresses are matched to AddressNC points (house number + street name + street type,
then house number + street name) and to the Helene estimation-sample addresses in Buncombe.
Only situs addresses are used; owner names are not read into the output.
Output (local disk): PROC/taxdelinq_buncombe_2025.parquet (lat, lon, bg, flooded, sfha100, delinq)
Aggregates: output/tables/taxdelinq_match_buncombe.csv
Usage: py -I code/22_tax_delinq_buncombe.py
"""
import re, sys
from pathlib import Path
import duckdb, pandas as pd, pypdf

sys.path.insert(0, str(Path(__file__).parent))
from config import RAW, INTER, PROC, OUT

pdf = RAW / "taxdelinq" / "buncombe_2025.pdf"
TYPES = set("ALY AVE BLVD BYP CIR CT CV XING DR EXPY EXT FWY HWY LN LOOP PKWY PL PLZ PT RD RUN SQ ST TER TRL TRCE TPKE WAY BND BR CRK "
            "EST GLN GRV HTS HL HOLW LNDG MTN PASS PATH PIKE PTE RDG ROW SPUR VW VLG WALK XRDS CMNS CTR MDWS PARK CSWY IS HBR SHRS "
            "CLOS GATE GRN KNL LK MNR OVAL RNCH SHL STA VLY VIS WDS CONN".split())
DIRS = {"N", "S", "E", "W", "NE", "NW", "SE", "SW"}
amt = re.compile(r"\s(\d{1,3}(?:,\d{3})*\.\d{2})\s+(\d+)\s+(.+)$")
rows, n_lines = [], 0
for page in pypdf.PdfReader(str(pdf)).pages:
    for line in (page.extract_text() or "").splitlines():
        m = amt.search(" " + line)
        if not m:
            continue
        n_lines += 1
        hn, rest = m.group(2).lstrip("0"), m.group(3).upper().split()
        if rest and rest[0] in DIRS and len(rest) > 2:
            rest = rest[1:]
        idx = [i for i, t in enumerate(rest) if t in TYPES and i > 0]
        if idx:
            i = idx[0] if len(idx) == 1 else idx[0]
            sn, st = " ".join(rest[:i]), rest[i]
        else:
            sn, st = " ".join(rest[:2]), None
        if hn and sn:
            rows.append((hn, sn, st))
bills = pd.DataFrame(rows, columns=["hn", "sn", "st"]).drop_duplicates()
print("bill lines with a situs house number:", n_lines, " distinct parsed situs:", len(bills))

con = duckdb.connect()
con.execute("PRAGMA memory_limit='1GB'; PRAGMA threads=2")
ap = f"read_parquet('{(INTER / 'geocode' / 'addresspoints_flood.parquet').as_posix()}')"
con.register("bills", bills)
# Buncombe is county_id 11
con.execute(f"""CREATE TABLE pts AS SELECT hn, sn, st, lat, lon FROM {ap} WHERE county_id = '11'""")
con.execute("""CREATE TABLE hit AS
  SELECT DISTINCT p.lat, p.lon FROM bills b JOIN pts p ON p.hn = b.hn AND p.sn = b.sn AND p.st IS NOT DISTINCT FROM b.st
  UNION
  SELECT DISTINCT p.lat, p.lon FROM bills b JOIN pts p ON p.hn = b.hn AND p.sn = b.sn
  WHERE NOT EXISTS (SELECT 1 FROM pts q WHERE q.hn = b.hn AND q.sn = b.sn AND q.st IS NOT DISTINCT FROM b.st)""")
matched = con.execute("""SELECT count(*) FROM bills b WHERE EXISTS (SELECT 1 FROM pts p WHERE p.hn = b.hn AND p.sn = b.sn)""").fetchone()[0]
print("situs matched to an address point: %d of %d (%.1f%%)" % (matched, len(bills), 100 * matched / len(bills)))
addr = f"read_parquet('{(PROC / 'addr_helene_20240514.parquet').as_posix()}')"
sf = f"read_parquet('{(INTER / 'flood' / 'sfha_helene_locs.parquet').as_posix()}')"
out = PROC / "taxdelinq_buncombe_2025.parquet"
# voter addresses are centroids of the address points under one matched key; match on rounded coordinates
con.execute(f"""COPY (
  SELECT a.lat, a.lon, a.bg, a.flooded, a.n_b, coalesce(s.sfha100, 0) AS sfha100,
         (EXISTS (SELECT 1 FROM hit h WHERE abs(h.lat - a.lat) < 1e-6 AND abs(h.lon - a.lon) < 1e-6))::INT AS delinq
  FROM {addr} a LEFT JOIN {sf} s ON s.lat = a.lat AND s.lon = a.lon
  WHERE substr(a.bg, 1, 5) = '37021') TO '{out.as_posix()}' (FORMAT parquet)""")
d = con.execute(f"""SELECT flooded, count(*) addresses, sum(delinq) delinquent, 100.0*avg(delinq) pct_delinquent,
                           100.0*avg(sfha100) pct_in_100yr FROM read_parquet('{out.as_posix()}') GROUP BY 1 ORDER BY 1""").df()
print(d.to_string())
d.insert(0, "situs_parsed", len(bills)); d.insert(1, "situs_matched", matched)
d.to_csv(OUT / "tables" / "taxdelinq_match_buncombe.csv", index=False)
