"""County two-party presidential vote share from public NCSBE precinct results (2016, 2020).

Used by `07_estimate.R <event> margin` to classify counties as close or safe.
Input : RAW/elections/results_pct_<date>.txt  (https://s3.amazonaws.com/dl.ncsbe.gov/ENRS/...)
Output: output/tables/county_pres_votes.csv  (county_id, county, rep2_2016, rep2_2020), public aggregates.
Usage : py -I code/16_county_votes.py
"""
import sys
from pathlib import Path
import duckdb

sys.path.insert(0, str(Path(__file__).parent))
from config import RAW, INTER, OUT

con = duckdb.connect()
out = None
for d, y in (("20161108", 2016), ("20201103", 2020)):
    f = (RAW / "elections" / f"results_pct_{d}.txt").as_posix()
    v = con.execute(f"""SELECT upper(trim("County")) AS county,
          sum(CASE WHEN "Choice Party"='REP' THEN "Total Votes" ELSE 0 END)::DOUBLE
          / sum(CASE WHEN "Choice Party" IN ('REP','DEM') THEN "Total Votes" ELSE 0 END) AS rep2_{y}
        FROM read_csv('{f}', delim='\t', header=true, all_varchar=false, ignore_errors=true)
        WHERE "Contest Name" = 'US PRESIDENT' GROUP BY 1""").df()
    out = v if out is None else out.merge(v, on="county", how="outer")
# NCSBE county_id is the rank of the county name in alphabetical order (1 = Alamance, 60 = Mecklenburg, 92 = Wake)
out = out.sort_values("county").reset_index(drop=True)
out.insert(0, "county_id", range(1, len(out) + 1))
assert len(out) == 100 and out.county[59] == "MECKLENBURG" and out.county[91] == "WAKE"
print(out.describe().to_string())
print("counties:", len(out), "missing:", out.isna().any(axis=1).sum())
out.sort_values("county").to_csv(OUT / "tables" / "county_pres_votes.csv", index=False)
