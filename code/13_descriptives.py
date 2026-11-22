"""Baseline characteristics of the 500 m estimation sample by flood exposure (Table: descriptives)
and small diagnostics quoted in the Data section. Aggregates only -> output/tables.
Usage: py -I code/13_descriptives.py
"""
import sys
from pathlib import Path
import duckdb, pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC, OUT

con = duckdb.connect()
con.execute("PRAGMA memory_limit='1200MB'; PRAGMA threads=2")
ap = (INTER / "geocode" / "addresspoints_flood.parquet").as_posix()
rows, diag = [], []
for ev, e, b, f, ref, reff in [("florence", "fl", "20180508", "20190910", "20170101", "20180508"),
                               ("helene", "he", "20240514", "20250909", "20230101", "20240514")]:
    study = con.execute(f"SELECT DISTINCT county_id FROM read_parquet('{ap}') WHERE {e}_fl > 0").df().county_id
    inl = ",".join(f"'{s}'" for s in study)
    samp = (f"tier IS NOT NULL AND tier <> 'unmatched' AND NOT ambiguous AND {e}_fl >= 0 "
            f"AND abs({e}_dist) <= 500 AND county_id IN ({inl})")
    coh = lambda bb: f"read_parquet('{(PROC / f'cohort_{ev}_{bb}.parquet').as_posix()}')"
    snapdate = f"DATE '{b[:4]}-{b[4:6]}-{b[6:]}'"
    d = con.execute(f"""SELECT ({e}_fl > 0.5)::INT flooded, count(*) voters,
        100*avg((party_cd='DEM')::INT) dem, 100*avg((party_cd='REP')::INT) rep, 100*avg((party_cd='UNA')::INT) una,
        avg(CASE WHEN age BETWEEN 17 AND 110 THEN age END) age, 100*avg((race_code='B')::INT) black,
        100*avg((race_code='W')::INT) white, 100*avg(has_unit::INT) unit, 100*avg((sex_code='F')::INT) female,
        100*avg((status_cd='I')::INT) inactive,
        avg(date_diff('day', registr_dt::DATE, {snapdate})/365.25) FILTER (WHERE registr_dt >= TIMESTAMP '1930-01-01') yrs_reg
        FROM {coh(b)} WHERE {samp} GROUP BY 1""").df()
    r = con.execute(f"""SELECT ({e}_fl > 0.5)::INT flooded, 100*avg(cc_{reff}::INT) cc_ref
        FROM {coh(ref)} WHERE {samp} GROUP BY 1""").df()
    d = d.merge(r, on="flooded"); d.insert(0, "event", ev); rows.append(d)
    x = con.execute(f"""SELECT count(*) n, 100*avg((n_ai_base>1)::INT) multi_ai_pct,
        sum((confidential_ind='Y')::INT) confidential,
        100*avg((tier IS NOT NULL AND tier <> 'unmatched')::INT) matched_pct, 100*avg(coalesce(ambiguous,false)::INT) ambiguous_pct,
        sum((addr_{f} AND {samp})::INT) movers_sample,
        100*sum((addr_{f} AND dhouse_{f} IS NULL AND {samp})::INT)/sum((addr_{f} AND {samp})::INT) dhouse_missing_pct
        FROM {coh(b)}""").df()
    x.insert(0, "event", ev); x["n_study_counties"] = len(study); diag.append(x)
D = pd.concat(rows); G = pd.concat(diag)
D.to_csv(OUT / "tables" / "descriptives.csv", index=False); G.to_csv(OUT / "tables" / "data_diagnostics.csv", index=False)
print(D.round(2).to_string(index=False)); print(G.round(3).to_string(index=False))
