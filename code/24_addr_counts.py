"""Raw counts of registered voters who left and arrived at sample addresses, by cohort and flood
status (exploratory, 2026-11-18). Reads PROC/addr_<event>_<b>.parquet from 20_build_addr_hh.py.
left = at the address at baseline and not at follow-up; arrived = at the address at follow-up and
not at baseline. Output: output/tables/new_addr_counts.csv
"""
import sys
from pathlib import Path
import duckdb, pandas as pd

sys.path.insert(0, str(Path(__file__).parent))
from config import PROC, OUT

rows = []
for f in sorted(PROC.glob("addr_*.parquet")):
    ev, b = f.stem.split("_")[1:3]
    d = duckdb.sql(f"""SELECT flooded, count(*) addresses, sum(n_b) voters_start, sum(n_b - (n_f - n_arr)) left_addr,
                              sum(n_arr) arrived, sum(n_f) voters_end, sum((n_f = 0)::INT) emptied
                       FROM read_parquet('{f.as_posix()}') GROUP BY 1 ORDER BY 1""").df()
    d.insert(0, "cohort", b); d.insert(0, "event", ev); rows.append(d)
o = pd.concat(rows)
o["left_pct_of_voters"] = 100 * o.left_addr / o.voters_start
o["arrived_per_100_voters"] = 100 * o.arrived / o.voters_start
o["emptied_pct_of_addresses"] = 100 * o.emptied / o.addresses
pd.set_option("display.width", 250)
print(o.round(1).to_string(index=False))
o.to_csv(OUT / "tables" / "new_addr_counts.csv", index=False)
