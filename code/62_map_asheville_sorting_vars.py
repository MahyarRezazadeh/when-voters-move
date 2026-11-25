"""Helene: Asheville maps of the other sorting variables, same layout as the age map (47_map_asheville_age.py) (2026-11-22).

Neighbourhood = 2020 Census block group with at least 25% of its area in the Asheville city boundary.
Before (May 2024) / after (September 2025) maps from geocoded registered adults (status A/I):
  party     Democratic share of two-party registrants = n_dem / (n_dem + n_rep); 1 = all Democrats, 0 = all Republicans
  White, Black, Hispanic   share of registered adults
Block groups with fewer than MIN_N registered adults (or fewer than MIN_N two-party registrants for party) are grey.
One-period map (ACS 2019-23 5-year, does not change between snapshots): median household income, share of adults 25+
with a bachelor's degree or more, median home value.
The September 2025 composition counts are written to PROC/bgcomp_20250909.parquet (aggregate counts, same definition as
52_build_bgcomp.py).
Usage: py -I code/62_map_asheville_sorting_vars.py
"""
import sys
from pathlib import Path
import duckdb
import geopandas as gpd
import numpy as np
import pandas as pd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize, TwoSlopeNorm, LinearSegmentedColormap

sys.path.insert(0, str(Path(__file__).parent))
from config import INTER, PROC, RAW

ROOT = Path(__file__).resolve().parents[1]
TIG = RAW / "tiger"
OUT = ROOT / "output" / "figures"
MIN_N = 50
SNAPS = [("20240514", "Before: May 2024"), ("20250909", "After: September 2025")]
NOTE = ("Neighbourhood = 2020 Census block group with at least 25% of its area in the city (line = city boundary). "
        f"Grey = fewer than {MIN_N} geocoded registered adults.\nSource: NC voter registration snapshots, geocoded to "
        "AddressNC points. Registered adults only, not the full population.")

# ---- September 2025 composition (aggregate counts), if not built yet
d = "20250909"
out = PROC / f"bgcomp_{d}.parquet"
if not out.exists():
    con = duckdb.connect(); con.execute("PRAGMA memory_limit='1500MB'; PRAGMA threads=2; SET preserve_insertion_order=false;")
    con.execute(f"PRAGMA temp_directory='{(PROC / 'duck_tmp').as_posix()}'")
    s = (INTER / "snapshots" / f"snap_{d}.parquet").as_posix(); g = (INTER / "geocode" / f"geo_{d}.parquet").as_posix()
    con.execute(f"""CREATE OR REPLACE TABLE sa AS SELECT county_id, voter_reg_num, trim(race_code) r, trim(ethnic_code) e, trim(party_cd) p
                    FROM read_parquet('{s}') WHERE trim(status_cd) IN ('A','I')""")
    con.execute(f"""COPY (SELECT substr(g.geoid,1,12) bg, count(*) n, sum((r='W')::INT) n_white, sum((r='B')::INT) n_black,
        sum((e='HL')::INT) n_hisp, sum((p='DEM')::INT) n_dem, sum((p='REP')::INT) n_rep, sum((p='UNA')::INT) n_una
        FROM read_parquet('{g}') g JOIN sa USING (county_id, voter_reg_num)
        WHERE g.tier <> 'unmatched' AND NOT g.ambiguous AND g.geoid IS NOT NULL GROUP BY 1) TO '{out.as_posix()}' (FORMAT parquet)""")
    con.close()

# ---- Asheville block groups
bg = gpd.read_file(f"zip://{(TIG / 'cb_2020_37_bg_500k.zip').as_posix()}")
bg = bg[bg.GEOID.str.startswith("37021")].to_crs(32617)
place = gpd.read_file(f"zip://{(TIG / 'tl_2024_37_place.zip').as_posix()}")
ash = place[place.NAME == "Asheville"].to_crs(32617)
inter = gpd.overlay(bg[["GEOID", "geometry"]], ash[["geometry"]], how="intersection")
share = (inter.set_index("GEOID").area / bg.set_index("GEOID").area.reindex(inter.GEOID).values)
city = bg[bg.GEOID.isin(share[share >= 0.25].index)].copy()

comp = {}
for d, _ in SNAPS:
    t = duckdb.sql(f"SELECT * FROM read_parquet('{(PROC / f'bgcomp_{d}.parquet').as_posix()}') WHERE bg LIKE '37021%'").df()
    t = t.rename(columns={"bg": "GEOID"})
    t["dem2"] = np.where(t.n_dem + t.n_rep >= MIN_N, t.n_dem / (t.n_dem + t.n_rep), np.nan)
    for c, k in (("white", "n_white"), ("black", "n_black"), ("hisp", "n_hisp")):
        t[c] = np.where(t.n >= MIN_N, t[k] / t.n, np.nan)
    comp[d] = city.merge(t, on="GEOID", how="left")


def seq(name):  # one hue, light -> dark; keep the lightest step visible
    return LinearSegmentedColormap.from_list(name, plt.get_cmap(name)(np.linspace(0.12, 1.0, 256)))


def panel(ax, m, col, cmap, norm, title, fmt):
    na = m[col].isna()
    if na.any(): m[na].plot(ax=ax, color="#d9d9d9", edgecolor="white", linewidth=0.4)
    m[~na].plot(ax=ax, column=col, cmap=cmap, norm=norm, edgecolor="white", linewidth=0.4)
    ash.boundary.plot(ax=ax, color="#333333", linewidth=0.8)
    ax.set_title(title, fontsize=12, loc="left"); ax.set_axis_off()
    s = m[col].dropna()
    ax.text(0.0, -0.02, f"{len(s)} block groups; median {fmt(s.median())}", transform=ax.transAxes, fontsize=8.5, color="#555555", va="top")


def before_after(col, cmap, norm, label, title, fname, fmt):
    fig, axes = plt.subplots(1, 2, figsize=(11, 6.2), constrained_layout=True)
    for ax, (d, t) in zip(axes, SNAPS):
        panel(ax, comp[d], col, cmap, norm, t, fmt)
    cb = fig.colorbar(plt.cm.ScalarMappable(norm=norm, cmap=cmap), ax=axes, orientation="horizontal", fraction=0.04, pad=0.06, aspect=45)
    cb.set_label(label, fontsize=9.5); cb.outline.set_visible(False)
    fig.suptitle(title, fontsize=13, x=0.02, ha="left")
    fig.text(0.02, -0.02, NOTE, fontsize=8, color="#555555", va="top")
    for ext in ("png", "pdf"):
        fig.savefig(OUT / f"{fname}.{ext}", dpi=200, bbox_inches="tight")
    plt.close(fig)
    a, b = comp["20240514"].set_index("GEOID")[col], comp["20250909"].set_index("GEOID")[col]
    ch = (b - a).dropna()
    print(f"{col}: shown {int(a.notna().sum())}/{int(b.notna().sum())}, range {np.nanmin(pd.concat([a, b])):.3f}-{np.nanmax(pd.concat([a, b])):.3f}, "
          f"change mean {ch.mean():+.4f} sd {ch.std():.4f}, |change|>=0.05: {int((ch.abs() >= 0.05).sum())} of {len(ch)}, corr {a.corr(b):.3f}", flush=True)


def rng(col):
    v = pd.concat([comp[d][col] for d, _ in SNAPS]).dropna()
    return v.min(), v.max()


pct = lambda x: f"{100 * x:.0f}%"
OUT.mkdir(parents=True, exist_ok=True)
# party: diverging, neutral midpoint at 0.5 (equal Democrats and Republicans); red = Republican, blue = Democratic
before_after("dem2", plt.get_cmap("RdBu"), TwoSlopeNorm(vcenter=0.5, vmin=0, vmax=1),
             "Democratic share of two-party registrants (0 = all Republican, 0.5 = equal, 1 = all Democratic)",
             "Asheville: party of neighbourhoods before and after Hurricane Helene", "asheville_party_map_helene", lambda x: f"{x:.2f}")
for col, name, cm in (("white", "White", "Purples"), ("black", "Black", "Greens"), ("hisp", "Hispanic", "Oranges")):
    lo, hi = rng(col)
    before_after(col, seq(cm), Normalize(vmin=0, vmax=hi), f"Share of registered adults who are {name} (darker = higher)",
                 f"Asheville: {name} share of neighbourhoods before and after Hurricane Helene", f"asheville_{col}_map_helene", pct)

# ---- ACS 2019-23 (one period)
def rd(f, cols):
    x = pd.read_csv(RAW / "acs" / f, sep="|", dtype=str)
    x = x[x.GEO_ID.str.startswith("1500000US37021")]
    x["GEOID"] = x.GEO_ID.str.replace("1500000US", "")
    return x[["GEOID"] + cols].assign(**{c: pd.to_numeric(x[c], errors="coerce") for c in cols})

inc = rd("acsdt5y2023-b19013.dat", ["B19013_E001"]).rename(columns={"B19013_E001": "inc"})
hv = rd("acsdt5y2023-b25077.dat", ["B25077_E001"]).rename(columns={"B25077_E001": "hval"})
ec = ["B15003_E001"] + [f"B15003_E0{k}" for k in range(22, 26)]
ed = rd("acsdt5y2023-b15003.dat", ec)
ed["ba"] = np.where(ed.B15003_E001 >= MIN_N, ed[ec[1:]].sum(axis=1) / ed.B15003_E001, np.nan)
A = city.merge(inc, on="GEOID", how="left").merge(hv, on="GEOID", how="left").merge(ed[["GEOID", "ba"]], on="GEOID", how="left")
A.loc[A.inc <= 0, "inc"] = np.nan; A.loc[A.hval <= 0, "hval"] = np.nan
fig, axes = plt.subplots(1, 3, figsize=(15, 6.4), constrained_layout=True)
for ax, (col, cm, lab, fmt) in zip(axes, [
        ("inc", "Blues", "Median household income ($)", lambda x: f"${x / 1000:,.0f}k"),
        ("ba", "Purples", "Adults 25+ with a bachelor's degree or more", pct),
        ("hval", "Greens", "Median home value ($)", lambda x: f"${x / 1000:,.0f}k")]):
    cmap = seq(cm); s = A[col].dropna(); norm = Normalize(vmin=s.min(), vmax=s.max())
    panel(ax, A, col, cmap, norm, lab, fmt)
    cb = fig.colorbar(plt.cm.ScalarMappable(norm=norm, cmap=cmap), ax=ax, orientation="horizontal", fraction=0.05, pad=0.08, aspect=30)
    cb.outline.set_visible(False); cb.ax.tick_params(labelsize=8)
    if col != "ba": cb.ax.xaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(lambda x, _: f"{x / 1000:,.0f}k"))
    else: cb.ax.xaxis.set_major_formatter(matplotlib.ticker.FuncFormatter(lambda x, _: f"{100 * x:.0f}%"))
    print(col, "shown", len(s), "range", round(s.min(), 3), round(s.max(), 3), flush=True)
fig.suptitle("Asheville: income, education and home value of neighbourhoods (ACS 2019-23, darker = higher)", fontsize=13, x=0.02, ha="left")
fig.text(0.02, -0.02, "Neighbourhood = 2020 Census block group with at least 25% of its area in the city (line = city boundary). "
         "Grey = no ACS estimate (or fewer than 50 adults 25+ for education).\nSource: American Community Survey 2019-23 5-year "
         "estimates, tables B19013, B15003, B25077. One period only: the ACS does not measure change across the storm.",
         fontsize=8, color="#555555", va="top")
for ext in ("png", "pdf"):
    fig.savefig(OUT / f"asheville_acs_map.{ext}", dpi=200, bbox_inches="tight")
