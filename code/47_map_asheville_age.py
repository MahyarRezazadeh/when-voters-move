"""Helene: map of neighbourhood age in Asheville before (May 2024) and after (Sept 2025) the storm (2026-11-21).

Neighbourhood = 2020 Census block group. Value = mean age of geocoded registered adults (status A/I) in the block
group, from the aggregate counts PROC/bgage_<d>.parquet (built by 45_build_nbage.py). Block groups that intersect
the Asheville city boundary (TIGER 2024 place) are shown. Block groups with fewer than MIN_N adults are suppressed
(grey). Same colour scale in both panels; darker = younger.
Boundaries: C:/voting_data/raw/tiger (Census cartographic boundary file cb_2020_37_bg_500k, tl_2024_37_place).
Usage: py -I code/47_map_asheville_age.py
"""
import sys
from pathlib import Path
import duckdb
import geopandas as gpd
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
from matplotlib.colors import Normalize

sys.path.insert(0, str(Path(__file__).parent))
from config import PROC

ROOT = Path(__file__).resolve().parents[1]
TIG = Path("C:/voting_data/raw/tiger")
OUT = ROOT / "output" / "figures"
MIN_N = 50
SNAPS = [("20240514", "Before: May 2024"), ("20250909", "After: September 2025")]

bg = gpd.read_file(f"zip://{(TIG / 'cb_2020_37_bg_500k.zip').as_posix()}")
bg = bg[bg.GEOID.str.startswith("37021")].to_crs(32617)
place = gpd.read_file(f"zip://{(TIG / 'tl_2024_37_place.zip').as_posix()}")
ash = place[place.NAME == "Asheville"].to_crs(32617)
# keep block groups with at least 25% of their area inside the city
inter = gpd.overlay(bg[["GEOID", "geometry"]], ash[["geometry"]], how="intersection")
share = (inter.set_index("GEOID").area / bg.set_index("GEOID").area.reindex(inter.GEOID).values)
keep = share[share >= 0.25].index
city = bg[bg.GEOID.isin(keep)].copy()

vals = {}
for d, _ in SNAPS:
    f = (PROC / f"bgage_{d}.parquet").as_posix()
    t = duckdb.sql(f"SELECT bg AS GEOID, sum(n) AS n, sum(age_sum) / sum(n) AS mean_age FROM read_parquet('{f}') "
                   f"WHERE bg LIKE '37021%' GROUP BY 1").df()
    m = city.merge(t, on="GEOID", how="left")
    m.loc[m.n.isna() | (m.n < MIN_N), "mean_age"] = None
    vals[d] = m
    print(d, "block groups", len(m), "matched", int(m.n.notna().sum()), "shown", int(m.mean_age.notna().sum()),
          "adults", int(m.n.fillna(0).sum()), flush=True)

allv = [v for m in vals.values() for v in m.mean_age.dropna()]
lo, hi = min(allv), max(allv)
norm = Normalize(vmin=lo, vmax=hi)
from matplotlib.colors import LinearSegmentedColormap
import numpy as np
cmap = LinearSegmentedColormap.from_list("bl", plt.get_cmap("Blues_r")(np.linspace(0, 0.82, 256)))  # one hue: dark = young, light = old; lightest step kept visible

fig, axes = plt.subplots(1, 2, figsize=(11, 6.2), constrained_layout=True)
for ax, (d, title) in zip(axes, SNAPS):
    m = vals[d]
    if m.mean_age.isna().any(): m[m.mean_age.isna()].plot(ax=ax, color="#d9d9d9", edgecolor="white", linewidth=0.4)
    m[m.mean_age.notna()].plot(ax=ax, column="mean_age", cmap=cmap, norm=norm, edgecolor="white", linewidth=0.4)
    ash.boundary.plot(ax=ax, color="#333333", linewidth=0.8)
    ax.set_title(title, fontsize=12, loc="left")
    ax.set_axis_off()
    s = m.mean_age.dropna()
    ax.text(0.0, -0.02, f"{len(s)} block groups; median of block-group means {s.median():.1f} years",
            transform=ax.transAxes, fontsize=8.5, color="#555555", va="top")
sm = plt.cm.ScalarMappable(norm=norm, cmap=cmap)
cb = fig.colorbar(sm, ax=axes, orientation="horizontal", fraction=0.04, pad=0.06, aspect=45)
cb.set_label("Mean age of registered adults in block group (darker = younger)", fontsize=9.5)
cb.outline.set_visible(False)
fig.suptitle("Asheville: age of neighbourhoods before and after Hurricane Helene", fontsize=13, x=0.02, ha="left")
fig.text(0.02, -0.02, f"Neighbourhood = 2020 Census block group with at least 25% of its area in the city (line = city boundary). "
         f"Every block group has at least {MIN_N} geocoded registered adults.\nSource: NC voter registration snapshots, geocoded to AddressNC points. "
         "Registered adults only, not the full population.", fontsize=8, color="#555555", va="top")
OUT.mkdir(parents=True, exist_ok=True)
for ext in ("png", "pdf"):
    fig.savefig(OUT / f"asheville_age_map_helene.{ext}", dpi=200, bbox_inches="tight")

# block-group change, aggregate only (for the reply)
a, b = vals["20240514"].set_index("GEOID").mean_age, vals["20250909"].set_index("GEOID").mean_age
ch = (b - a).dropna()
print("range", round(lo, 1), round(hi, 1), "change: mean", round(ch.mean(), 2), "sd", round(ch.std(), 2),
      "younger by 1+", int((ch <= -1).sum()), "older by 1+", int((ch >= 1).sum()), "of", len(ch))
print("corr before-after", round(a.corr(b), 3))
