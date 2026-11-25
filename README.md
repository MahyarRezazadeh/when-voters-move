# When Voters Move

Code for a research project on whether flooding from hurricanes leads voters to move and re-register in another county, and what that relocation does to the partisan make-up of the places they leave and join. The setting is North Carolina, with Hurricane Florence (2018) and Hurricane Helene (2024) as the two main events and Hurricane Matthew (2016) as a secondary one.

**Author:** Mahyar Rezazadeh, Department of Economics, Simon Fraser University

**Status:** work in progress. Results are not reported here.

## What is in this repository

Only code. No data of any kind is included: no voter records, no flood maps, no address points, and no output tables.

## Data sources (all public, downloaded by the scripts)

- North Carolina State Board of Elections voter registration snapshots, 2005 onward.
- Flood extent for Florence and Matthew: Sentinel-1 random-forest rasters, Schaffer-Smith et al. (2020), KNB, doi:10.5063/F1JM280P (CC-BY 4.0).
- Estimated inundation for Helene: North Carolina Emergency Management.
- AddressNC address points, used for local geocoding.
- FEMA Special Flood Hazard Areas, and other public county-level data used in the area-level scripts.

## Privacy design

- Individual voter records are kept outside any cloud-synced folder, under `C:/voting_data` by default (set the environment variable `VOTING_DATA_ROOT` to change it). See `code/config.py`.
- Names are never stored in analysis files. A salted hash is kept for linkage checks only; the salt is generated locally and is never published.
- Geocoding is done locally against the public address points. No voter address is sent to an outside service.
- Scripts write only aggregated, non-identifying results inside the project folder.

## How the code is organized

Run the scripts from the project root, in numerical order. The main steps are:

| Scripts | Step |
|---|---|
| `01`–`03`, `09` | Download voter snapshots, flood layers, address points and flood hazard areas |
| `04`, `05`, `08`, `10` | Geocode addresses locally and assign flood exposure |
| `06` | Build the voter cohorts |
| `07`, `plot_event_study.R` | Main difference-in-differences and event-study estimates |
| `11`–`19` | Decomposition, follow-up, descriptive statistics, tables, county votes and district outcomes |
| `20`–`64` | Household-level, area-level and neighbourhood-sorting extensions |

The `run_*_queue.ps1` files run long jobs one at a time on a Windows machine with limited memory.

## Software

- Python 3: duckdb, pyarrow, pandas, numpy, geopandas, shapely, pyproj, requests, matplotlib, pypdf
- R: data.table, duckdb, fixest, ggplot2

## Contact

https://mahyarrezazadeh.github.io
