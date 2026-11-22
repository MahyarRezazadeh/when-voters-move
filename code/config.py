"""Project configuration (paths).

Individual-level voter data are stored OUTSIDE the OneDrive-synced project folder
(DATA_ROOT, default C:/voting_data) so that addresses and voter identifiers are never
uploaded to a cloud service. Only aggregated outputs are written inside the project.
Override with the environment variable VOTING_DATA_ROOT.
"""
import os
from pathlib import Path

PROJECT = Path(__file__).resolve().parents[1]
DATA_ROOT = Path(os.environ.get("VOTING_DATA_ROOT", "C:/voting_data"))
RAW = DATA_ROOT / "raw"
INTER = DATA_ROOT / "intermediate"
PROC = DATA_ROOT / "processed"
OUT = PROJECT / "output"
LOGS = PROJECT / "logs"
for p in (RAW, INTER, PROC, OUT / "tables", OUT / "figures", OUT / "diagnostics", LOGS):
    p.mkdir(parents=True, exist_ok=True)

SNAP_URL = "https://s3.amazonaws.com/dl.ncsbe.gov/data/Snapshots/VR_Snapshot_{d}.zip"

# Salt for pseudonymous name hashes (diagnostics only). Kept local, never published.
SALT_FILE = DATA_ROOT / "salt.txt"
