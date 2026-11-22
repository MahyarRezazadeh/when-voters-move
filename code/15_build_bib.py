"""Build manuscript/common/references.bib: only the entries cited in manuscript/common/*.tex.

Entries come from literature/references.bib and manuscript/data_sources.bib. Three entries in
the literature file carry working notes inside bibliographic fields; they are replaced by the
clean versions below. Prints any cited key that has no entry.
Usage: py -I code/15_build_bib.py
"""
import re
from pathlib import Path

ROOT = Path(__file__).resolve().parents[1]
COMMON = ROOT / "manuscript" / "common"
src = (ROOT / "literature" / "references.bib").read_text(encoding="utf-8") + "\n" + \
      (ROOT / "manuscript" / "data_sources.bib").read_text(encoding="utf-8")

CLEAN = {
    "brown2025sources": """@techreport{brown2025sources,
  author = {Brown, Jacob R. and Cantoni, Enrico and Enos, Ryan D. and Pons, Vincent and Sartre, Emilie},
  title = {Sources and Extent of Rising Partisan Segregation in the {U.S.}: Evidence from 143 Million Voters},
  year = {2025},
  type = {Working Paper},
  number = {33422},
  institution = {National Bureau of Economic Research},
  doi = {10.3386/w33422}
}""",
    "cooper2026electoral": """@article{cooper2026electoral,
  author = {Cooper, Christopher A.},
  title = {Electoral Resilience in the Face of Disaster: The Impact of {Hurricane Helene} on the 2024 Election in {North Carolina}},
  year = {2026},
  journal = {State Politics \\& Policy Quarterly},
  note = {Advance online publication}
}""",
    "johnson2026threeessays": """@phdthesis{johnson2026threeessays,
  author = {Johnson, Thomas Ryan},
  title = {Three Essays in Political Economy},
  year = {2026},
  school = {Washington University in St. Louis},
  doi = {10.7936/hp7d-td03}
}""",
    "fema_nfhl": """@misc{fema_nfhl,
  author = {{Federal Emergency Management Agency}},
  title = {National Flood Hazard Layer: Flood Hazard Zones},
  year = {2026},
  howpublished = {Hosted copy: Esri, {USA} Flood Hazard Reduced Set. \\url{https://services.arcgis.com/P3ePLMYs2RVChkJx/arcgis/rest/services/USA_Flood_Hazard_Reduced_Set_gdb/FeatureServer}},
  note = {Accessed 17 November 2026}
}""",
}


def entries(text):
    out, i = {}, 0
    for m in re.finditer(r"^@(\w+)\{([^,\s]+),", text, re.M):
        start = m.start(); depth = 0; j = text.index("{", start)
        for k in range(j, len(text)):
            if text[k] == "{":
                depth += 1
            elif text[k] == "}":
                depth -= 1
                if depth == 0:
                    out[m.group(2)] = text[start:k + 1]; break
    return out


E = entries(src)
keys = []
for f in sorted(COMMON.glob("*.tex")):
    for m in re.finditer(r"\\cite[tp]?\*?(?:\[[^\]]*\])*\{([^}]*)\}", f.read_text(encoding="utf-8")):
        keys += [k.strip() for k in m.group(1).split(",")]
keys = sorted(set(keys))
missing = [k for k in keys if k not in E and k not in CLEAN]
body = "\n\n".join(CLEAN.get(k, E.get(k, "")) for k in keys if k not in missing)
body = body.replace("@thesis{", "@phdthesis{")
(COMMON / "references.bib").write_text(body + "\n", encoding="utf-8")
print(len(keys), "cited;", "missing:", missing)
