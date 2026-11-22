"""Write the manuscript's LaTeX tables from output/tables/*.csv into manuscript/common/tables/.

Every number in a table comes from a CSV written by the estimation code; nothing is typed by
hand. Each file is a complete table float that uses \\tabnotes{...} (defined in each journal
wrapper) for the notes under the table.
Usage: py -I code/14_make_tables.py
"""
from pathlib import Path
import pandas as pd

ROOT = Path(__file__).resolve().parents[1]
TAB = ROOT / "output" / "tables"
OUT = ROOT / "manuscript" / "common" / "tables"
OUT.mkdir(parents=True, exist_ok=True)


def rd(name):
    p = TAB / name
    return pd.read_csv(p) if p.exists() else None


def n0(x):
    return f"{int(round(x)):,}"


def f(x, d=2):
    if pd.isna(x):
        return "--"
    s = f"{x:.{d}f}"
    if float(s) == 0:
        s = s.lstrip("-")
    return s.replace("-", "$-$")


def es(est, se, d=2):
    return f"{f(est, d)} ({f(se, d)})"


def pv(p):
    return "$<$0.001" if p < 0.001 else f"{p:.3f}"


def table(name, caption, label, colspec, header, body, notes, size="\\small"):
    tex = (f"\\begin{{table}}[htbp]\n\\centering\n\\caption{{{caption}}}\\label{{{label}}}\n{size}\n"
           f"\\begin{{tabular}}{{{colspec}}}\n\\toprule\n{header}\n\\midrule\n" + "\n".join(body) +
           f"\n\\bottomrule\n\\end{{tabular}}\n\\tabnotes{{{notes}}}\n\\end{{table}}\n")
    (OUT / f"{name}.tex").write_text(tex, encoding="utf-8")
    print("wrote", name)


# ---- descriptives --------------------------------------------------------------------------
d = rd("descriptives.csv")
g = {(r.event, int(r.flooded)): r for r in d.itertuples()}
cols = [("florence", 1), ("florence", 0), ("helene", 1), ("helene", 0)]
rows = [("Voters", "voters", 0), ("Democratic (\\%)", "dem", 1), ("Republican (\\%)", "rep", 1),
        ("Unaffiliated (\\%)", "una", 1), ("Age (mean, years)", "age", 1), ("Black (\\%)", "black", 1),
        ("White (\\%)", "white", 1), ("Female (\\%)", "female", 1), ("Address with unit number (\\%)", "unit", 1),
        ("Inactive record (\\%)", "inactive", 1),
        ("Re-registered in another county, last pre-storm cohort (\\%)", "cc_ref", 2)]
body = [lab + " & " + " & ".join(n0(getattr(g[c], k)) if dd == 0 else f(getattr(g[c], k), dd) for c in cols) + " \\\\"
        for lab, k, dd in rows]
table("tab_descriptives", "Baseline characteristics of voters in the estimation sample, by flood exposure",
      "tab:descriptives", "p{0.42\\linewidth}rrrr",
      " & \\multicolumn{2}{c}{Florence (May 2018)} & \\multicolumn{2}{c}{Helene (May 2024)} \\\\\n"
      "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\n & Flooded & Unflooded & Flooded & Unflooded \\\\", body,
      "Voters with an active or inactive record at the storm cohort's baseline snapshot whose address matched "
      "a single AddressNC location within 500 meters of the flood boundary in study counties. The last row is the "
      "primary outcome in the pre-storm cohort closest to the storm (2017--2018 for Florence, 2023--2024 for Helene).")

# ---- main DiD ------------------------------------------------------------------------------
prim = rd("did_primary_holm.csv").set_index("event")
sens = rd("sens_bandwidth.csv"); sens["event"] = sens.event.str.lower()
inf = {e: rd(f"did_inference_{e}.csv").set_index("vcov") for e in ("florence", "helene")}
E = ["florence", "helene"]
body = ["Effect of flooding (percentage points) & " + " & ".join(f(prim.loc[e, "est_pp"]) for e in E) + " \\\\",
        "\\quad standard error, clustered by tract & " + " & ".join(f"({f(prim.loc[e, 'se_pp'])})" for e in E) + " \\\\",
        "\\quad $p$-value & " + " & ".join(pv(prim.loc[e, "p"]) for e in E) + " \\\\",
        "\\quad Holm-adjusted $p$-value & " + " & ".join(pv(prim.loc[e, "p_holm"]) for e in E) + " \\\\",
        "Pre-storm rate at flooded addresses (\\%) & " + " & ".join(f(prim.loc[e, "rate_ref_flooded_pct"]) for e in E) + " \\\\",
        "Effect as percent of pre-storm rate & " + " & ".join(f(prim.loc[e, "pct_of_base"], 0) for e in E) + " \\\\",
        "Voter-cohort observations & " + " & ".join(n0(prim.loc[e, "n_obs"]) for e in E) + " \\\\",
        "Voters at flooded addresses, storm cohort & " + " & ".join(n0(prim.loc[e, "n_voters_flooded_post"]) for e in E) + " \\\\",
        "Tracts with flooded and unflooded addresses & " + " & ".join(n0(prim.loc[e, "n_tracts_both"]) for e in E) + " \\\\",
        "\\midrule", "\\multicolumn{3}{l}{\\emph{Other distance bands around the flood boundary}} \\\\"]
for bw in (250, 1000, 2000):
    s = sens[sens.bw == bw].set_index("event")
    body.append(f"\\quad {bw:,} meters & " + " & ".join(es(s.loc[e, "est"], s.loc[e, "se"]) for e in E) + " \\\\")
body += ["\\midrule", "\\multicolumn{3}{l}{\\emph{Other standard errors for the 500-meter estimate}} \\\\"]
for v, lab in [("block-group cluster", "Clustered by block group"), ("Conley 1 km", "Spatial correlation up to 1 km"),
               ("Conley 2 km", "Spatial correlation up to 2 km"), ("Conley 5 km", "Spatial correlation up to 5 km")]:
    body.append(f"\\quad {lab} & " + " & ".join(f"({f(inf[e].loc[v, 'se_pp'])})" for e in E) + " \\\\")
raw = rd("did_florence_rawmap_bw500.csv")
if raw is not None:
    r = raw.iloc[0]
    body += ["\\midrule", f"Flood map without removal of small patches & {es(r.est_pp, r.se_pp)} & \\\\"]
table("tab_main", "Effect of flooding at the registration address on re-registration in another county",
      "tab:main", "p{0.5\\linewidth}cc", " & Florence & Helene \\\\", body,
      "Estimates of $\\beta$ in Equation~\\eqref{eq:did}, in percentage points, with standard errors in parentheses. "
      "The outcome equals one if the voter is registered in a county other than her baseline county about 16 months "
      "after the cohort's baseline. All regressions include address and block group by cohort fixed effects; the "
      "Florence regressions add Hurricane Matthew exposure by cohort. The sample is voters within 500 meters of the "
      "flood boundary unless a row states another band. Holm adjustment is across the two storms.")

# ---- Helene floodplain ---------------------------------------------------------------------
fp = pd.concat([rd(f"did_helene_floodplain_bw{b}.csv") for b in (250, 500, 1000, 2000)])
lab = {"baseline (no floodplain control)": "No floodplain control",
       "+ 100-year floodplain x cohort": "100-year floodplain $\\times$ cohort",
       "+ 100-year and 500-year x cohort": "100-year and 500-year floodplain $\\times$ cohort",
       "outside 100-year floodplain only": "Sample: addresses outside the 100-year floodplain",
       "within 100-year floodplain only": "Sample: addresses inside the 100-year floodplain"}
body = []
for s, l in lab.items():
    x = fp[fp.spec == s].set_index("bandwidth_m")
    r = x.loc[500]
    body.append(f"{l} & {es(r.est_pp, r.se_pp)} & {pv(r.p)} & {n0(r.n_flooded_post)} & {n0(r.n_unflooded_post)} & "
                f"{n0(r.n_tracts_both)} & {f(x.est_pp.min())} to {f(x.est_pp.max())} \\\\")
table("tab_floodplain", "Hurricane Helene: effect of flooding with controls for floodplain location",
      "tab:floodplain", "p{0.3\\linewidth}cccccc",
      " & Estimate & $p$ & \\multicolumn{2}{c}{Storm-cohort voters} & Tracts & Range over \\\\\n"
      " & (SE) & & flooded & unflooded & with both & distance bands \\\\", body,
      "Estimates of $\\beta$ in Equation~\\eqref{eq:did} for Helene at 500 meters, in percentage points; the outcome "
      "is re-registration in another county. Floodplain status is the Federal Emergency Management Agency flood "
      "hazard zone of the address (100-year: Special Flood Hazard Area; 500-year: 0.2 percent annual chance zone). "
      "The last column gives the smallest and largest estimate across the 250, 500, 1,000 and 2,000 meter bands.",
      size="\\footnotesize")

# ---- secondary outcomes --------------------------------------------------------------------
of, oh = rd("outcomes_florence.csv"), rd("outcomes_helene.csv")
if of is not None and oh is not None:
    hp = oh[oh.spec == "primary"].set_index("outcome"); hf = oh[oh.spec != "primary"].set_index("outcome")
    fl = of.set_index("outcome")
    names = {"re-registered in another county (primary)": "Re-registered in another county (primary)",
             "any registration-address change": "Any change of registration address",
             "address change within county": "\\quad within the county",
             "cross-precinct move": "Moved to another precinct",
             "cross-House-district move": "Moved to another state house district",
             "party change": "Changed party",
             "exit from active registration": "Exit from active registration",
             "exit: recorded as moved": "\\quad recorded as moved",
             "exit: moved from state": "\\quad\\quad of which moved from state",
             "exit: list maintenance": "\\quad removed by list maintenance",
             "exit: deceased": "\\quad deceased",
             "exit: other reason": "\\quad other reason"}
    body = [f"{l} & {f(fl.loc[k, 'rate_ref_flooded_pct'])} & {es(fl.loc[k, 'est_pp'], fl.loc[k, 'se_pp'])} & "
            f"{f(hp.loc[k, 'rate_ref_flooded_pct'])} & {es(hp.loc[k, 'est_pp'], hp.loc[k, 'se_pp'])} & "
            f"{es(hf.loc[k, 'est_pp'], hf.loc[k, 'se_pp'])} \\\\" for k, l in names.items()]
    table("tab_outcomes", "Effect of flooding on other registration outcomes", "tab:outcomes",
          "p{0.31\\linewidth}ccccc",
          " & \\multicolumn{2}{c}{Florence} & \\multicolumn{3}{c}{Helene} \\\\\n\\cmidrule(lr){2-3}\\cmidrule(lr){4-6}\n"
          " & Pre-storm & Estimate & Pre-storm & Estimate & With floodplain \\\\\n"
          " & rate (\\%) & (SE) & rate (\\%) & (SE) & control (SE) \\\\", body,
          "Estimates of $\\beta$ in Equation~\\eqref{eq:did} at 500 meters, in percentage points, standard errors "
          "clustered by tract in parentheses. Every outcome is defined for all baseline voters and measured about 16 "
          "months after baseline. Pre-storm rate: mean of the outcome at flooded addresses in the last pre-storm "
          "cohort. ``With floodplain control'' adds 100-year floodplain status by cohort. ``Recorded as moved'' "
          "combines the removal reasons moved from county, moved from state, and moved within state. The house "
          "district of a destination address is unknown for about 40 percent of address changes and is then counted "
          "as no district move. $p$-values are not adjusted for multiple outcomes.", size="\\footnotesize")

# ---- decomposition -------------------------------------------------------------------------
body = []
for e in E:
    s = rd(f"decomp_studyarea_{e}.csv"); s = s[s.area == "study counties"].set_index("p")
    body.append(f"\\multicolumn{{8}}{{l}}{{\\emph{{{e.capitalize()} study counties}}}} \\\\")
    for p, l in [("DEM", "Democratic"), ("REP", "Republican"), ("UNA", "Unaffiliated")]:
        r = s.loc[p]
        body.append(f"\\quad {l} & {f(r.share_b_pct, 1)} & {f(r.share_f_pct, 1)} & {f(r.d_share_pp)} & "
                    f"{f(r.d_share_migration_pp)} & {f(r.d_share_switching_pp)} & {f(r.d_share_entry_pp)} & "
                    f"{f(r.d_share_exit_pp)} \\\\")
table("tab_decomp", "Change in party shares of registered voters in study counties, and its sources",
      "tab:decomp", "lccccccc",
      " & \\multicolumn{2}{c}{Share (\\%)} & Change & \\multicolumn{4}{c}{Change if only one source had operated (pp)} \\\\\n"
      "\\cmidrule(lr){2-3}\\cmidrule(lr){5-8}\n"
      " & baseline & follow-up & (pp) & moves between counties & party switching & entry & exit \\\\", body,
      "Registered voters (active and inactive) in the counties with at least one flooded address point: 77 counties "
      "for Florence (8 May 2018 to 10 September 2019) and 23 for Helene (14 May 2024 to 9 September 2025). The last "
      "four columns apply one term of Equation~\\eqref{eq:decomp} at a time to the baseline counts, so they do not "
      "add up exactly to the total change. Libertarian and other parties are omitted from the table.",
      size="\\footnotesize")

body = []
for e in E:
    b = rd(f"decomp_beta_{e}.csv").set_index("component")
    body.append(f"\\multicolumn{{4}}{{l}}{{\\emph{{{e.capitalize()}}} ({n0(b.n_flooded_sample.iloc[0])} voters at flooded addresses)}} \\\\")
    for k, l in [("all movers (primary beta)", "All parties"), ("baseline party DEM", "Democratic"),
                 ("baseline party REP", "Republican"), ("baseline party UNA", "Unaffiliated"),
                 ("baseline party OTH", "Other")]:
        r = b.loc[k]
        sh = "100.0" if k.startswith("all") else f(r.share_of_flooded_voters_pct, 1)
        body.append(f"\\quad {l} & {sh} & {es(r.est_pp, r.se_pp, 3)} & {f(r.excess_movers_sample, 0)} ({f(r.excess_se_sample, 0)}) \\\\")
table("tab_excess", "Additional voters re-registered in another county because of flooding, by baseline party",
      "tab:excess", "lccc",
      " & Share of voters at & Effect, percentage & Additional \\\\\n & flooded addresses (\\%) & points (SE) & movers (SE) \\\\",
      body,
      "Each row is a separate estimate of Equation~\\eqref{eq:did} at 500 meters in which the outcome equals one if "
      "the voter re-registered in another county and her baseline party is the row's party; the party rows add up "
      "to the first row. Additional movers: the coefficient times the number of voters at flooded addresses in the "
      "storm cohort; its standard error scales the coefficient's standard error by that number.")

# ---- appendix: event study, dynamics, return, identity -------------------------------------
ec = rd("es_cohort.csv"); dy = rd("es_dynamics.csv"); pg = rd("es_dynamics_pregap.csv")
body = []
for e in ("Florence", "Helene"):
    body.append(f"\\multicolumn{{3}}{{l}}{{\\emph{{{e}}}}} \\\\")
    for r in ec[ec.event == e].itertuples():
        kl = r.k_label.replace("\u2013", "--").replace("â€“", "--")
        if r.ref:
            body.append(f"\\quad {kl} (omitted) & 0 & \\\\")
        else:
            body.append(f"\\quad {kl}{' (storm cohort)' if r.post else ''} & {es(r.est, r.se)} & {pv(r.p)} \\\\")
fe = rd("es_cohort_helene_floodplain.csv")
if fe is not None:
    body.append("\\multicolumn{3}{l}{\\emph{Helene, with 100-year floodplain $\\times$ cohort}} \\\\")
    x = fe[fe.spec.str.startswith("+")]
    for r in x.itertuples():
        yr = {"20210101": "2021--22", "20220517": "2022--23", "20240514": "2024--25 (storm cohort)"}[str(r.cohort)]
        body.append(f"\\quad {yr} & {es(r.est_pp, r.se_pp)} & {pv(r.p)} \\\\")
table("tab_eventstudy", "Flooded-minus-unflooded difference in re-registration in another county, by cohort",
      "tab:eventstudy", "lcc", "Cohort window & Estimate (SE) & $p$ \\\\", body,
      "Coefficients on flooded address interacted with cohort indicators, relative to the last pre-storm cohort, "
      "in percentage points. Same sample and fixed effects as Table~\\ref{tab:main}.")

body = []
for e in ("Florence", "Helene"):
    body.append(f"\\multicolumn{{4}}{{l}}{{\\emph{{{e}, storm cohort}}}} \\\\")
    for r in dy[dy.event == e].itertuples():
        body.append(f"\\quad {f(r.k_num, 1)} months after landfall & {es(r.est, r.se)} & {pv(r.p)} & {f(r.rate_flooded_pct)} \\\\")
    body.append(f"\\multicolumn{{4}}{{l}}{{\\emph{{{e}, pre-storm cohorts at about 16 months}}}} \\\\")
    for r in pg[pg.event == e].itertuples():
        body.append(f"\\quad {r.cohort.replace(chr(8211), '--').replace('â€“', '--')} & {es(r.est, r.se)} & & \\\\")
table("tab_dynamics", "Flooded-minus-unflooded difference within block groups at each follow-up snapshot",
      "tab:dynamics", "lccc", " & Difference (SE) & $p$ & Rate at flooded \\\\\n & & & addresses (\\%) \\\\", body,
      "Cross-sectional regressions of re-registration in another county on the flooded indicator with block group "
      "fixed effects, in percentage points, standard errors clustered by tract. These are not differenced against a "
      "pre-storm cohort; the pre-storm rows show the difference that existed before each storm.")

rt = [rd(f"outcomes_return_{e}.csv") for e in E]
if all(x is not None for x in rt):
    body = []
    for e, x in zip(E, rt):
        for r in x.sort_values("flooded", ascending=False).itertuples():
            body.append(f"{e.capitalize()}, {'flooded' if r.flooded else 'unflooded'} addresses & {n0(r.n_movers)} & "
                        f"{n0(r.n_returned)} & {f(r.returned_pct, 1)} & {n0(r.n_exited)} \\\\")
    table("tab_return", "Return to the baseline county among voters who re-registered in another county",
          "tab:return", "lcccc", " & Movers at about & Back in baseline county & Percent & No active record \\\\\n"
          " & 12 months & at last follow-up & returned & at last follow-up \\\\", body,
          "Storm-cohort voters in the 500-meter sample who were registered in another county at the 16-month "
          "follow-up. The last follow-up is 3 November 2020 for Florence and 3 November 2026 for Helene.")

idn = pd.concat([rd(f"decomp_identity_{e}.csv") for e in E])
body = [f"{r.event.capitalize()} & {n0(r.N_b)} & {n0(r.N_f)} & {r.cells} & {r.cells_resid_nonzero} \\\\" for r in idn.itertuples()]
table("tab_identity", "Check of the accounting identity", "tab:identity", "lcccc",
      " & Registered at & Registered at & County $\\times$ party & Cells with a \\\\\n"
      " & baseline & follow-up & cells & nonzero residual \\\\", body,
      "Statewide counts of active and inactive registrants. A residual is the difference between the two sides of "
      "Equation~\\eqref{eq:decomp} in a county by party cell.")
