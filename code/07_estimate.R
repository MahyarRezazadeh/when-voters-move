# Cohort difference-in-differences: flood exposure and re-registration in another county.
# Implements ANALYSIS_PLAN_2026-11-14.md Sections 2-6 (with Amendments A-B).
#
# Usage: Rscript code/07_estimate.R <event> <mode> [bandwidth_m]
#   event: florence | helene
#   mode : pre  -> PRE-STORM COHORTS ONLY: placebo estimate and minimum detectable effect
#                  (plan Section 8). Reads no post-storm outcome.
#          main -> all cohorts: DiD (did_<event>_bw<bw>.csv); at 500 m also inference
#                  robustness, cohort event study and post-storm dynamics.
#   Rscript code/07_estimate.R combine  -> Holm across storms; files read by plot_event_study.R.
# Reads pseudonymised cohort files from DATA_ROOT/processed via DuckDB (only needed columns
# and sample rows). Writes aggregates only to output/tables.
#
# Implementation choices (fixed before any post-storm outcome was tabulated):
#  * Flooded_a = 1 if the majority of the AddressNC points under the voter's matched key are
#    flooded (<e>_fl > 0.5); nearly all keys have one point.
#  * Sample: unambiguous match, every candidate point inside the map (<e>_fl >= 0), mean signed
#    distance to the flood boundary within the bandwidth, study county = county with at least
#    one flooded AddressNC point (Amendment B).
#  * Address FE = matched location (lat, lon). Block group / tract from the AddressNC block GEOID.
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb) })
args <- commandArgs(trailingOnly = TRUE)
ev <- if (length(args) >= 1) args[1] else "florence"
mode <- if (length(args) >= 2) args[2] else "pre"
bw <- if (length(args) >= 3) as.numeric(args[3]) else 500
DATA_ROOT <- Sys.getenv("VOTING_DATA_ROOT", "C:/voting_data")
PROC <- file.path(DATA_ROOT, "processed"); INTER <- file.path(DATA_ROOT, "intermediate")
TAB <- "output/tables"; dir.create(TAB, showWarnings = FALSE, recursive = TRUE)

design <- list(
  florence = list(e = "fl", pre = list(c("20150101", "20160607"), c("20170101", "20180508")),
                  post = c("20180508", "20190910"), ref = "20170101", ctrl = "mt",
                  fu = c("20181106", "20190101", "20190910", "20200101", "20201103"), landfall = "2018-09-14"),
  helene = list(e = "he", pre = list(c("20210101", "20220517"), c("20220517", "20230912"),
                                     c("20230101", "20240514")),
                post = c("20240514", "20250909"), ref = "20230101", ctrl = NULL,
                fu = c("20241105", "20250101", "20250909", "20260303", "20261103"), landfall = "2024-09-26"),
  combine = list()
)[[ev]]

# ---- combine: collect per-storm outputs into the files plot_event_study.R reads; Holm ------
if (ev == "combine") {
  did <- rbindlist(lapply(list.files(TAB, "^did_(florence|helene)_bw[0-9]+\\.csv$", full.names = TRUE), fread), fill = TRUE)
  prim <- did[bandwidth_m == 500]
  prim[, p_holm := p.adjust(p, "holm")]
  fwrite(prim, file.path(TAB, "did_primary_holm.csv"))
  sens <- did[, .(event = tools::toTitleCase(event), bw = bandwidth_m, est = est_pp, lo = est_pp - 1.96 * se_pp,
                  hi = est_pp + 1.96 * se_pp, se = se_pp, p, n_obs, n_tracts_both)]
  fwrite(sens[order(event, bw)], file.path(TAB, "sens_bandwidth.csv"))
  for (nm in c("es_cohort", "es_dynamics", "es_dynamics_pregap")) {
    fs <- list.files(TAB, sprintf("^%s_(florence|helene)\\.csv$", nm), full.names = TRUE)
    if (length(fs)) fwrite(rbindlist(lapply(fs, fread), fill = TRUE), file.path(TAB, paste0(nm, ".csv")))
  }
  print(prim); print(sens)
  quit(save = "no")
}
e <- design$e
if (mode == "raw") e <- paste0(e, "r")   # Amendment A robustness: flood map before removing speckle

con <- dbConnect(duckdb())
dbExecute(con, "PRAGMA memory_limit='2GB'"); dbExecute(con, "PRAGMA threads=3")
ap <- file.path(INTER, "geocode", "addresspoints_flood.parquet")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE %s_fl > 0", ap, e))$county_id

load_cohort <- function(b, f, extra = character(0), more = "") {
  ctrl <- if (!is.null(design$ctrl)) sprintf(", %s_fl AS ctrl_fl", design$ctrl) else ""
  ctrl <- paste0(ctrl, more)   # extra columns from the cohort file (modes fp / decomp)
  if (length(extra)) ctrl <- paste0(ctrl, paste0(sprintf(", cc_%1$s::INT AS cc_%1$s", extra), collapse = ""))
  q <- sprintf("
    SELECT pid, county_id, lat, lon, substr(geoid,1,12) AS bg, substr(geoid,1,11) AS tract,
           (%1$s_fl > 0.5)::INT AS flooded, %1$s_dist AS dist, %1$s_sh AS sh100 %2$s,
           cc_%3$s::INT AS cc, '%4$s' AS cohort
    FROM read_parquet('%5$s')
    WHERE tier IS NOT NULL AND tier <> 'unmatched' AND NOT ambiguous
      AND %1$s_fl >= 0 AND abs(%1$s_dist) <= %6$f
      AND county_id IN (%7$s)",
    e, ctrl, f, b, file.path(PROC, sprintf("cohort_%s_%s.parquet", ev, b)), bw,
    paste0("'", study, "'", collapse = ","))
  as.data.table(dbGetQuery(con, q))
}

if (mode == "pre") {
  D <- rbindlist(lapply(design$pre, function(x) load_cohort(x[1], x[2])))
  D[, loc := .GRP, by = .(lat, lon)]
  # placebo: the pre cohort closest to the storm plays the role of the storm cohort
  D[, placebo := as.integer(cohort == design$ref)]
  fe <- "loc + bg^cohort"
  rhs <- "flooded:placebo"
  if (!is.null(design$ctrl)) rhs <- paste0(rhs, " + i(cohort, ctrl_fl, ref = '", design$pre[[1]][1], "')")
  m <- feols(as.formula(sprintf("cc ~ %s | %s", rhs, fe)), data = D, cluster = ~tract)
  b <- coef(m)["flooded:placebo"]; se <- se(m)["flooded:placebo"]
  both <- D[, .(f = any(flooded == 1), u = any(flooded == 0)), by = tract][f & u, .N]
  base <- D[cohort == design$ref & flooded == 1, mean(cc)]
  base_u <- D[cohort == design$ref & flooded == 0, mean(cc)]
  res <- data.table(event = ev, bandwidth_m = bw, cohorts = paste(sapply(design$pre, `[`, 1), collapse = ";"),
                    n_obs = nobs(m), n_voters_flooded_ref = D[cohort == design$ref & flooded == 1, .N],
                    n_tracts_both = both, base_rate_flooded_pct = 100 * base, base_rate_unflooded_pct = 100 * base_u,
                    placebo_pp = 100 * b, placebo_se_pp = 100 * se, placebo_p = pvalue(m)["flooded:placebo"],
                    mde80_pp = 100 * 2.80 * se, mde80_pct_of_base = 100 * 2.80 * se / base)
  print(res)
  fwrite(res, file.path(TAB, sprintf("mde_%s_bw%d.csv", ev, bw)))
  if (length(design$pre) > 2) {   # Helene: lead coefficients for each earlier pre cohort
    es <- feols(as.formula(sprintf("cc ~ i(cohort, flooded, ref = '%s') | loc + bg^cohort", design$ref)),
                data = D, cluster = ~tract)
    fwrite(data.table(term = names(coef(es)), est_pp = 100 * coef(es), se_pp = 100 * se(es)),
           file.path(TAB, sprintf("pre_leads_%s_bw%d.csv", ev, bw)))
    print(coeftable(es))
  }
} else if (mode == "main") {
  # Written 2026-11-16 after the pre-storm MDE was recorded in the analysis plan (Record 2026-11-16).
  lab <- function(b, f) paste0(substr(b, 1, 4), "–", substr(f, 3, 4))
  Ev <- tools::toTitleCase(ev)
  pre <- rbindlist(lapply(design$pre, function(x) load_cohort(x[1], x[2])))
  extra <- if (bw == 500) design$fu else character(0)
  pst <- load_cohort(design$post[1], design$post[2], extra)
  D <- rbindlist(list(pre, pst[, .SD, .SDcols = names(pre)]))
  D[, loc := .GRP, by = .(lat, lon)]
  D[, post := as.integer(cohort == design$post[1])]
  ctrl <- if (!is.null(design$ctrl)) sprintf(" + i(cohort, ctrl_fl, ref = '%s')", design$pre[[1]][1]) else ""
  fe <- "loc + bg^cohort"

  # (1) cohort DiD (plan Section 5): beta on flooded x post
  m <- feols(as.formula(sprintf("cc ~ flooded:post%s | %s", ctrl, fe)), data = D, cluster = ~tract)
  k <- "flooded:post"
  both <- D[post == 1, .(f = any(flooded == 1), u = any(flooded == 0)), by = tract][f & u, .N]
  res <- data.table(event = ev, bandwidth_m = bw, n_obs = nobs(m),
                    n_voters_flooded_post = D[post == 1 & flooded == 1, .N], n_tracts_both = both,
                    rate_post_flooded_pct = 100 * D[post == 1 & flooded == 1, mean(cc)],
                    rate_post_unflooded_pct = 100 * D[post == 1 & flooded == 0, mean(cc)],
                    rate_ref_flooded_pct = 100 * D[cohort == design$ref & flooded == 1, mean(cc)],
                    est_pp = 100 * coef(m)[k], se_pp = 100 * se(m)[k], p = pvalue(m)[k])
  res[, pct_of_base := 100 * est_pp / rate_ref_flooded_pct]
  print(res)
  fwrite(res, file.path(TAB, sprintf("did_%s_bw%d.csv", ev, bw)))

  if (bw == 500) {
    # (2) inference robustness (plan Section 6): block-group clusters, Conley 1/2/5 km
    rob <- list(data.table(vcov = "tract cluster", se_pp = 100 * se(m)[k]))
    rob[[2]] <- data.table(vcov = "block-group cluster", se_pp = 100 * se(summary(m, cluster = ~bg))[k])
    for (km in c(1, 2, 5)) rob[[length(rob) + 1]] <- tryCatch(
      data.table(vcov = sprintf("Conley %g km", km), se_pp = 100 * se(summary(m, vcov = conley(km)))[k]),
      error = function(err) data.table(vcov = sprintf("Conley %g km", km), se_pp = NA_real_, note = conditionMessage(err)))
    rob <- rbindlist(rob, fill = TRUE)
    rob[, `:=`(event = ev, est_pp = res$est_pp, p = 2 * pnorm(-abs(res$est_pp / se_pp)))]
    print(rob); fwrite(rob, file.path(TAB, sprintf("did_inference_%s.csv", ev)))

    # (3) cohort event study, reference = last pre cohort
    es <- feols(as.formula(sprintf("cc ~ i(cohort, flooded, ref = '%s')%s | %s", design$ref, ctrl, fe)),
                data = D, cluster = ~tract)
    ct <- as.data.table(coeftable(es), keep.rownames = "term")[grepl("^cohort::", term)]
    ct[, b := sub("^cohort::([0-9]+):flooded$", "\\1", term)]
    allc <- c(design$pre, list(design$post))
    cal <- data.table(b = sapply(allc, `[`, 1), f = sapply(allc, `[`, 2))
    cal[, `:=`(k_label = lab(b, f), k_num = seq_len(.N), ref = b == design$ref, post = b == design$post[1])]
    out <- merge(cal, ct[, .(b, est = 100 * Estimate, se = 100 * `Std. Error`, p = `Pr(>|t|)`)], by = "b", all.x = TRUE)
    out[ref == TRUE, `:=`(est = 0, se = NA)]
    out[, `:=`(event = Ev, lo = est - 1.96 * se, hi = est + 1.96 * se)]
    setorder(out, k_num); print(out)
    fwrite(out[, .(event, k_label, k_num, est, lo, hi, se, p, ref, post)], file.path(TAB, sprintf("es_cohort_%s.csv", ev)))

    # (4) dynamics: only the ~16-month horizon has a pre-storm horizon to difference against,
    # so (as the plan prescribes) each follow-up is a cross-sectional comparison within the
    # storm cohort with block-group FE, labelled as such; the same cross-section in each pre
    # cohort gives the pre-storm flooded-unflooded gap it should be read against.
    dyn <- rbindlist(lapply(design$fu, function(f) {
      mm <- feols(as.formula(sprintf("cc_%s ~ flooded | bg", f)), data = pst, cluster = ~tract)
      data.table(event = Ev, followup = f,
                 k_num = round(as.numeric(as.Date(f, "%Y%m%d") - as.Date(design$landfall)) / 30.44, 1),
                 est = 100 * coef(mm)["flooded"], se = 100 * se(mm)["flooded"], p = pvalue(mm)["flooded"],
                 rate_flooded_pct = 100 * pst[flooded == 1, mean(get(paste0("cc_", f)))],
                 spec = "cross-section, block-group FE")
    }))
    dyn[, `:=`(lo = est - 1.96 * se, hi = est + 1.96 * se)]
    print(dyn); fwrite(dyn, file.path(TAB, sprintf("es_dynamics_%s.csv", ev)))
    gap <- rbindlist(lapply(design$pre, function(x) {
      mm <- feols(cc ~ flooded | bg, data = D[cohort == x[1]], cluster = ~tract)
      data.table(event = Ev, cohort = lab(x[1], x[2]), est = 100 * coef(mm)["flooded"], se = 100 * se(mm)["flooded"])
    }))
    print(gap); fwrite(gap, file.path(TAB, sprintf("es_dynamics_pregap_%s.csv", ev)))
  }
} else if (mode == "raw") {
  # Primary DiD on the raw Sentinel-1 map (permanent water removed, small patches kept).
  D <- rbindlist(lapply(c(design$pre, list(design$post)), function(x) load_cohort(x[1], x[2])))
  D[, loc := .GRP, by = .(lat, lon)]
  D[, post := as.integer(cohort == design$post[1])]
  ctrl <- if (!is.null(design$ctrl)) sprintf(" + i(cohort, ctrl_fl, ref = '%s')", design$pre[[1]][1]) else ""
  m <- feols(as.formula(sprintf("cc ~ flooded:post%s | loc + bg^cohort", ctrl)), data = D, cluster = ~tract)
  k <- "flooded:post"
  res <- data.table(event = ev, map = "raw", bandwidth_m = bw, n_obs = nobs(m),
                    n_voters_flooded_post = D[post == 1 & flooded == 1, .N],
                    rate_ref_flooded_pct = 100 * D[cohort == design$ref & flooded == 1, mean(cc)],
                    est_pp = 100 * coef(m)[k], se_pp = 100 * se(m)[k], p = pvalue(m)[k])
  print(res); fwrite(res, file.path(TAB, sprintf("did_%s_rawmap_bw%d.csv", ev, bw)))
} else if (mode == "fp") {
  # Helene floodplain control (plan Section 3 and 5: 100-year floodplain indicator x cohort, and
  # the within-SFHA comparison of flagged vs unflagged polygons). Written 2026-11-17, AFTER the
  # main Helene estimate was seen; the specifications are the ones the plan prespecified.
  # Floodplain status comes from code/10_sfha_assign.py (FEMA NFHL zones at the matched location).
  stopifnot(ev == "helene")
  sf <- as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')",
                                              file.path(INTER, "flood", "sfha_helene_locs.parquet"))))
  D <- rbindlist(lapply(c(design$pre, list(design$post)), function(x) load_cohort(x[1], x[2])))
  n0 <- nrow(D)
  D <- merge(D, sf, by = c("lat", "lon"))
  cat("rows before/after floodplain merge:", n0, nrow(D), "\n")
  D[, loc := .GRP, by = .(lat, lon)]
  D[, post := as.integer(cohort == design$post[1])]
  D[, zone := fifelse(sfha100 == 1, "100-year", fifelse(sfha500 == 1, "500-year", "outside"))]
  xt <- D[post == 1, .(n_voters = .N, cc_pct = 100 * mean(cc)), by = .(flooded, zone)][order(flooded, zone)]
  xr <- D[cohort == design$ref, .(cc_ref_pct = 100 * mean(cc)), by = .(flooded, zone)]
  xt <- merge(xt, xr, by = c("flooded", "zone"), all.x = TRUE)
  print(xt); fwrite(xt, file.path(TAB, sprintf("fp_crosstab_helene_bw%d.csv", bw)))
  fe <- "loc + bg^cohort"; k <- "flooded:post"
  one <- function(label, rhs, dat) {
    m <- tryCatch(feols(as.formula(sprintf("cc ~ %s | %s", rhs, fe)), data = dat, cluster = ~tract),
                  error = function(err) NULL)
    if (is.null(m) || !(k %in% names(coef(m)))) return(data.table(spec = label, note = "not estimable"))
    both <- dat[post == 1, .(f = any(flooded == 1), u = any(flooded == 0)), by = tract][f & u, .N]
    data.table(spec = label, bandwidth_m = bw, n_obs = nobs(m), n_flooded_post = dat[post == 1 & flooded == 1, .N],
               n_unflooded_post = dat[post == 1 & flooded == 0, .N], n_tracts_both = both,
               rate_ref_flooded_pct = 100 * dat[cohort == design$ref & flooded == 1, mean(cc)],
               est_pp = 100 * coef(m)[k], se_pp = 100 * se(m)[k], p = pvalue(m)[k])
  }
  ref <- design$ref
  res <- rbindlist(list(
    one("baseline (no floodplain control)", "flooded:post", D),
    one("+ 100-year floodplain x cohort", sprintf("flooded:post + i(cohort, sfha100, ref = '%s')", ref), D),
    one("+ 100-year and 500-year x cohort",
        sprintf("flooded:post + i(cohort, sfha100, ref = '%s') + i(cohort, sfha500, ref = '%s')", ref, ref), D),
    one("within 100-year floodplain only", "flooded:post", D[sfha100 == 1]),
    one("outside 100-year floodplain only", "flooded:post", D[sfha100 == 0])), fill = TRUE)
  print(res); fwrite(res, file.path(TAB, sprintf("did_helene_floodplain_bw%d.csv", bw)))
  if (bw == 500) {
    es1 <- function(label, rhs, dat) {
      es <- feols(as.formula(sprintf("cc ~ i(cohort, flooded, ref = '%s')%s | %s", ref, rhs, fe)), data = dat, cluster = ~tract)
      ct <- as.data.table(coeftable(es), keep.rownames = "term")[grepl("^cohort::[0-9]+:flooded$", term)]
      ct[, .(spec = label, cohort = sub("^cohort::([0-9]+):flooded$", "\\1", term), est_pp = 100 * Estimate,
             se_pp = 100 * `Std. Error`, p = `Pr(>|t|)`)]
    }
    es <- rbindlist(list(
      es1("+ 100-year floodplain x cohort", sprintf(" + i(cohort, sfha100, ref = '%s')", ref), D),
      es1("within 100-year floodplain only", "", D[sfha100 == 1])))
    print(es); fwrite(es, file.path(TAB, "es_cohort_helene_floodplain.csv"))
    # pre-storm cross-sectional flooded-unflooded gaps, with and without the floodplain indicator
    gap <- rbindlist(lapply(c(design$pre, list(design$post)), function(x) {
      d <- D[cohort == x[1]]
      a <- feols(cc ~ flooded | bg, data = d, cluster = ~tract)
      b <- feols(cc ~ flooded + sfha100 | bg, data = d, cluster = ~tract)
      w <- feols(cc ~ flooded | bg, data = d[sfha100 == 1], cluster = ~tract)
      data.table(cohort = x[1], gap_pp = 100 * coef(a)["flooded"], gap_se = 100 * se(a)["flooded"],
                 gap_ctrl_pp = 100 * coef(b)["flooded"], gap_ctrl_se = 100 * se(b)["flooded"],
                 sfha100_pp = 100 * coef(b)["sfha100"], sfha100_se = 100 * se(b)["sfha100"],
                 gap_within_pp = 100 * coef(w)["flooded"], gap_within_se = 100 * se(w)["flooded"])
    }))
    print(gap); fwrite(gap, file.path(TAB, "fp_gaps_helene.csv"))
  }
} else if (mode == "outcomes") {
  # Secondary outcomes of plan Section 4 with the primary specification (500 m). Run 2026-11-17.
  # All outcomes are defined for the whole baseline cohort. xprec comes from 12_followup_precinct.py.
  mk <- function(x) {
    f <- x[2]
    D <- load_cohort(x[1], f, more = sprintf(
      ", addr_%1$s::INT AS addr, wcaddr_%1$s::INT AS wc, party_%1$s::INT AS party, exit_%1$s::INT AS ex,
       (exit_%1$s AND exitrsn_%1$s IN ('R:RL','R:RS','R:RH'))::INT AS ex_moved,
       (exit_%1$s AND exitrsn_%1$s = 'R:RS')::INT AS ex_state,
       (exit_%1$s AND exitrsn_%1$s = 'R:RM')::INT AS ex_rm,
       (exit_%1$s AND exitrsn_%1$s = 'R:RD')::INT AS ex_dead,
       (addr_%1$s AND dhouse_%1$s IS NOT NULL AND trim(dhouse_%1$s) <> trim(NC_house_abbrv))::INT AS xhouse", f))
    xp <- as.data.table(dbGetQuery(con, sprintf("SELECT pid, xprec FROM read_parquet('%s')",
                                                file.path(PROC, sprintf("xprec_%s_%s.parquet", ev, x[1])))))
    merge(D, xp, by = "pid", all.x = TRUE)
  }
  D <- rbindlist(lapply(c(design$pre, list(design$post)), mk))
  D[, loc := .GRP, by = .(lat, lon)]
  D[, post := as.integer(cohort == design$post[1])]
  D[, ex_other := ex - ex_moved - ex_rm - ex_dead]
  ctrl <- if (!is.null(design$ctrl)) sprintf(" + i(cohort, ctrl_fl, ref = '%s')", design$pre[[1]][1]) else ""
  specs <- list(primary = ctrl)
  if (ev == "helene") {
    sf <- as.data.table(dbGetQuery(con, sprintf("SELECT lat, lon, sfha100 FROM read_parquet('%s')",
                                                file.path(INTER, "flood", "sfha_helene_locs.parquet"))))
    D <- merge(D, sf, by = c("lat", "lon"))
    specs[["with 100-year floodplain x cohort"]] <- sprintf(" + i(cohort, sfha100, ref = '%s')", design$ref)
  }
  fe <- "loc + bg^cohort"; k <- "flooded:post"
  outs <- c(cc = "re-registered in another county (primary)", addr = "any registration-address change",
            wc = "address change within county", xprec = "cross-precinct move", xhouse = "cross-House-district move",
            party = "party change", ex = "exit from active registration", ex_moved = "exit: recorded as moved",
            ex_state = "exit: moved from state", ex_rm = "exit: list maintenance", ex_dead = "exit: deceased",
            ex_other = "exit: other reason")
  res <- rbindlist(lapply(names(specs), function(s) rbindlist(lapply(names(outs), function(y) {
    D[, yy := get(y)]
    m <- feols(as.formula(sprintf("yy ~ flooded:post%s | %s", specs[[s]], fe)), data = D, cluster = ~tract)
    data.table(event = ev, spec = s, outcome = outs[[y]], n_obs = nobs(m),
               rate_ref_flooded_pct = 100 * D[cohort == design$ref & flooded == 1, mean(yy, na.rm = TRUE)],
               rate_post_flooded_pct = 100 * D[post == 1 & flooded == 1, mean(yy, na.rm = TRUE)],
               rate_post_unflooded_pct = 100 * D[post == 1 & flooded == 0, mean(yy, na.rm = TRUE)],
               est_pp = 100 * coef(m)[k], se_pp = 100 * se(m)[k], p = pvalue(m)[k])
  }))))
  res[, pct_of_base := 100 * est_pp / rate_ref_flooded_pct]
  print(res); fwrite(res, file.path(TAB, sprintf("outcomes_%s.csv", ev)))
  # outcome 7, return: storm-cohort voters registered in another county at ~12 months who are
  # back in the baseline county at the last follow-up (descriptive; no pre-storm counterpart)
  last <- design$fu[length(design$fu)]
  pst <- load_cohort(design$post[1], design$post[2],
                     more = sprintf(", cc_%1$s::INT AS cc_last, obs_%1$s::INT AS obs_last", last))
  rt <- pst[cc == 1, .(n_movers = .N, n_returned = sum(obs_last == 1 & cc_last == 0),
                       n_still_away = sum(cc_last == 1), n_exited = sum(obs_last == 0)), by = flooded]
  rt[, `:=`(event = ev, returned_pct = 100 * n_returned / n_movers, last_followup = last)]
  tt <- prop.test(rt$n_returned, rt$n_movers)
  rt[, p_diff := tt$p.value]
  print(rt); fwrite(rt, file.path(TAB, sprintf("outcomes_return_%s.csv", ev)))
} else if (mode == "decomp") {
  # Disaster-attributable component of the decomposition (plan Section 7, last paragraph).
  # Y_p = moved to another county AND baseline party is p. The DiD coefficients on Y_p add up
  # exactly to the primary beta, so they split the excess movers by baseline party. The same is
  # done by destination (another study county vs elsewhere in NC). Standard errors are tract-
  # clustered regression SEs scaled by N (N treated as fixed), not a cluster bootstrap.
  fpost <- design$post[2]
  mk <- function(x) load_cohort(x[1], x[2], more = sprintf(", party_cd, dcounty_%s AS dcounty", x[2]))
  D <- rbindlist(lapply(c(design$pre, list(design$post)), mk))
  D[, loc := .GRP, by = .(lat, lon)]
  D[, post := as.integer(cohort == design$post[1])]
  D[, pg := fifelse(party_cd %in% c("DEM", "REP", "UNA"), party_cd, "OTH")]
  D[, dstudy := as.integer(cc == 1 & dcounty %in% study)]
  ctrl <- if (!is.null(design$ctrl)) sprintf(" + i(cohort, ctrl_fl, ref = '%s')", design$pre[[1]][1]) else ""
  fe <- "loc + bg^cohort"; k <- "flooded:post"
  nfl <- D[post == 1 & flooded == 1, .N]
  # all flooded, unambiguously geocoded baseline voters in the storm cohort (any distance)
  nall <- dbGetQuery(con, sprintf("SELECT count(*) n FROM read_parquet('%s') WHERE tier IS NOT NULL AND tier <> 'unmatched'
                                   AND NOT ambiguous AND %s_fl > 0.5",
                                  file.path(PROC, sprintf("cohort_%s_%s.parquet", ev, design$post[1])), e))$n
  est <- function(label, y) {
    D[, yy := y]
    m <- feols(as.formula(sprintf("yy ~ flooded:post%s | %s", ctrl, fe)), data = D, cluster = ~tract)
    data.table(event = ev, component = label, est_pp = 100 * coef(m)[k], se_pp = 100 * se(m)[k], p = pvalue(m)[k])
  }
  res <- rbindlist(c(
    list(est("all movers (primary beta)", D$cc)),
    lapply(c("DEM", "REP", "UNA", "OTH"), function(p) est(paste("baseline party", p), D$cc * (D$pg == p))),
    list(est("destination: another study county", D$dstudy),
         est("destination: rest of NC", D$cc * (1 - D$dstudy)))))
  sh <- D[post == 1 & flooded == 1, .(share = .N / nfl), by = pg]
  res[, share_of_flooded_voters_pct := 100 * sh$share[match(sub("baseline party ", "", component), sh$pg)]]
  res[, `:=`(share_of_beta_pct = 100 * est_pp / est_pp[1], n_flooded_sample = nfl, n_flooded_all = nall,
             excess_movers_sample = est_pp / 100 * nfl, excess_se_sample = se_pp / 100 * nfl,
             excess_movers_all = est_pp / 100 * nall, excess_se_all = se_pp / 100 * nall)]
  print(res); fwrite(res, file.path(TAB, sprintf("decomp_beta_%s.csv", ev)))
  # destination counties of storm-cohort movers, flooded vs unflooded (cells below 10 suppressed)
  dd <- D[post == 1 & cc == 1, .N, by = .(flooded, dcounty)]
  dd <- dcast(dd, dcounty ~ flooded, value.var = "N", fill = 0); setnames(dd, c("0", "1"), c("n_unflooded", "n_flooded"))
  dd[, `:=`(share_flooded_pct = 100 * n_flooded / sum(n_flooded), share_unflooded_pct = 100 * n_unflooded / sum(n_unflooded),
            dest_in_study_area = dcounty %in% study)]
  dd <- dd[n_flooded >= 10 & n_unflooded >= 10][order(-n_flooded)]
  print(head(dd, 15)); fwrite(dd, file.path(TAB, sprintf("decomp_dest_%s.csv", ev)))
} else if (mode == "sort") {
  # Partisan sorting of movers (not prespecified; added 2026-11-17 at the author's request).
  # County lean = Republican share of Democratic + Republican registrants in the county, from
  # all baseline voters of the same cohort (statewide file), so it is fixed before the window.
  # dl = lean(destination county) - lean(baseline county) for voters who re-registered in
  # another county, 0 otherwise.
  cfile <- function(b) file.path(PROC, sprintf("cohort_%s_%s.parquet", ev, b))
  lean <- function(b) as.data.table(dbGetQuery(con, sprintf(
    "SELECT county_id, sum((party_cd='REP')::INT)::DOUBLE / sum((party_cd IN ('DEM','REP'))::INT) AS r2
     FROM read_parquet('%s') GROUP BY 1", cfile(b))))
  mk <- function(x) {
    d <- load_cohort(x[1], x[2], more = sprintf(", party_cd, dcounty_%s AS dcounty", x[2]))
    ln <- lean(x[1])
    d[, r_o := ln$r2[match(county_id, ln$county_id)]]
    d[, r_d := ln$r2[match(dcounty, ln$county_id)]]
    d[, dl := fifelse(cc == 1 & !is.na(r_d), 100 * (r_d - r_o), 0)]
    d
  }
  D <- rbindlist(lapply(c(design$pre, list(design$post)), mk))
  D[, loc := .GRP, by = .(lat, lon)]
  D[, post := as.integer(cohort == design$post[1])]
  D[, pg := fifelse(party_cd %in% c("DEM", "REP", "UNA"), party_cd, "OTH")]
  cat("movers with missing destination lean:", D[cc == 1 & is.na(r_d), .N], "\n")
  # 1. movers in the estimation sample: where they go, by party, flooded and period
  ds <- D[cc == 1 & pg != "OTH", .(n_movers = .N, lean_origin = mean(100 * r_o), lean_dest = mean(100 * r_d, na.rm = TRUE),
                                    mean_dl = mean(dl), pct_to_more_rep = 100 * mean(dl > 0)),
          by = .(period = fifelse(post == 1, "storm cohort", "pre-storm cohorts"), flooded, pg)]
  ds <- ds[n_movers >= 10][order(period, pg, flooded)]
  ds[, event := ev]; print(ds); fwrite(ds, file.path(TAB, sprintf("sorting_movers_%s.csv", ev)))
  # 2. DiD on destination-specific outcomes, by baseline party
  ctrl <- if (!is.null(design$ctrl)) sprintf(" + i(cohort, ctrl_fl, ref = '%s')", design$pre[[1]][1]) else ""
  nfl <- D[post == 1 & flooded == 1, .N]
  est <- function(label, party, y) {
    D[, yy := y]
    m <- feols(as.formula(sprintf("yy ~ flooded:post%s | loc + bg^cohort", ctrl)), data = D, cluster = ~tract)
    k <- "flooded:post"
    data.table(event = ev, party = party, outcome = label, est = coef(m)[k], se = se(m)[k], p = pvalue(m)[k])
  }
  res <- rbindlist(c(
    lapply(c("DEM", "REP", "UNA"), function(p) rbind(
      est("moved to a more Republican county (pp)", p, 100 * D$cc * (D$pg == p) * (D$dl > 0)),
      est("moved to a more Democratic county (pp)", p, 100 * D$cc * (D$pg == p) * (D$dl < 0)),
      est("move x change in county lean (pp x pp / 100)", p, D$cc * (D$pg == p) * D$dl))),
    list(est("partisans: toward own party minus away (pp)", "DEM+REP",
             100 * D$cc * (fifelse(D$pg == "REP", sign(D$dl), fifelse(D$pg == "DEM", -sign(D$dl), 0)))),
         est("REP lean change minus DEM lean change (pp x pp / 100)", "DEM+REP",
             D$cc * D$dl * (fifelse(D$pg == "REP", 1, fifelse(D$pg == "DEM", -1, 0)))))))
  res[, n_flooded_sample := nfl]
  print(res); fwrite(res, file.path(TAB, sprintf("sorting_did_%s.csv", ev)))
  # 3. all cross-county movers statewide: destination lean by party within origin county
  sw <- rbindlist(lapply(list(design$pre[[length(design$pre)]], design$post), function(x) {
    m <- as.data.table(dbGetQuery(con, sprintf(
      "SELECT county_id, party_cd, dcounty_%1$s AS dcounty FROM read_parquet('%2$s')
       WHERE cc_%1$s AND party_cd IN ('DEM','REP','UNA')", x[2], cfile(x[1]))))
    ln <- lean(x[1])
    m[, `:=`(r_o = 100 * ln$r2[match(county_id, ln$county_id)], r_d = 100 * ln$r2[match(dcounty, ln$county_id)])]
    m <- m[!is.na(r_d)]
    m[, party_cd := relevel(factor(party_cd), "DEM")]
    f <- feols(r_d ~ party_cd | county_id, data = m, cluster = ~county_id)
    a <- m[, .(n_movers = .N, lean_origin = mean(r_o), lean_dest = mean(r_d), pct_to_more_rep = 100 * mean(r_d > r_o)), by = party_cd]
    a[, `:=`(event = ev, cohort = x[1], dest_lean_vs_DEM_same_origin = c(DEM = 0, coef(f))[paste0(c("", "party_cd", "party_cd")[1 + (party_cd != "DEM")], fifelse(party_cd == "DEM", "DEM", as.character(party_cd)))],
             se = c(DEM = NA, se(f))[paste0(c("", "party_cd", "party_cd")[1 + (party_cd != "DEM")], fifelse(party_cd == "DEM", "DEM", as.character(party_cd)))])]
    a[order(party_cd)]
  }))
  print(sw); fwrite(sw, file.path(TAB, sprintf("sorting_statewide_%s.csv", ev)))
} else if (mode == "margin") {
  # Close versus safe counties (not prespecified; added 2026-11-17 at the author's request).
  # County margin = Republican share of the two-party presidential vote in the last
  # presidential election before the storm (2016 for Florence, 2020 for Helene), from
  # code/16_county_votes.py. A county is "close" if that share is within h of 50 percent,
  # for h = 5 and 10 points; otherwise it is safe for one party.
  V <- fread(file.path(TAB, "county_pres_votes.csv"))
  V[, v := if (ev == "florence") rep2_2016 else rep2_2020]
  mk <- function(x) {
    d <- load_cohort(x[1], x[2], more = sprintf(", party_cd, dcounty_%s AS dcounty", x[2]))
    d[, v_o := V$v[match(county_id, as.character(V$county_id))]]
    d[, v_d := V$v[match(dcounty, as.character(V$county_id))]]
    d
  }
  D <- rbindlist(lapply(c(design$pre, list(design$post)), mk))
  D[, loc := .GRP, by = .(lat, lon)]
  D[, post := as.integer(cohort == design$post[1])]
  D[, pg := fifelse(party_cd %in% c("DEM", "REP", "UNA"), party_cd, "OTH")]
  D[, mv := as.integer(cc == 1 & !is.na(v_d))]
  D[is.na(v_d), v_d := 0.5]
  # own-party share of the two-party vote in the origin and destination county
  D[, `:=`(own_o = fifelse(pg == "REP", v_o, 1 - v_o), own_d = fifelse(pg == "REP", v_d, 1 - v_d))]
  cat3 <- function(own, h) fifelse(abs(own - 0.5) < h, "close", fifelse(own > 0.5, "safe_own", "safe_other"))
  ctrl <- if (!is.null(design$ctrl)) sprintf(" + i(cohort, ctrl_fl, ref = '%s')", design$pre[[1]][1]) else ""
  k <- "flooded:post"
  est <- function(label, party, h, y) {
    D[, yy := y]
    m <- feols(as.formula(sprintf("yy ~ flooded:post%s | loc + bg^cohort", ctrl)), data = D, cluster = ~tract)
    data.table(event = ev, party = party, h_points = 100 * h, outcome = label, est_pp = coef(m)[k], se_pp = se(m)[k], p = pvalue(m)[k])
  }
  res <- list(); desc <- list(); org <- list()
  for (p in c("DEM", "REP")) res[[length(res) + 1]] <- est("moved to another county (all destinations)", p, NA, 100 * D$mv * (D$pg == p))
  for (h in c(0.05, 0.10)) {
    D[, `:=`(dc = cat3(own_d, h), oc = cat3(own_o, h))]
    # movers by destination type (partisans only)
    a <- D[mv == 1 & pg %in% c("DEM", "REP"), .N, by = .(period = fifelse(post == 1, "storm cohort", "pre-storm cohorts"), flooded, pg, dc)]
    a[, pct := 100 * N / sum(N), by = .(period, flooded, pg)]
    a[, `:=`(event = ev, h_points = 100 * h)]; desc[[length(desc) + 1]] <- a[N >= 10]
    # share of pre-storm moves from flooded addresses that went to a close county: the benchmark
    s0 <- D[mv == 1 & post == 0 & flooded == 1 & pg %in% c("DEM", "REP"), mean(dc == "close")]
    for (p in c("DEM", "REP")) {
      res[[length(res) + 1]] <- est("moved to a close county", p, h, 100 * D$mv * (D$pg == p) * (D$dc == "close"))
      res[[length(res) + 1]] <- est("moved to a county safe for own party", p, h, 100 * D$mv * (D$pg == p) * (D$dc == "safe_own"))
    }
    r <- est(sprintf("partisans: moves to close counties minus benchmark share (%.1f%%) of all moves", 100 * s0), "DEM+REP", h,
             100 * D$mv * (D$pg %in% c("DEM", "REP")) * ((D$dc == "close") - s0))
    res[[length(res) + 1]] <- r
    # effect on moving by type of ORIGIN county
    for (p in c("DEM", "REP")) {
      # voters of party p only, so the estimate is in percentage points of that party's voters
      P <- D[pg == p]
      P[, yy := 100 * mv]
      P[, `:=`(t_close = flooded * post * (oc == "close"), t_own = flooded * post * (oc == "safe_own"), t_oth = flooded * post * (oc == "safe_other"))]
      keep <- c("t_close", "t_own", "t_oth")[c(P[, sum(t_close)], P[, sum(t_own)], P[, sum(t_oth)]) > 0]
      m <- feols(as.formula(sprintf("yy ~ %s%s | loc + bg^cohort", paste(keep, collapse = " + "), ctrl)), data = P, cluster = ~tract)
      nn <- D[post == 1 & flooded == 1 & pg == p, .N, by = oc]
      o <- data.table(event = ev, party = p, h_points = 100 * h, origin = sub("t_", "", keep), est_pp = coef(m)[keep], se_pp = se(m)[keep],
                      p = pvalue(m)[keep])
      o[, n_flooded_party := nn$N[match(c(close = "close", own = "safe_own", oth = "safe_other")[origin], nn$oc)]]
      if (all(c("t_close", "t_own") %in% keep)) {
        vc <- vcov(m); dd <- coef(m)["t_close"] - coef(m)["t_own"]
        sd <- sqrt(vc["t_close", "t_close"] + vc["t_own", "t_own"] - 2 * vc["t_close", "t_own"])
        o <- rbind(o, data.table(event = ev, party = p, h_points = 100 * h, origin = "close minus own", est_pp = dd, se_pp = sd,
                                 p = 2 * pnorm(-abs(dd / sd)), n_flooded_party = NA))
      }
      org[[length(org) + 1]] <- o
    }
  }
  res <- rbindlist(res); desc <- rbindlist(desc)[order(h_points, pg, dc, period, flooded)]; org <- rbindlist(org)
  options(width = 200)
  print(desc); print(res); print(org)
  fwrite(desc, file.path(TAB, sprintf("margin_movers_%s.csv", ev)))
  fwrite(res, file.path(TAB, sprintf("margin_did_%s.csv", ev)))
  fwrite(org, file.path(TAB, sprintf("margin_origin_%s.csv", ev)))
}
dbDisconnect(con, shutdown = TRUE)
