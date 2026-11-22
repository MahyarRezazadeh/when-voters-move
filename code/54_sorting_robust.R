# Helene: robustness of the mover-sorting results in 53_sorting.R (exploratory, written 2026-11-21).
# Same sample and variables as 53_sorting.R. For each sorting dimension, the coefficient of interest is
#   own characteristic x (flooded x Helene window)  ["x:fp"]  -- do flooded movers sort differently after the storm?
# Specification ladder (all tract-clustered unless stated):
#   R0  as run in 53: y ~ x + fp + x:fp | origin bg x window                       (no flooded or x:flooded terms)
#   R1  full DiD: R0 + flooded + x:flooded                                          (reference specification from here on)
#   R2  R1 + origin address FE                                                     (within-address across windows)
#   R3  R1 + baseline household FE (hb, 27_build_hhid.py: same address incl. unit)
#   R4  R1 + flooded-specific linear trend across windows (flooded:t + x:flooded:t)
#   R5  R1 with origin county x window FE + origin bg FE instead of origin bg x window FE
#   R6  R1 + destination county x window FE (sorting within destination county)
#   R7  R2 + R4 + R6 together
#   R8  R1, SEs clustered by origin block group
#   R9  R1, households of any size and block groups with 10+ adults (more movers)
# Quarter FE: NOT identifiable. Pre-storm cohorts have one follow-up snapshot each, so the quarter of a move is not
#   observed; registr_dt does not change on a within-county address change. Origin bg x window FE already absorb any
#   time shock common to a window. The closest feasible check is (D): storm-window movers split by the snapshot
#   interval in which the move first appears (by Nov 2024 / by Jan 2025 / by Sep 2025), with timing-cell controls.
# (C) Event study (destination minus origin) re-run under R1, R2, R3, R5, R6.
# Usage (Windows machine with C:/voting_data): Rscript code/54_sorting_robust.R   (log: logs/54_sorting_robust.log)
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb); library(ggplot2) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 220)
ROOT <- Sys.getenv("VOTING_DATA_ROOT", "C:/voting_data")
PROC <- file.path(ROOT, "processed"); INTER <- file.path(ROOT, "intermediate"); RAW <- file.path(ROOT, "raw")
TAB <- "output/tables"; FIG <- "output/figures"
WIN <- list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
STORM <- "20240514"; REF <- "20230101"
sink(file("logs/54_sorting_robust.log", open = "wt"), split = TRUE)
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0", file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
D <- rbindlist(lapply(WIN, function(w) {
  tim <- if (w[1] == STORM) "CASE WHEN c.addr_20241105 THEN 'e' WHEN c.addr_20250101 THEN 'm' ELSE 'l' END" else "'pre'"
  as.data.table(dbGetQuery(con, sprintf("
  SELECT round(c.lat, 6) AS lat, round(c.lon, 6) AS lon, substr(c.geoid,1,12) AS bg, (c.he_fl > 0.5)::INT AS flooded,
         CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END AS age, trim(c.race_code) AS race, trim(c.ethnic_code) AS eth,
         trim(c.party_cd) AS party, k.bg_b, k.bg_f, c.county_id AS cty, c.dcounty_%s AS dcty, h.hh_n, x.hb, %s AS tim, '%s' AS cohort
  FROM read_parquet('%s/cohort_helene_%s.parquet') c JOIN read_parquet('%s/hh_helene_%s.parquet') h USING (pid)
       JOIN read_parquet('%s/nb_helene_%s.parquet') k USING (pid) LEFT JOIN read_parquet('%s/hhid_helene_%s.parquet') x USING (pid)
  WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0 AND abs(c.he_dist) <= 500
    AND c.county_id IN (%s) AND c.addr_%s AND k.bg_f IS NOT NULL AND k.bg_f <> k.bg_b",
  w[2], tim, w[1], PROC, w[1], PROC, w[1], PROC, w[1], PROC, w[1], paste0("'", study, "'", collapse = ","), w[2]))) }))
CB <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/bgcomp_%s.parquet')", PROC, d)))[, cohort := d]))
AG <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT bg, sum(n) AS na, sum(age_sum) AS asum FROM read_parquet('%s/bgage_%s.parquet') GROUP BY 1", PROC, d)))[, cohort := d]))
PV <- as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/parcel_value_helene.parquet')", PROC)))
dbDisconnect(con, shutdown = TRUE)
rd <- function(f) { x <- fread(f, sep = "|", colClasses = "character"); x[grepl("^1500000US37", GEO_ID), .(bg = sub("1500000US", "", GEO_ID), v = as.numeric(get(names(x)[2])))] }
ACS <- merge(rd(file.path(RAW, "acs/acsdt5y2023-b19013.dat"))[, .(bg, inc = v)], rd(file.path(RAW, "acs/acsdt5y2023-b25077.dat"))[, .(bg, hval = v)], by = "bg", all = TRUE)
ACS[inc <= 0, inc := NA][hval <= 0, hval := NA]

# neighbourhood measures at window start; keep 10+ adults here, the main sample applies 30+ below (as in 53)
NB <- merge(CB, AG, by = c("bg", "cohort"))[n >= 10, .(bg, cohort, nadult = n, sh_white = 100 * n_white / n, sh_black = 100 * n_black / n,
                                                       sh_hisp = 100 * n_hisp / n, sh_dem = 100 * n_dem / n, sh_rep = 100 * n_rep / n, mage = asum / na)]
NB <- merge(NB, ACS[, .(bg, linc = log(inc), lhval = log(hval))], by = "bg", all.x = TRUE)
nbv <- setdiff(names(NB), c("bg", "cohort"))
o <- copy(NB); setnames(o, nbv, paste0(nbv, "_0")); setnames(o, "bg", "bg_b")
f <- copy(NB); setnames(f, nbv, paste0(nbv, "_1")); setnames(f, "bg", "bg_f")
D <- merge(merge(D, o, by = c("bg_b", "cohort")), f, by = c("bg_f", "cohort"))
D <- merge(D, PV, by = c("lat", "lon"), all.x = TRUE)
D <- D[!is.na(age)]
D[, `:=`(white = as.integer(race == "W"), black = as.integer(race == "B"), hisp = as.integer(eth == "HL"),
         dem = as.integer(party == "DEM"), rep = as.integer(party == "REP"),
         agegrp = factor(fifelse(age < 35, "u35", fifelse(age >= 65, "o65", "m")), levels = c("m", "u35", "o65")),
         val_unit = fifelse(parval > 0, parval / units, NA_real_), multi = as.integer(units > 1),
         decade = fifelse(structyear >= 1800 & structyear <= 2025, as.character(10 * (structyear %/% 10)), "unknown"))]
D[, `:=`(lval = log(val_unit), lacres = log(pmax(gisacres, 0.01)), age10 = age / 10)]
D[, `:=`(bgc = .GRP), by = .(bg_b, cohort)][, tract := as.integer(factor(substr(bg_b, 1, 11)))]
D[, `:=`(loc = .GRP), by = .(lat, lon)][, ctyw := paste(cty, cohort)][, dctyw := paste(dcty, cohort)]
D[, `:=`(fp = as.integer(flooded == 1 & cohort == STORM), t = match(cohort, sapply(WIN, `[`, 1)) - match(REF, sapply(WIN, `[`, 1)))]
D[, `:=`(te = as.integer(tim == "e"), tm = as.integer(tim == "m"))]
D[, hb := as.character(hb)][is.na(hb), hb := paste0("na", loc)]
D[, main := hh_n >= 1 & hh_n <= 6 & nadult_0 >= 30 & nadult_1 >= 30]
M <- D[main == TRUE]
cat("main sample (same as 53):", nrow(M), " flooded in Helene window:", M[fp == 1, .N], "\n")
cat("wide sample (any household size, bg 10+ adults):", nrow(D), " flooded in Helene window:", D[fp == 1, .N], "\n")
cat("storm-window flooded movers by first-observed interval (e = by Nov 2024, m = by Jan 2025, l = by Sep 2025):\n"); print(M[fp == 1, .N, by = tim])
cat("flooded Helene-window movers whose address has movers in another window (identify R2):",
    M[fp == 1 & loc %in% M[cohort != STORM, unique(loc)], .N], "\n")
cat("... whose baseline household has movers in another window (identify R3):", M[fp == 1 & hb %in% M[cohort != STORM, unique(hb)], .N], "\n")

tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`, n = nobs(m))] }
CTL <- "+ lacres + multi + i(decade) + i(agegrp)"
SPEC <- list(
  c("Income: destination log median household income (x100) on log origin home value per unit", "100 * linc_1", "lval", CTL),
  c("Income: destination log median home value (x100) on log origin home value per unit", "100 * lhval_1", "lval", CTL),
  c("Race: destination share White (points) on own White", "sh_white_1", "white", "+ i(agegrp)"),
  c("Race: destination share Black (points) on own Black", "sh_black_1", "black", "+ i(agegrp)"),
  c("Ethnicity: destination share Hispanic (points) on own Hispanic", "sh_hisp_1", "hisp", "+ i(agegrp)"),
  c("Party: destination share Democrat (points) on own Democrat", "sh_dem_1", "dem", "+ i(agegrp)"),
  c("Party: destination share Republican (points) on own Republican", "sh_rep_1", "rep", "+ i(agegrp)"),
  c("Age: destination mean age (years) per 10 years of own age", "mage_1", "age10", ""))
DID <- "+ flooded + %1$s:flooded + fp + %1$s:fp"
TRD <- "+ flooded:t + %1$s:flooded:t"
LADDER <- list(
  R0 = list(lab = "R0 as run in 53 (no flooded, x:flooded)", rhs = "+ fp + %1$s:fp", fe = "bgc", cl = ~tract, dat = "M"),
  R1 = list(lab = "R1 full DiD (reference)", rhs = DID, fe = "bgc", cl = ~tract, dat = "M"),
  R2 = list(lab = "R2 + origin address FE", rhs = DID, fe = "bgc + loc", cl = ~tract, dat = "M"),
  R3 = list(lab = "R3 + baseline household FE", rhs = DID, fe = "bgc + hb", cl = ~tract, dat = "M"),
  R4 = list(lab = "R4 + flooded linear trend", rhs = paste(DID, TRD), fe = "bgc", cl = ~tract, dat = "M"),
  R5 = list(lab = "R5 county x window + bg FE", rhs = DID, fe = "ctyw + bg_b", cl = ~tract, dat = "M"),
  R6 = list(lab = "R6 + destination county x window FE", rhs = DID, fe = "bgc + dctyw", cl = ~tract, dat = "M"),
  R7 = list(lab = "R7 address FE + trend + dest county x window", rhs = paste(DID, TRD), fe = "bgc + loc + dctyw", cl = ~tract, dat = "M"),
  R8 = list(lab = "R8 R1, SE clustered by block group", rhs = DID, fe = "bgc", cl = ~bg_b, dat = "M"),
  R9 = list(lab = "R9 R1, any household size, bg 10+ adults", rhs = DID, fe = "bgc", cl = ~tract, dat = "D"))
fit <- function(s, L) {
  dat <- get(L$dat); fml <- as.formula(sprintf("%s ~ %s %s %s | %s", s[2], s[3], sprintf(L$rhs, s[3]), s[4], L$fe))
  m <- tryCatch(feols(fml, dat, cluster = L$cl), error = function(e) { cat("  failed:", L$lab, "|", conditionMessage(e), "\n"); NULL })
  if (is.null(m)) return(NULL)
  r <- tidy(m, sprintf("^(%1$s:fp|fp:%1$s|fp)$", s[3]))
  r[, term := fifelse(term == "fp", "fp", "x:fp")]
  r[, `:=`(n_flooded_helene = dat[fp == 1 & !is.na(eval(parse(text = s[2]))) & (s[3] != "lval" | !is.na(lval)), .N])] }
R <- rbindlist(lapply(SPEC, function(s) rbindlist(lapply(names(LADDER), function(k) {
  r <- fit(s, LADDER[[k]]); if (!is.null(r)) r[, `:=`(dimension = s[1], spec = k, spec_lab = LADDER[[k]]$lab)] }))))
fwrite(R, file.path(TAB, "sorting_robust_helene.csv"))
cat("\n== own characteristic x (flooded x Helene window), by specification ==\n")
W <- dcast(R[term == "x:fp"][, dim := substr(dimension, 1, 60)], dim ~ spec, value.var = "est", fun.aggregate = function(v) round(v, 2))
print(W)
print(R[term == "x:fp", .(dim = substr(dimension, 1, 48), spec, est = round(est, 3), se = round(se, 3), p = signif(p, 2), n, n_flooded_helene)])

# (C) event study of destination minus origin under several FE sets
ESP <- list(R1 = c("bgc", "M"), R2 = c("bgc + loc", "M"), R3 = c("bgc + hb", "M"), R5 = c("ctyw + bg_b", "M"), R6 = c("bgc + dctyw", "M"), R9 = c("bgc", "D"))
ES <- rbindlist(lapply(list(c("linc", "Destination minus origin log median household income (x100)"), c("lhval", "Destination minus origin log median home value (x100)"),
                            c("sh_white", "Destination minus origin share White (points)"), c("sh_dem", "Destination minus origin share Democrat (points)")), function(v) {
  rbindlist(lapply(names(ESP), function(k) {
    dat <- copy(get(ESP[[k]][2]))
    dat[, yy := (if (v[1] %in% c("linc", "lhval")) 100 else 1) * (get(paste0(v[1], "_1")) - get(paste0(v[1], "_0")))]
    m <- feols(as.formula(sprintf("yy ~ i(cohort, flooded, ref = '%s') + flooded | %s", REF, ESP[[k]][1])), dat, cluster = ~tract)
    tidy(m, ":flooded$")[, window := sub("cohort::([0-9]+):flooded", "\\1", term)][, `:=`(outcome = v[2], spec = k,
      pre_p = tryCatch(wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]], error = function(e) NA_real_))] })) }))
fwrite(ES, file.path(TAB, "sorting_robust_es_helene.csv"))
cat("\n== (C) event study, storm-window coefficient and pre-trend p, by specification ==\n")
print(ES[window == STORM, .(outcome = substr(outcome, 1, 55), spec, est = round(est, 3), se = round(se, 3), p = signif(p, 2), pre_p = round(pre_p, 3), n)])

# (D) move timing within the storm window (closest feasible substitute for quarter FE)
TD <- rbindlist(lapply(SPEC, function(s) {
  x <- s[3]
  fml <- as.formula(sprintf("%1$s ~ %2$s + flooded + %2$s:flooded + te + tm + %2$s:te + %2$s:tm + fp + %2$s:fp + fp:te + fp:tm + %2$s:fp:te + %2$s:fp:tm %3$s | bgc", s[2], x, s[4]))
  m <- feols(fml, M, cluster = ~tract)
  b <- coef(m); V <- vcov(m); nm <- names(b)
  key <- function(...) { p <- c(...); nm[sapply(nm, function(z) setequal(strsplit(z, ":")[[1]], p))] }
  k0 <- key(x, "fp"); out <- list()
  for (g in c("l", "e", "m")) {
    ks <- c(k0, if (g != "l") key(x, "fp", paste0("t", g)))
    est <- sum(b[ks]); se <- sqrt(sum(V[ks, ks]))
    out[[g]] <- data.table(dimension = s[1], timing = c(l = "late (Jan-Sep 2025)", e = "early (by Nov 2024)", m = "middle (Nov 2024-Jan 2025)")[g],
                           est = est, se = se, p = 2 * pnorm(-abs(est / se)), n_flooded = M[fp == 1 & tim == g & !is.na(eval(parse(text = s[2]))), .N])
  }
  rbindlist(out) }))
fwrite(TD, file.path(TAB, "sorting_timing_helene.csv"))
cat("\n== (D) own x flooded-in-Helene-window, by when the move first appears ==\n")
print(TD[, .(dim = substr(dimension, 1, 48), timing, est = round(est, 3), se = round(se, 3), p = signif(p, 2), n_flooded)])

# figure: x:fp coefficient across the ladder, with the all-mover sorting slope (from 53) as a reference scale
ink <- "#1f2a44"; muted <- "#6b7280"; grid <- "#e5e7eb"; hel <- "#2563eb"
th <- theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = grid),
  plot.title = element_text(colour = ink, face = "bold"), plot.subtitle = element_text(colour = muted, size = 8.5), axis.text = element_text(colour = muted),
  strip.text = element_text(hjust = 0, colour = ink), plot.title.position = "plot", plot.background = element_rect(fill = "white", colour = NA))
P <- R[term == "x:fp"][, `:=`(lab = factor(spec_lab, levels = rev(sapply(LADDER, `[[`, "lab"))), dim = sub(" on .*", "", dimension))]
g <- ggplot(P, aes(est, lab)) + geom_vline(xintercept = 0, colour = muted, linewidth = 0.4) +
  geom_errorbar(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, colour = spec == "R1"), width = 0, orientation = "y", linewidth = 0.7) +
  geom_point(aes(colour = spec == "R1"), size = 2) + scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = hel), guide = "none") +
  facet_wrap(~dim, scales = "free_x", ncol = 4, labeller = label_wrap_gen(40)) +
  labs(x = "Own characteristic x flooded x Helene window, 95% interval", y = NULL, title = "Do flooded movers sort differently after Helene? Robustness",
       subtitle = "Movers to a new block group, baseline address within 500 m of the Helene flood boundary, 2021-2025. Blue = full DiD reference (R1).") + th
ggsave(file.path(FIG, "fig_sorting_robust.png"), g, width = 14, height = 7.5, dpi = 200); ggsave(file.path(FIG, "fig_sorting_robust.pdf"), g, width = 14, height = 7.5)
cat("\nfigure written\n"); sink()
