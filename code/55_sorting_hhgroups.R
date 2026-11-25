# Helene: do flooded movers sort by neighbourhood composition, separately for the two household responses? (2026-11-22)
# Groups (from hh_helene_<b>.parquet, households of 2-6 adults, person changed address in the window):
#   together : all observed members of the baseline household are at one new address at follow-up
#   split    : observed members of the baseline household are at more than one address at follow-up, and this person moved
# Sample as in 53_sorting.R: moved to a different block group, baseline address within 500 m of the Helene flood
#   boundary, study counties, both block groups with 30+ registered adults; four 16-month windows.
#   Ages: moved with family 18+; split from family 18-65.
# (A) Sorting slope by cell (flooded/unflooded x Helene window/earlier windows), within each group:
#       destination characteristic ~ own characteristic x cell | origin block group x window, tract-clustered SEs.
#     Difference-in-differences of the slope: own x flooded x Helene window, with own x flooded and own as controls.
#     Dimensions: race (White, Black), Hispanic, party (Democrat, Republican), age, income (log origin home value per
#     unit -> log ACS median household income; lot size, decade built, multi-unit and age-group controls).
#     Education: no individual education in the voter file, so (B) only.
# (B) Event study, destination minus origin block group: share with bachelor's degree or more (ACS B15003, adults 25+),
#     log median household income, mean age, share White, share Democrat; flooded x window, ref 2023-24, by group.
# Usage: Rscript code/55_sorting_hhgroups.R  (log: logs/55_sorting_hhgroups.log)
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb); library(ggplot2) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 220)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; RAW <- "C:/voting_data/raw"; TAB <- "output/tables"; FIG <- "output/figures"
WIN <- list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
NOCOVID <- Sys.getenv("DROP_COVID_WINDOW") == "1"; if (NOCOVID) WIN <- WIN[-1]  # optional: drop the 2021-22 window (COVID period), 2026-11-22
CVS <- if (NOCOVID) "_nocovid" else ""
ofile <- function(dir, f) file.path(dir, paste0(tools::file_path_sans_ext(f), CVS, ".", tools::file_ext(f)))
cvcap <- function(g) if (NOCOVID) g + labs(caption = "2021-22 window (COVID period) excluded; pre-trend test uses 2022-23 only.") else g
STORM <- "20240514"; REF <- "20230101"
sink(file(paste0("logs/55_sorting_hhgroups", CVS, ".log"), open = "wt"), split = TRUE)
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0", file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
D <- rbindlist(lapply(WIN, function(w) as.data.table(dbGetQuery(con, sprintf("
  SELECT round(c.lat, 6) AS lat, round(c.lon, 6) AS lon, (c.he_fl > 0.5)::INT AS flooded,
         CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END AS age, trim(c.race_code) AS race, trim(c.ethnic_code) AS eth,
         trim(c.party_cd) AS party, k.bg_b, k.bg_f, CASE WHEN h.together = 1 THEN 'together' WHEN h.split = 1 THEN 'split' END AS grp, '%s' AS cohort
  FROM read_parquet('%s/cohort_helene_%s.parquet') c JOIN read_parquet('%s/hh_helene_%s.parquet') h USING (pid)
       JOIN read_parquet('%s/nb_helene_%s.parquet') k USING (pid)
  WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0 AND abs(c.he_dist) <= 500
    AND c.county_id IN (%s) AND h.hh_n BETWEEN 2 AND 6 AND c.addr_%s AND k.bg_f IS NOT NULL AND k.bg_f <> k.bg_b
    AND (h.together = 1 OR h.split = 1)",
  w[1], PROC, w[1], PROC, w[1], PROC, w[1], paste0("'", study, "'", collapse = ","), w[2])))))
CB <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/bgcomp_%s.parquet')", PROC, d)))[, cohort := d]))
AG <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT bg, sum(n) AS na, sum(age_sum) AS asum FROM read_parquet('%s/bgage_%s.parquet') GROUP BY 1", PROC, d)))[, cohort := d]))
PV <- as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/parcel_value_helene.parquet')", PROC)))
dbDisconnect(con, shutdown = TRUE)
rd <- function(f) { x <- fread(f, sep = "|", colClasses = "character"); x[grepl("^1500000US37", GEO_ID), .(bg = sub("1500000US", "", GEO_ID), v = as.numeric(get(names(x)[2])))] }
ACS <- merge(rd(file.path(RAW, "acs/acsdt5y2023-b19013.dat"))[, .(bg, inc = v)], rd(file.path(RAW, "acs/acsdt5y2023-b25077.dat"))[, .(bg, hval = v)], by = "bg", all = TRUE)
ACS[inc <= 0, inc := NA][hval <= 0, hval := NA]
ED <- fread(file.path(RAW, "acs/acsdt5y2023-b15003.dat"), sep = "|", colClasses = "character")[grepl("^1500000US37", GEO_ID)]
ED <- ED[, .(bg = sub("1500000US", "", GEO_ID), tot = as.numeric(B15003_E001),
             ba = as.numeric(B15003_E022) + as.numeric(B15003_E023) + as.numeric(B15003_E024) + as.numeric(B15003_E025))]
ACS <- merge(ACS, ED[tot > 0, .(bg, sh_ba = 100 * ba / tot)], by = "bg", all = TRUE)

NB <- merge(CB, AG, by = c("bg", "cohort"))[n >= 30, .(bg, cohort, sh_white = 100 * n_white / n, sh_black = 100 * n_black / n, sh_hisp = 100 * n_hisp / n,
                                                       sh_dem = 100 * n_dem / n, sh_rep = 100 * n_rep / n, mage = asum / na)]
NB <- merge(NB, ACS[, .(bg, linc = log(inc), sh_ba)], by = "bg", all.x = TRUE)
nbv <- setdiff(names(NB), c("bg", "cohort"))
o <- copy(NB); setnames(o, nbv, paste0(nbv, "_0")); setnames(o, "bg", "bg_b")
f <- copy(NB); setnames(f, nbv, paste0(nbv, "_1")); setnames(f, "bg", "bg_f")
D <- merge(merge(D, o, by = c("bg_b", "cohort")), f, by = c("bg_f", "cohort"))
D <- merge(D, PV, by = c("lat", "lon"), all.x = TRUE)
D <- D[!is.na(age) & !is.na(grp)]
# age restriction (user, 2026-11-22): moved with family = adults 18+ only; split from family = ages 18-65
D <- D[(grp == "together" & age >= 18) | (grp == "split" & age >= 18 & age <= 65)]
D[, `:=`(white = as.integer(race == "W"), black = as.integer(race == "B"), hisp = as.integer(eth == "HL"),
         dem = as.integer(party == "DEM"), rep = as.integer(party == "REP"), age10 = age / 10,
         agegrp = factor(fifelse(age < 35, "u35", fifelse(age >= 65, "o65", "m")), levels = c("m", "u35", "o65")),
         val_unit = fifelse(parval > 0, parval / units, NA_real_), multi = as.integer(units > 1),
         decade = fifelse(structyear >= 1800 & structyear <= 2025, as.character(10 * (structyear %/% 10)), "unknown"))]
D[, `:=`(lval = log(val_unit), lacres = log(pmax(gisacres, 0.01)))]
D[, bgc := .GRP, by = .(bg_b, cohort)][, tract := as.integer(factor(substr(bg_b, 1, 11)))]
D[, `:=`(fp = as.integer(flooded == 1 & cohort == STORM), post = as.integer(cohort == STORM))]
D[, cell := factor(paste0(fifelse(flooded == 1, "flooded", "unflooded"), "_", fifelse(post == 1, "helene", "pre")),
                   levels = c("unflooded_pre", "flooded_pre", "unflooded_helene", "flooded_helene"))]
cnt <- D[, .N, by = .(grp, cohort, flooded)][order(grp, cohort, flooded)][N < 10, N := NA]
cat("movers to a new block group by group x window x flooded (cells below 10 suppressed):\n"); print(cnt)
fwrite(cnt, ofile(TAB, "sorting_hhgroups_counts_helene.csv"))

tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`, n = nobs(m))] }
CTL <- "+ lacres + multi + i(decade) + i(agegrp)"
SPEC <- list(
  c("Race: share White (points), own White", "sh_white_1", "white", "+ i(agegrp)"),
  c("Race: share Black (points), own Black", "sh_black_1", "black", "+ i(agegrp)"),
  c("Ethnicity: share Hispanic (points), own Hispanic", "sh_hisp_1", "hisp", "+ i(agegrp)"),
  c("Party: share Democrat (points), own Democrat", "sh_dem_1", "dem", "+ i(agegrp)"),
  c("Party: share Republican (points), own Republican", "sh_rep_1", "rep", "+ i(agegrp)"),
  c("Age: mean age (years), per 10 years of own age", "mage_1", "age10", ""),
  c("Income: log median household income (x100), per log origin home value", "100 * linc_1", "lval", CTL))
A <- rbindlist(lapply(c("together", "split"), function(g) rbindlist(lapply(SPEC, function(s) {
  d <- D[grp == g]
  # slopes by cell
  m1 <- feols(as.formula(sprintf("%s ~ %s:cell + cell %s | bgc", s[2], s[3], s[4])), d, cluster = ~tract)
  r1 <- tidy(m1, sprintf("^%s:cell|^cell[a-z_]+:%s$", s[3], s[3]))[, `:=`(part = "slope", cell = sub(".*cell([a-z_]+).*", "\\1", term))]
  if (s[3] != "age10") r1[, n_own1_cell := d[!is.na(eval(parse(text = s[2]))) & get(s[3]) == 1, .N, by = cell][match(r1$cell, cell), fifelse(N < 10, NA_integer_, N)]]
  # difference-in-differences of the slope
  m2 <- feols(as.formula(sprintf("%s ~ %s + %s:flooded + %s:post + %s:fp + flooded + fp %s | bgc", s[2], s[3], s[3], s[3], s[3], s[4])), d, cluster = ~tract)
  r2 <- tidy(m2, sprintf("^%s:fp$", s[3]))[, `:=`(part = "DiD", cell = "flooded x Helene vs. flooded earlier and unflooded")]
  # event study of the slope (flooded x window, ref 2023-24)
  d[, xf := get(s[3]) * flooded]
  m3 <- feols(as.formula(sprintf("%s ~ i(cohort, %s) + i(cohort, xf, ref = REF) + i(cohort, flooded, ref = REF) + flooded %s | bgc", s[2], s[3], s[4])), d, cluster = ~tract)
  r3 <- tidy(m3, "::[0-9]+:xf$")[, `:=`(part = "ES", cell = sub("cohort::([0-9]+):xf", "\\1", term))]
  r3[, pre_p := wald(m3, "cohort::(20210101|20220517):xf", print = FALSE)[["p"]]]
  nfl <- d[fp == 1 & !is.na(eval(parse(text = s[2]))) & !is.na(get(s[3])) & (s[3] != "lval" | !is.na(lval)), .N]
  rbind(r1, r2, r3, fill = TRUE)[, `:=`(group = g, dimension = s[1], n_flooded_helene = nfl)] }), fill = TRUE)))
fwrite(A, ofile(TAB, "sorting_hhgroups_helene.csv"))
cat("\n== (A) sorting slopes by cell, DiD of the slope, and slope event study (flooded x window, ref 2023-24) ==\n")
print(A[, .(group, dimension = substr(dimension, 1, 50), part, cell, n_own1_cell, est = round(est, 3), se = round(se, 3), p = signif(p, 2), pre_p = round(pre_p, 2), n, n_flooded_helene)])

ESV <- list(c("sh_ba", 1, "Share with bachelor's degree or more (points)"), c("linc", 100, "Log median household income (x100)"),
            c("mage", 1, "Mean age of adults (years)"), c("sh_white", 1, "Share White (points)"), c("sh_dem", 1, "Share Democrat (points)"))
ES <- rbindlist(lapply(c("together", "split"), function(g) rbindlist(lapply(ESV, function(v) {
  d <- D[grp == g]; d[, yy := as.numeric(v[2]) * (get(paste0(v[1], "_1")) - get(paste0(v[1], "_0")))]
  m <- feols(yy ~ i(cohort, flooded, ref = REF) + flooded | bgc, d, cluster = ~tract)
  r <- tidy(m, ":flooded$")[, window := sub("cohort::([0-9]+):flooded", "\\1", term)]
  rbind(r, data.table(term = "ref", est = 0, se = 0, p = NA, n = nobs(m), window = REF), fill = TRUE)[, `:=`(group = g, outcome = v[3],
    pre_p = wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]], n_flooded_helene = d[fp == 1 & !is.na(yy), .N],
    mean_unfl_helene = d[flooded == 0 & post == 1, mean(yy, na.rm = TRUE)])] }))))
fwrite(ES, ofile(TAB, "sorting_hhgroups_es_helene.csv"))
cat("\n== (B) destination minus origin, flooded x window, by group ==\n")
print(ES[, .(group, outcome = substr(outcome, 1, 45), window, est = round(est, 3), se = round(se, 3), p = signif(p, 2), pre_p = round(pre_p, 2), n_flooded_helene)])

# ============================ figures ============================================================
ink <- "#1f2a44"; muted <- "#6b7280"; grid <- "#e5e7eb"; hel <- "#2563eb"
th <- theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), plot.title = element_text(colour = ink, face = "bold"),
  plot.subtitle = element_text(colour = muted, size = 8.5), axis.text = element_text(colour = muted), strip.text = element_text(hjust = 0, colour = ink),
  plot.title.position = "plot", plot.background = element_rect(fill = "white", colour = NA))
GL <- c(together = "Moved with family (whole household to one new address)", split = "Split from family (household members at different addresses)")
LAB <- c("20210101" = "2021-22", "20220517" = "2022-23", "20230101" = "2023-24\n(ref.)", "20240514" = "2024-25\n(Helene)")
P <- A[part == "ES"][, .(group, dimension, window = cell, est, se, n_flooded_helene)]
P <- rbind(P, unique(A[part == "ES", .(group, dimension, n_flooded_helene)])[, `:=`(window = REF, est = 0, se = 0)], use.names = TRUE)
P[, dl := sub(",.*", "", dimension)]
P[, wl := factor(LAB[window], levels = LAB)][, gl := factor(GL[group], levels = GL)]
g1 <- ggplot(P, aes(wl, est, colour = window == STORM)) + geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0, linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = hel), guide = "none") + facet_grid(dl ~ gl, scales = "free_y") +
  labs(x = "16-month window (starting year)", y = "Flooded minus unflooded sorting slope", title = "Do flooded movers sort more strongly after Helene? Sorting slope, by household response",
       subtitle = paste0("Slope = destination block-group characteristic on own characteristic (units as in table). Flooded x window, origin block group x window FE, tract-clustered 95% intervals.",
                         "\nFlooded movers in 2024-25: moved with family ", D[grp == "together" & fp == 1, .N], ", split from family ", D[grp == "split" & fp == 1, .N], " (both below 2,000). Ages: with family 18+, split 18-65.")) +
  th + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = grid), strip.text.y = element_text(angle = 0, size = 8))
ggsave(ofile(FIG, "fig_sorting_hhgroups_slope_es.png"), cvcap(g1), width = 11, height = 11, dpi = 200); ggsave(ofile(FIG, "fig_sorting_hhgroups_slope_es.pdf"), cvcap(g1), width = 11, height = 11)
E <- copy(ES)[, `:=`(wl = factor(LAB[window], levels = LAB), gl = factor(GL[group], levels = GL))]
g2 <- ggplot(E, aes(wl, est, colour = window == STORM)) + geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0, linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = hel), guide = "none") + facet_grid(outcome ~ gl, scales = "free_y", labeller = label_wrap_gen(28)) +
  labs(x = "16-month window (starting year)", y = "Flooded minus unflooded movers", title = "Destination minus origin neighbourhood, by household response",
       subtitle = "Flooded x window coefficients, origin block group x window FE, tract-clustered 95% intervals; reference 2023-24. Education: ACS 2019-23, adults 25+.") +
  th + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = grid), strip.text.y = element_text(angle = 0, size = 8))
ggsave(ofile(FIG, "fig_sorting_hhgroups_level_es.png"), cvcap(g2), width = 11, height = 9.5, dpi = 200); ggsave(ofile(FIG, "fig_sorting_hhgroups_level_es.pdf"), cvcap(g2), width = 11, height = 9.5)
cat("\nfigures written\n"); sink()
