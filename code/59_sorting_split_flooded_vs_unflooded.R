# Helene: among adults who SPLIT from their household, do flooded movers sort differently from unflooded movers? (2026-11-22)
# Same design as 58_sorting_split_vs_family.R, with the comparison changed: treated = flooded (he_fl > 0.5), comparison =
# unflooded (baseline address within 500 m of the flood boundary). Sample: adults 18+ in households of 2-6 adults whose
# members ended at more than one address, who moved to a different block group in a 16-month window; both block groups
# with 30+ registered adults; four windows (Helene = 2024-25). Origin block group FE + window FE, tract-clustered SEs.
# (A) Sorting slope by cell (flooded/unflooded x Helene/earlier windows); DiD own x flooded x Helene window; event study
#     own x flooded x window, ref 2023-24. Dimensions: White, Black, Hispanic, Democrat, Republican, age, income.
# (B) Destination minus origin (education, income, mean age, share White, share Democrat): flooded x window, ref 2023-24.
# Usage: Rscript code/59_sorting_split_flooded_vs_unflooded.R  (log: logs/59_sorting_split_flooded_vs_unflooded.log)
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb); library(ggplot2) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 220)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; RAW <- "C:/voting_data/raw"; TAB <- "output/tables"; FIG <- "output/figures"
WIN <- list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
NOCOVID <- Sys.getenv("DROP_COVID_WINDOW") == "1"; if (NOCOVID) WIN <- WIN[-1]  # optional: drop the 2021-22 window (COVID period), 2026-11-22
CVS <- if (NOCOVID) "_nocovid" else ""
ofile <- function(dir, f) file.path(dir, paste0(tools::file_path_sans_ext(f), CVS, ".", tools::file_ext(f)))
cvcap <- function(g) if (NOCOVID) g + labs(caption = "2021-22 window (COVID period) excluded; pre-trend test uses 2022-23 only.") else g
STORM <- "20240514"; REF <- "20230101"
LBL <- "Adults who split from their household"
sink(file(paste0("logs/59_sorting_split_flooded_vs_unflooded", CVS, ".log"), open = "wt"), split = TRUE)
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0", file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
D <- rbindlist(lapply(WIN, function(w) as.data.table(dbGetQuery(con, sprintf("
  SELECT round(c.lat, 6) AS lat, round(c.lon, 6) AS lon, (c.he_fl > 0.5)::INT AS flooded,
         CASE WHEN c.age BETWEEN 18 AND 110 THEN c.age END AS age, trim(c.race_code) AS race, trim(c.ethnic_code) AS eth,
         trim(c.party_cd) AS party, k.bg_b, k.bg_f, CASE WHEN h.together = 1 THEN 'together' WHEN h.split = 1 THEN 'split' END AS grp, '%s' AS cohort
  FROM read_parquet('%s/cohort_helene_%s.parquet') c JOIN read_parquet('%s/hh_helene_%s.parquet') h USING (pid)
       JOIN read_parquet('%s/nb_helene_%s.parquet') k USING (pid)
  WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0 AND abs(c.he_dist) <= 500
    AND c.county_id IN (%s) AND h.hh_n BETWEEN 2 AND 6 AND c.addr_%s AND k.bg_f IS NOT NULL AND k.bg_f <> k.bg_b
    AND h.split = 1",
  w[1], PROC, w[1], PROC, w[1], PROC, w[1], paste0("'", study, "'", collapse = ","), w[2])))))
CB <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/bgcomp_%s.parquet')", PROC, d)))[, cohort := d]))
AG <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT bg, sum(n) AS na, sum(age_sum) AS asum FROM read_parquet('%s/bgage_%s.parquet') GROUP BY 1", PROC, d)))[, cohort := d]))
PV <- as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/parcel_value_helene.parquet')", PROC)))
dbDisconnect(con, shutdown = TRUE)
rd <- function(f) { x <- fread(f, sep = "|", colClasses = "character"); x[grepl("^1500000US37", GEO_ID), .(bg = sub("1500000US", "", GEO_ID), v = as.numeric(get(names(x)[2])))] }
ACS <- rd(file.path(RAW, "acs/acsdt5y2023-b19013.dat"))[, .(bg, inc = fifelse(v > 0, v, NA_real_))]
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
D <- D[!is.na(age) & !is.na(grp)]   # adults 18+ in both groups (user, 2026-11-22)
D[, `:=`(white = as.integer(race == "W"), black = as.integer(race == "B"), hisp = as.integer(eth == "HL"),
         dem = as.integer(party == "DEM"), rep = as.integer(party == "REP"), age10 = age / 10,
         agegrp = factor(fifelse(age < 35, "u35", fifelse(age >= 65, "o65", "m")), levels = c("m", "u35", "o65")),
         val_unit = fifelse(parval > 0, parval / units, NA_real_), multi = as.integer(units > 1),
         decade = fifelse(structyear >= 1800 & structyear <= 2025, as.character(10 * (structyear %/% 10)), "unknown"))]
D[, `:=`(lval = log(val_unit), lacres = log(pmax(gisacres, 0.01)))]
D[, tract := as.integer(factor(substr(bg_b, 1, 11)))]
D[, `:=`(post = as.integer(cohort == STORM), fl = flooded)][, sp := fl * post]
D[, cell := factor(paste0(fifelse(fl == 1, "flooded", "unflooded"), "_", fifelse(post == 1, "helene", "pre")),
                   levels = c("unflooded_pre", "flooded_pre", "unflooded_helene", "flooded_helene"))]
cnt <- D[, .N, by = .(flooded, cohort)][order(flooded, cohort)][N < 10, N := NA]
cat(LBL, "to a new block group, by group x window (cells below 10 suppressed):\n"); print(cnt)
fwrite(cnt, ofile(TAB, "sorting_split_flooded_counts_helene.csv"))

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
FE <- "| bg_b + cohort"
A <- rbindlist(lapply(SPEC, function(s) {
  d <- copy(D)
  m1 <- feols(as.formula(sprintf("%s ~ %s:cell + cell %s %s", s[2], s[3], s[4], FE)), d, cluster = ~tract)
  r1 <- tidy(m1, sprintf("^%s:cell|^cell[a-z_]+:%s$", s[3], s[3]))[, `:=`(part = "slope", cell = sub(".*cell([a-z_]+).*", "\\1", term))]
  if (!s[3] %in% c("age10", "lval")) r1[, n_own1_cell := d[get(s[3]) == 1, .N, by = cell][match(r1$cell, cell), fifelse(N < 10, NA_integer_, N)]]
  m2 <- feols(as.formula(sprintf("%s ~ %s + %s:fl + %s:post + %s:sp + fl + sp %s %s", s[2], s[3], s[3], s[3], s[3], s[4], FE)), d, cluster = ~tract)
  r2 <- tidy(m2, sprintf("^%s:sp$", s[3]))[, `:=`(part = "DiD", cell = "flooded x Helene window")]
  d[, xs := get(s[3]) * fl]
  m3 <- feols(as.formula(sprintf("%s ~ i(cohort, %s) + i(cohort, xs, ref = REF) + i(cohort, fl, ref = REF) + fl %s %s", s[2], s[3], s[4], FE)), d, cluster = ~tract)
  r3 <- tidy(m3, "::[0-9]+:xs$")[, `:=`(part = "ES", cell = sub("cohort::([0-9]+):xs", "\\1", term))]
  r3[, pre_p := wald(m3, "cohort::(20210101|20220517):xs", print = FALSE)[["p"]]]
  rbind(r1, r2, r3, fill = TRUE)[, `:=`(dimension = s[1], n_flooded_helene = d[sp == 1 & (s[3] != "lval" | !is.na(lval)), .N],
                                        n_unflooded_helene = d[fl == 0 & post == 1 & (s[3] != "lval" | !is.na(lval)), .N])] }), fill = TRUE)
fwrite(A, ofile(TAB, "sorting_split_flooded_helene.csv"))
cat("
== (A)", LBL, ": sorting slopes by cell, DiD of the slope, slope event study (ref 2023-24) ==
")
print(A[, .(dimension = substr(dimension, 1, 50), part, cell, n_own1_cell, est = round(est, 3), se = round(se, 3), p = signif(p, 2), pre_p = round(pre_p, 2), n, n_flooded_helene, n_unflooded_helene)], nrows = 200)

ESV <- list(c("sh_ba", 1, "Share with bachelor's degree or more (points)"), c("linc", 100, "Log median household income (x100)"),
            c("mage", 1, "Mean age of adults (years)"), c("sh_white", 1, "Share White (points)"), c("sh_dem", 1, "Share Democrat (points)"))
ES <- rbindlist(lapply(ESV, function(v) {
  d <- copy(D); d[, yy := as.numeric(v[2]) * (get(paste0(v[1], "_1")) - get(paste0(v[1], "_0")))]
  m <- feols(as.formula(paste("yy ~ i(cohort, fl, ref = REF) + fl + i(agegrp)", FE)), d, cluster = ~tract)
  r <- tidy(m, ":fl$")[, window := sub("cohort::([0-9]+):fl", "\\1", term)]
  rbind(r, data.table(term = "ref", est = 0, se = 0, p = NA, n = nobs(m), window = REF), fill = TRUE)[, `:=`(outcome = v[3],
    pre_p = wald(m, "cohort::(20210101|20220517):fl", print = FALSE)[["p"]], n_flooded_helene = d[sp == 1 & !is.na(yy), .N])] }))
fwrite(ES, ofile(TAB, "sorting_split_flooded_es_helene.csv"))
cat("\n== (B) destination minus origin, flooded minus unflooded x window ==\n")
print(ES[, .(outcome = substr(outcome, 1, 45), window, est = round(est, 3), se = round(se, 3), p = signif(p, 2), pre_p = round(pre_p, 2), n_flooded_helene)])

# ============================ figures ============================================================
ink <- "#1f2a44"; muted <- "#6b7280"; grid <- "#e5e7eb"; hel <- "#2563eb"
th <- theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), plot.title = element_text(colour = ink, face = "bold"),
  plot.subtitle = element_text(colour = muted, size = 8.5), axis.text = element_text(colour = muted), strip.text = element_text(hjust = 0, colour = ink),
  plot.title.position = "plot", plot.background = element_rect(fill = "white", colour = NA), panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour = grid))
LAB <- c("20210101" = "2021-22", "20220517" = "2022-23", "20230101" = "2023-24\n(ref.)", "20240514" = "2024-25\n(Helene)")
P <- A[part == "ES", .(dimension, window = cell, est, se)]
P <- rbind(P, unique(A[part == "ES", .(dimension)])[, `:=`(window = REF, est = 0, se = 0)], use.names = TRUE)
P[, `:=`(dl = sub(",.*", "", dimension), wl = factor(LAB[window], levels = LAB))]
ns <- D[sp == 1, .N]; nf <- D[fl == 0 & post == 1, .N]
g1 <- ggplot(P, aes(wl, est, colour = window == STORM)) + geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0, linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = hel), guide = "none") + facet_wrap(~dl, scales = "free_y", ncol = 2) +
  labs(x = "16-month window (starting year)", y = "Flooded minus unflooded sorting slope",
       title = paste0(LBL, ": do flooded movers sort differently from unflooded movers?"),
       subtitle = paste0("Slope = destination block-group characteristic on own characteristic. Own characteristic x flooded x window; origin block group FE and window FE;",
                         "\ntract-clustered 95% intervals; reference 2023-24. ", LBL, " in 2024-25: flooded ", ns, ", unflooded within 500 m ", nf, ".")) + th
ggsave(ofile(FIG, "fig_sorting_split_flooded_slope_es.png"), cvcap(g1), width = 11, height = 9.5, dpi = 200); ggsave(ofile(FIG, "fig_sorting_split_flooded_slope_es.pdf"), cvcap(g1), width = 11, height = 9.5)
E <- copy(ES)[, wl := factor(LAB[window], levels = LAB)]
g2 <- ggplot(E, aes(wl, est, colour = window == STORM)) + geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0, linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = hel), guide = "none") + facet_wrap(~outcome, scales = "free_y", ncol = 2) +
  labs(x = "16-month window (starting year)", y = "Flooded minus unflooded movers", title = paste0(LBL, ": destination minus origin neighbourhood, flooded vs. unflooded"),
       subtitle = "Flooded x window coefficients, origin block group FE and window FE, age-group controls, tract-clustered 95% intervals; reference 2023-24.") + th
ggsave(ofile(FIG, "fig_sorting_split_flooded_level_es.png"), cvcap(g2), width = 11, height = 8.5, dpi = 200); ggsave(ofile(FIG, "fig_sorting_split_flooded_level_es.pdf"), cvcap(g2), width = 11, height = 8.5)
cat("\nfigures written\n"); sink()
