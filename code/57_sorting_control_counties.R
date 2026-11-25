# Helene: sorting of flooded movers compared with movers in counties untouched by any 2024-25 disaster (2026-11-22).
# Treated: adults at a flooded address (inside the state Helene inundation layer, he_fl > 0.5) in the Helene study counties.
# Comparison: all adults in the 32 comparison counties (56_build_nb_control.py: no FEMA major disaster declaration for
#   Helene, PTC Eight or Chantal; not adjacent to a Helene-declared county; not Granville).
# Movers: changed address and moved to a different block group in a 16-month window; both block groups with 30+ registered adults.
# Groups: all adult movers (18+); moved with family (household of 2-6 adults, whole household to one new address, 18+);
#   split from family (household of 2-6, members at different addresses, this person moved, ages 18-65).
# (A) Sorting slope: destination block-group characteristic ~ own characteristic, by cell (treated/comparison x
#     Helene window/earlier windows), origin block group x window FE, SEs clustered by origin tract.
#     DiD of the slope: own x treated x Helene window. Event study of the slope: own x treated x window, ref 2023-24.
#     Dimensions: White, Black, Hispanic, Democrat, Republican, age. (No own income: parcel values exist only for the Helene area.)
# (B) Destination minus origin, treated x window, ref 2023-24, origin bg FE + window FE: share with bachelor's+, log median household income,
#     mean age, share White, share Democrat.
# Usage: Rscript code/57_sorting_control_counties.R   (log: logs/57_sorting_control_counties.log)
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb); library(ggplot2); library(jsonlite) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 220)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; RAW <- "C:/voting_data/raw"; TAB <- "output/tables"; FIG <- "output/figures"
WIN <- list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
NOCOVID <- Sys.getenv("DROP_COVID_WINDOW") == "1"; if (NOCOVID) WIN <- WIN[-1]  # optional: drop the 2021-22 window (COVID period), 2026-11-22
CVS <- if (NOCOVID) "_nocovid" else ""
ofile <- function(dir, f) file.path(dir, paste0(tools::file_path_sans_ext(f), CVS, ".", tools::file_ext(f)))
cvcap <- function(g) if (NOCOVID) g + labs(caption = "2021-22 window (COVID period) excluded; pre-trend test uses 2022-23 only.") else g
STORM <- "20240514"; REF <- "20230101"
sink(file(paste0("logs/57_sorting_control_counties", CVS, ".log"), open = "wt"), split = TRUE)
ctrl <- fromJSON(file.path(PROC, "control_counties.json"))$county_id
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1500MB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0", file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
q <- function(w, nbfile, where) sprintf("
  SELECT CASE WHEN c.age BETWEEN 18 AND 110 THEN c.age END AS age, trim(c.race_code) AS race, trim(c.ethnic_code) AS eth,
         trim(c.party_cd) AS party, k.bg_b, k.bg_f, h.hh_n, h.split, h.together, '%s' AS cohort
  FROM read_parquet('%s/cohort_helene_%s.parquet') c JOIN read_parquet('%s/hh_helene_%s.parquet') h USING (pid)
       JOIN read_parquet('%s/%s_helene_%s.parquet') k USING (pid)
  WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.addr_%s AND k.bg_f IS NOT NULL AND k.bg_f <> k.bg_b AND %s",
  w[1], PROC, w[1], PROC, w[1], PROC, nbfile, w[1], w[2], where)
D <- rbindlist(lapply(WIN, function(w) rbind(
  as.data.table(dbGetQuery(con, q(w, "nb", sprintf("c.he_fl > 0.5 AND abs(c.he_dist) <= 500 AND c.county_id IN (%s)", paste0("'", study, "'", collapse = ",")))))[, treated := 1L],
  as.data.table(dbGetQuery(con, q(w, "nbc", sprintf("c.county_id IN (%s)", paste0("'", ctrl, "'", collapse = ",")))))[, treated := 0L])))
CB <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/bgcomp_%s.parquet')", PROC, d)))[, cohort := d]))
AG <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT bg, sum(n) AS na, sum(age_sum) AS asum FROM read_parquet('%s/bgage_%s.parquet') GROUP BY 1", PROC, d)))[, cohort := d]))
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
D <- D[!is.na(age) & hh_n <= 6]
D[, `:=`(white = as.integer(race == "W"), black = as.integer(race == "B"), hisp = as.integer(eth == "HL"),
         dem = as.integer(party == "DEM"), rep = as.integer(party == "REP"), age10 = age / 10,
         agegrp = factor(fifelse(age < 35, "u35", fifelse(age >= 65, "o65", "m")), levels = c("m", "u35", "o65")),
         post = as.integer(cohort == STORM))]
D[, tp := treated * post]
D[, bgc := .GRP, by = .(bg_b, cohort)][, tract := as.integer(factor(substr(bg_b, 1, 11)))]
D[, cell := factor(paste0(fifelse(treated == 1, "flooded", "comparison"), "_", fifelse(post == 1, "helene", "pre")),
                   levels = c("comparison_pre", "flooded_pre", "comparison_helene", "flooded_helene"))]
GRP <- list(all = D, together = D[hh_n >= 2 & together == 1], split = D[hh_n >= 2 & split == 1 & age <= 65])
cnt <- rbindlist(lapply(names(GRP), function(g) GRP[[g]][, .N, by = .(cohort, treated)][, group := g]))[order(group, cohort, treated)][N < 10, N := NA]
cat("movers to a new block group, by group x window x treated (cells below 10 suppressed):\n"); print(cnt)
fwrite(cnt, ofile(TAB, "sorting_control_counts_helene.csv"))

tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`, n = nobs(m))] }
SPEC <- list(
  c("Race: share White (points), own White", "sh_white_1", "white", "+ i(agegrp)"),
  c("Race: share Black (points), own Black", "sh_black_1", "black", "+ i(agegrp)"),
  c("Ethnicity: share Hispanic (points), own Hispanic", "sh_hisp_1", "hisp", "+ i(agegrp)"),
  c("Party: share Democrat (points), own Democrat", "sh_dem_1", "dem", "+ i(agegrp)"),
  c("Party: share Republican (points), own Republican", "sh_rep_1", "rep", "+ i(agegrp)"),
  c("Age: mean age (years), per 10 years of own age", "mage_1", "age10", ""))
A <- rbindlist(lapply(names(GRP), function(g) rbindlist(lapply(SPEC, function(s) {
  d <- copy(GRP[[g]])
  m1 <- feols(as.formula(sprintf("%s ~ %s:cell + cell %s | bgc", s[2], s[3], s[4])), d, cluster = ~tract)
  r1 <- tidy(m1, sprintf("^%s:cell|^cell[a-z_]+:%s$", s[3], s[3]))[, `:=`(part = "slope", cell = sub(".*cell([a-z_]+).*", "\\1", term))]
  if (s[3] != "age10") r1[, n_own1_cell := d[get(s[3]) == 1, .N, by = cell][match(r1$cell, cell), fifelse(N < 10, NA_integer_, N)]]
  m2 <- feols(as.formula(sprintf("%s ~ %s + %s:treated + %s:post + %s:tp + tp %s | bgc", s[2], s[3], s[3], s[3], s[3], s[4])), d, cluster = ~tract)
  r2 <- tidy(m2, sprintf("^%s:tp$", s[3]))[, `:=`(part = "DiD", cell = "flooded x Helene window vs. comparison counties and earlier windows")]
  d[, xt := get(s[3]) * treated]
  m3 <- feols(as.formula(sprintf("%s ~ i(cohort, %s) + i(cohort, xt, ref = REF) + i(cohort, treated, ref = REF) %s | bgc", s[2], s[3], s[4])), d, cluster = ~tract)
  r3 <- tidy(m3, "::[0-9]+:xt$")[, `:=`(part = "ES", cell = sub("cohort::([0-9]+):xt", "\\1", term))]
  r3[, pre_p := wald(m3, "cohort::(20210101|20220517):xt", print = FALSE)[["p"]]]
  rbind(r1, r2, r3, fill = TRUE)[, `:=`(group = g, dimension = s[1], n_flooded_helene = d[tp == 1, .N])] }), fill = TRUE)))
fwrite(A, ofile(TAB, "sorting_control_helene.csv"))
cat("\n== (A) sorting slopes by cell, DiD of the slope, slope event study (ref 2023-24) ==\n")
print(A[, .(group, dimension = substr(dimension, 1, 45), part, cell = substr(cell, 1, 30), n_own1_cell, est = round(est, 3), se = round(se, 3), p = signif(p, 2), pre_p = round(pre_p, 2), n, n_flooded_helene)], nrows = 500)

ESV <- list(c("sh_ba", 1, "Share with bachelor's degree or more (points)"), c("linc", 100, "Log median household income (x100)"),
            c("mage", 1, "Mean age of adults (years)"), c("sh_white", 1, "Share White (points)"), c("sh_dem", 1, "Share Democrat (points)"))
ES <- rbindlist(lapply(names(GRP), function(g) rbindlist(lapply(ESV, function(v) {
  d <- copy(GRP[[g]]); d[, yy := as.numeric(v[2]) * (get(paste0(v[1], "_1")) - get(paste0(v[1], "_0")))]
  # treated is constant within an origin block group, so origin bg FE + window FE here (not bg x window)
  m <- feols(yy ~ i(cohort, treated, ref = REF) | bg_b + cohort, d, cluster = ~tract)
  r <- tidy(m, ":treated$")[, window := sub("cohort::([0-9]+):treated", "\\1", term)]
  rbind(r, data.table(term = "ref", est = 0, se = 0, p = NA, n = nobs(m), window = REF), fill = TRUE)[, `:=`(group = g, outcome = v[3],
    pre_p = wald(m, "cohort::(20210101|20220517):treated", print = FALSE)[["p"]], n_flooded_helene = d[tp == 1 & !is.na(yy), .N])] }))))
fwrite(ES, ofile(TAB, "sorting_control_es_helene.csv"))
cat("\n== (B) destination minus origin, flooded vs comparison counties x window ==\n")
print(ES[, .(group, outcome = substr(outcome, 1, 45), window, est = round(est, 3), se = round(se, 3), p = signif(p, 2), pre_p = round(pre_p, 2), n_flooded_helene)], nrows = 200)

# ============================ figures ============================================================
ink <- "#1f2a44"; muted <- "#6b7280"; grid <- "#e5e7eb"; hel <- "#2563eb"
th <- theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), plot.title = element_text(colour = ink, face = "bold"),
  plot.subtitle = element_text(colour = muted, size = 8.5), axis.text = element_text(colour = muted), strip.text = element_text(hjust = 0, colour = ink),
  plot.title.position = "plot", plot.background = element_rect(fill = "white", colour = NA), panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(colour = grid), strip.text.y = element_text(angle = 0, size = 8))
nfl <- sapply(GRP, function(d) d[tp == 1, .N])
GL <- c(all = sprintf("All adult movers (flooded 2024-25: %d)", nfl[["all"]]), together = sprintf("Moved with family, 18+ (%d)", nfl[["together"]]),
        split = sprintf("Split from family, 18-65 (%d)", nfl[["split"]]))
LAB <- c("20210101" = "2021-22", "20220517" = "2022-23", "20230101" = "2023-24\n(ref.)", "20240514" = "2024-25\n(Helene)")
P <- A[part == "ES", .(group, dimension, window = cell, est, se)]
P <- rbind(P, unique(A[part == "ES", .(group, dimension)])[, `:=`(window = REF, est = 0, se = 0)], use.names = TRUE)
P[, `:=`(dl = sub(",.*", "", dimension), wl = factor(LAB[window], levels = LAB), gl = factor(GL[group], levels = GL))]
g1 <- ggplot(P, aes(wl, est, colour = window == STORM)) + geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0, linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = hel), guide = "none") + facet_grid(dl ~ gl, scales = "free_y") +
  labs(x = "16-month window (starting year)", y = "Flooded minus comparison-county sorting slope",
       title = "Do flooded movers sort more strongly after Helene than movers in unaffected counties?",
       subtitle = paste0("Slope = destination block-group characteristic on own characteristic. Comparison: movers in 32 NC counties with no 2024-25 disaster declaration and not adjacent to a Helene county.",
                         "\nOwn characteristic x flooded x window; origin block group x window FE; tract-clustered 95% intervals; reference 2023-24. Flooded counts below 2,000 except where noted.")) + th
ggsave(ofile(FIG, "fig_sorting_control_slope_es.png"), cvcap(g1), width = 12, height = 11, dpi = 200); ggsave(ofile(FIG, "fig_sorting_control_slope_es.pdf"), cvcap(g1), width = 12, height = 11)
E <- copy(ES)[, `:=`(wl = factor(LAB[window], levels = LAB), gl = factor(GL[group], levels = GL))]
g2 <- ggplot(E, aes(wl, est, colour = window == STORM)) + geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0, linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = hel), guide = "none") + facet_grid(outcome ~ gl, scales = "free_y", labeller = labeller(outcome = label_wrap_gen(26))) +
  labs(x = "16-month window (starting year)", y = "Flooded minus comparison-county movers", title = "Destination minus origin neighbourhood: flooded movers vs. movers in unaffected counties",
       subtitle = "Flooded x window coefficients, origin block group FE and window FE, tract-clustered 95% intervals; reference 2023-24. Education and income: ACS 2019-23 block groups.") + th
ggsave(ofile(FIG, "fig_sorting_control_level_es.png"), cvcap(g2), width = 12, height = 9.5, dpi = 200); ggsave(ofile(FIG, "fig_sorting_control_level_es.pdf"), cvcap(g2), width = 12, height = 9.5)
cat("\nfigures written\n"); sink()
