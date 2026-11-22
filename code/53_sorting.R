# Helene area: what kinds of residential sorting happen among movers? (descriptive + flood interaction; 2026-11-21)
# Sample: adults who changed address and moved to a different block group within a 16-month window, baseline address
#   within 500 m of the Helene flood boundary, household of 1-6 adults, both block groups with 30+ registered adults.
# (A) Sorting: destination block-group characteristic ~ own characteristic | origin block group x window FE (people
#     leaving the same neighbourhood in the same window), tract-clustered SEs.
#     Income: own = log assessed value per unit of the origin home (parcel value / address points on parcel), controls:
#     log lot acres, decade built, multi-unit parcel, age group. Destination = log ACS 2019-23 median household income
#     (and median home value) of the destination block group.
#     Race/ethnicity/party: own = White / Black / Hispanic / Democrat / Republican; destination = share of registered
#     adults in that group (measured at window start). Age: destination mean age per 10 years of own age.
# (B) Do flooded movers in the Helene window sort differently? own characteristic x (flooded x Helene window).
# (C) Event study: destination minus origin block-group income / home value, flooded x window, ref 2023-24.
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb); library(ggplot2) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 220)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; TAB <- "output/tables"; FIG <- "output/figures"
WIN <- list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
STORM <- "20240514"; REF <- "20230101"
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0", file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
D <- rbindlist(lapply(WIN, function(w) as.data.table(dbGetQuery(con, sprintf("
  SELECT round(c.lat, 6) AS lat, round(c.lon, 6) AS lon, substr(c.geoid,1,12) AS bg, (c.he_fl > 0.5)::INT AS flooded,
         CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END AS age, trim(c.race_code) AS race, trim(c.ethnic_code) AS eth,
         trim(c.party_cd) AS party, k.bg_b, k.bg_f, '%s' AS cohort
  FROM read_parquet('%s/cohort_helene_%s.parquet') c JOIN read_parquet('%s/hh_helene_%s.parquet') h USING (pid)
       JOIN read_parquet('%s/nb_helene_%s.parquet') k USING (pid)
  WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0 AND abs(c.he_dist) <= 500
    AND c.county_id IN (%s) AND h.hh_n BETWEEN 1 AND 6 AND c.addr_%s AND k.bg_f IS NOT NULL AND k.bg_f <> k.bg_b",
  w[1], PROC, w[1], PROC, w[1], PROC, w[1], paste0("'", study, "'", collapse = ","), w[2])))))
CB <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/bgcomp_%s.parquet')", PROC, d)))[, cohort := d]))
AG <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT bg, sum(n) AS na, sum(age_sum) AS asum FROM read_parquet('%s/bgage_%s.parquet') GROUP BY 1", PROC, d)))[, cohort := d]))
PV <- as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/parcel_value_helene.parquet')", PROC)))
dbDisconnect(con, shutdown = TRUE)
rd <- function(f) { x <- fread(f, sep = "|", colClasses = "character"); x[grepl("^1500000US37", GEO_ID), .(bg = sub("1500000US", "", GEO_ID), v = as.numeric(get(names(x)[2])))] }
ACS <- merge(rd("C:/voting_data/raw/acs/acsdt5y2023-b19013.dat")[, .(bg, inc = v)], rd("C:/voting_data/raw/acs/acsdt5y2023-b25077.dat")[, .(bg, hval = v)], by = "bg", all = TRUE)
ACS[inc <= 0, inc := NA][hval <= 0, hval := NA]

# neighbourhood measures at window start (30+ registered adults)
NB <- merge(CB, AG, by = c("bg", "cohort"))[n >= 30, .(bg, cohort, sh_white = 100 * n_white / n, sh_black = 100 * n_black / n, sh_hisp = 100 * n_hisp / n,
                                                       sh_dem = 100 * n_dem / n, sh_rep = 100 * n_rep / n, mage = asum / na)]
NB <- merge(NB, ACS[, .(bg, linc = log(inc), lhval = log(hval))], by = "bg", all.x = TRUE)
o <- copy(NB); setnames(o, setdiff(names(o), c("bg", "cohort")), paste0(setdiff(names(o), c("bg", "cohort")), "_0")); setnames(o, "bg", "bg_b")
f <- copy(NB); setnames(f, setdiff(names(f), c("bg", "cohort")), paste0(setdiff(names(f), c("bg", "cohort")), "_1")); setnames(f, "bg", "bg_f")
D <- merge(merge(D, o, by = c("bg_b", "cohort")), f, by = c("bg_f", "cohort"))
D <- merge(D, PV, by = c("lat", "lon"), all.x = TRUE)
D <- D[!is.na(age)]
D[, `:=`(white = as.integer(race == "W"), black = as.integer(race == "B"), hisp = as.integer(eth == "HL"),
         dem = as.integer(party == "DEM"), rep = as.integer(party == "REP"),
         agegrp = factor(fifelse(age < 35, "u35", fifelse(age >= 65, "o65", "m")), levels = c("m", "u35", "o65")),
         val_unit = fifelse(parval > 0, parval / units, NA_real_), multi = as.integer(units > 1),
         decade = fifelse(structyear >= 1800 & structyear <= 2025, as.character(10 * (structyear %/% 10)), "unknown"))]
D[, `:=`(lval = log(val_unit), lacres = log(pmax(gisacres, 0.01)), age10 = age / 10)]
D[, bgc := .GRP, by = .(bg_b, cohort)][, tract := as.integer(factor(substr(bg_b, 1, 11)))]
D[, fp := as.integer(flooded == 1 & cohort == STORM)]
cat("movers to a new block group:", nrow(D), " flooded in Helene window:", D[fp == 1, .N], "\n")
cat("origin home matched to parcel with value > 0:", D[!is.na(lval), .N], " (flooded Helene window:", D[fp == 1 & !is.na(lval), .N], ")\n")
cat("residential use among valued:", round(D[!is.na(lval), mean(grepl("RES", toupper(parusedesc)))], 3), "\n")

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
A <- rbindlist(lapply(SPEC, function(s) {
  m <- feols(as.formula(sprintf("%s ~ %s %s | bgc", s[2], s[3], s[4])), D, cluster = ~tract)
  r1 <- tidy(m, sprintf("^%s$", gsub("([()/])", "\\\\\\1", s[3])))[, `:=`(dimension = s[1], part = "A: all movers")]
  m2 <- feols(as.formula(sprintf("%s ~ %s + fp + %s:fp %s | bgc", s[2], s[3], s[3], s[4])), D, cluster = ~tract)
  r2 <- tidy(m2, "fp")[, `:=`(dimension = s[1], part = "B: flooded movers, Helene window (difference)")]
  rbind(r1, r2)[, n_flooded_helene := D[fp == 1 & !is.na(eval(parse(text = s[2]))) & (s[3] != "lval" | !is.na(lval)), .N]] }))
fwrite(A, file.path(TAB, "sorting_movers_helene.csv"))
cat("\n== (A) sorting among all movers and (B) difference for flooded movers in the Helene window ==\n")
print(A[, .(dimension = substr(dimension, 1, 95), part, term, est = round(est, 3), se = round(se, 3), p = signif(p, 2), n, n_flooded_helene)])

# (C) event study: does flooding change the income level of the destination (destination minus origin)?
ES <- rbindlist(lapply(list(c("linc", "Destination minus origin log median household income (x100)"), c("lhval", "Destination minus origin log median home value (x100)"),
                            c("sh_white", "Destination minus origin share White (points)"), c("sh_dem", "Destination minus origin share Democrat (points)")), function(v) {
  D[, yy := (if (v[1] %in% c("linc", "lhval")) 100 else 1) * (get(paste0(v[1], "_1")) - get(paste0(v[1], "_0")))]
  m <- feols(yy ~ i(cohort, flooded, ref = REF) + flooded | bgc, D, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, window := sub("cohort::([0-9]+):flooded", "\\1", term)]
  rbind(o, data.table(term = "ref", est = 0, se = 0, p = NA, n = nobs(m), window = REF), fill = TRUE)[, `:=`(outcome = v[2],
    pre_p = wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]], n_flooded_helene = D[fp == 1 & !is.na(yy), .N])] }))
fwrite(ES, file.path(TAB, "sorting_movers_es_helene.csv"))
cat("\n== (C) event study, flooded x window, origin block group x window FE ==\n")
print(ES[, .(outcome = substr(outcome, 1, 62), window, est = round(est, 3), se = round(se, 3), p = signif(p, 2), pre_p = round(pre_p, 3), n_flooded_helene)])

# ============================ figures ============================================================
ink <- "#1f2a44"; muted <- "#6b7280"; grid <- "#e5e7eb"; hel <- "#2563eb"
th <- theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(), panel.grid.major.x = element_line(colour = grid),
  plot.title = element_text(colour = ink, face = "bold"), plot.subtitle = element_text(colour = muted, size = 8.5), axis.text = element_text(colour = muted),
  strip.text = element_text(hjust = 0, colour = ink), plot.title.position = "plot", plot.background = element_rect(fill = "white", colour = NA))
P <- A[part == "A: all movers" | grepl(":fp$", term), .(lab = sub(" on .*", "", sub("^[A-Za-z]+: ", "", dimension)), dim = sub(":.*", "", dimension), part, est, se)]
P[, lab := factor(paste0(dim, ": ", lab), levels = rev(unique(paste0(dim, ": ", lab))))]
P[, part := factor(part, levels = c("A: all movers", "B: flooded movers, Helene window (difference)"), labels = c("All movers: sorting coefficient", "Flooded movers in Helene window: difference"))]
g <- ggplot(P, aes(est, lab)) + geom_vline(xintercept = 0, colour = muted, linewidth = 0.4) +
  geom_errorbar(aes(xmin = est - 1.96 * se, xmax = est + 1.96 * se, colour = part), width = 0, orientation = "y", linewidth = 0.8) + geom_point(aes(colour = part), size = 2.4) +
  scale_colour_manual(values = c(ink, hel), guide = "none") + facet_wrap(~part, scales = "free_x") +
  labs(x = "Coefficient (units in row label), 95% interval", y = NULL, title = "What kinds of sorting happen among movers?",
       subtitle = paste0("Movers to a new block group, baseline address within 500 m of the Helene flood boundary, 2021-2025 (", format(nrow(D), big.mark = ","), " movers).",
                         "\nOrigin block group x window fixed effects; tract-clustered SEs. Income rows: own = log assessed value per unit of origin home, with lot size, decade built,",
                         "\nmulti-unit and age-group controls; destination = ACS 2019-23 block-group median. Other rows: own group vs destination share (registered adults).")) + th
ggsave(file.path(FIG, "fig_sorting_movers.png"), g, width = 11, height = 5.2, dpi = 200); ggsave(file.path(FIG, "fig_sorting_movers.pdf"), g, width = 11, height = 5.2)
LAB <- c("20210101" = "2021-22", "20220517" = "2022-23", "20230101" = "2023-24\n(reference)", "20240514" = "2024-25\n(Helene)")
E <- copy(ES)[, wl := factor(LAB[window], levels = LAB)][, panel := sprintf("%s\nflooded movers in 2024-25: %s", outcome, format(n_flooded_helene, big.mark = ","))]
g2 <- ggplot(E, aes(wl, est)) + geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) + geom_vline(xintercept = 3.5, colour = muted, linetype = "dashed", linewidth = 0.4) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se, colour = window == STORM), width = 0, linewidth = 0.8) + geom_point(aes(colour = window == STORM), size = 2.6) +
  scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = hel), guide = "none") + facet_wrap(~panel, scales = "free_y", ncol = 2) +
  labs(x = "16-month window (starting year)", y = "Flooded minus unflooded movers", title = "Where do flooded movers go? Destination relative to origin neighbourhood",
       subtitle = "Movers to a new block group; flooded x window coefficients, origin block group x window FE, 95% intervals clustered by tract; reference 2023-24.") +
  th + theme(panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = grid))
ggsave(file.path(FIG, "fig_sorting_movers_es.png"), g2, width = 10, height = 7.2, dpi = 200); ggsave(file.path(FIG, "fig_sorting_movers_es.pdf"), g2, width = 10, height = 7.2)
cat("\nfigures written\n")
