# Helene households: (A) persistence about 25 months after the storm, (B) heterogeneity by household type
# (exploratory, 2026-11-19). Unit: household of two to six registered adults at one address at baseline,
# address within 500 m of the flood boundary. Inputs from 20_build_addr_hh.py, 27_build_hhid.py, 31_build_hh_long.py.
# Specification as before: y ~ flooded x window | address FE + block group x window FE, SEs clustered by tract.
# Usage: Rscript code/32_hh_persist_hetero.R
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb); library(ggplot2) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"
TAB <- "output/tables"; FIG <- "output/figures/new_outcomes"
W16 <- list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
LONG <- c("20210101", "20220517", "20240514"); STORM <- "20240514"
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0",
                                 file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
pq <- function(stem, b) sprintf("read_parquet('%s')", file.path(PROC, sprintf("%s_helene_%s.parquet", stem, b)))
D <- rbindlist(lapply(W16, function(x) { b <- x[1]
  lj <- if (b %in% LONG) sprintf("LEFT JOIN %s l USING (pid)", pq("hhl", b)) else ""
  lc <- if (b %in% LONG) "max(l.split_l) AS split_l, max(l.together_l) AS together_l, max(l.home_l) AS home_l, max(l.nobs_l) AS nobs_l" else
    "NULL::INT AS split_l, NULL::INT AS together_l, NULL::INT AS home_l, NULL::INT AS nobs_l"
  as.data.table(dbGetQuery(con, sprintf("
    SELECT any_value(c.lat) AS lat, any_value(c.lon) AS lon, any_value(substr(c.geoid,1,12)) AS bg,
           max((c.he_fl > 0.5)::INT) AS flooded, max(h.split) AS split, max(h.together) AS together,
           count(*) AS members, max(h.hh_n) AS hh_n, avg(CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END) AS age,
           avg((c.race_code = 'W')::INT) AS white, avg((c.sex_code = 'F')::INT) AS female, max(c.has_unit::INT) AS unit,
           any_value(t.htype) AS htype, max(t.old65) AS old65, %s, '%s' AS cohort
    FROM %s c JOIN %s h USING (pid) JOIN %s i USING (pid) JOIN %s t USING (pid) %s
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0
      AND abs(c.he_dist) <= 500 AND c.county_id IN (%s) AND h.hh_n BETWEEN 2 AND 6
    GROUP BY i.hb", lc, b, pq("cohort", b), pq("hh", b), pq("hhid", b), pq("hht", b), lj, paste0("'", study, "'", collapse = ","))))
}))
dbDisconnect(con, shutdown = TRUE)
D[, loc := .GRP, by = .(lat, lon)][, bgc := .GRP, by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, c("lat", "lon", "bg") := NULL]
D[, post := as.integer(cohort == STORM)][, fp := flooded * post]
cat("household-windows:", nrow(D), " storm window flooded:", D[post == 1 & flooded == 1, .N], " unflooded:", D[post == 1 & flooded == 0, .N], "\n")
CTRL <- " + i(cohort, members) + i(cohort, age) + i(cohort, white) + i(cohort, female) + i(cohort, unit)"
tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)] }

# ============================ A. persistence ====================================================
L <- D[cohort %in% LONG & !is.na(split_l)]
YL <- c(split_l = "Household split", together_l = "Household moved together",
        home_l = "All still at original address")
resA <- rbindlist(lapply(names(YL), function(v) rbindlist(lapply(c("baseline", "household controls"), function(s) {
  P <- if (s == "baseline") L else L[!is.na(age)]; P[, yy := 100 * get(v)]
  m <- feols(as.formula(paste0("yy ~ i(cohort, flooded, ref = '20220517')", if (s == "baseline") "" else CTRL, " | loc + bgc")), P, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, term := sub("cohort::(\\d+):flooded", "\\1", term)]
  o <- rbind(o, data.table(term = "20220517", est = 0, se = 0, p = NA))
  o[, `:=`(outcome = YL[[v]], spec = s, base_mean = P[cohort == "20220517" & flooded == 1, mean(yy)],
           unflooded_storm_mean = P[post == 1 & flooded == 0, mean(yy)], n_obs = nobs(m),
           n_flooded_storm = P[post == 1 & flooded == 1, .N], n_unflooded_storm = P[post == 1 & flooded == 0, .N])]
}))))
fwrite(resA, file.path(TAB, "hh_persist_helene.csv"))
options(width = 220); print(resA[term == STORM, .(outcome, spec, est, se, p, unflooded_storm_mean, n_flooded_storm, n_unflooded_storm)])
# where storm-window households stood at about 12 months and at about 25 months after the storm
S <- D[post == 1 & !is.na(split_l)]
S[, s12 := fifelse(split == 1, "split", fifelse(together == 1, "moved together", "neither"))]
S[, s25 := fifelse(split_l == 1, "split", fifelse(together_l == 1, "moved together", fifelse(home_l == 1, "all at original address", "other (fewer than two members observed)")))]
tr <- S[, .N, by = .(flooded, s12, s25)][order(flooded, s12, s25)]
tr[, share_of_s12 := round(100 * N / sum(N), 1), by = .(flooded, s12)]
tr[N < 10, `:=`(N = NA_integer_, share_of_s12 = NA_real_)]      # cells below 10 suppressed
fwrite(tr, file.path(TAB, "hh_persist_transitions_helene.csv")); print(tr)

# ============================ B. heterogeneity ==================================================
D[, `:=`(g_type = fifelse(htype == "couple", "Couple", fifelse(htype == "multigen", "Two generations (age gap 18+)", "Other same-generation")),
         g_size = fifelse(hh_n == 2, "Two members", "Three to six members"),
         g_unit = fifelse(unit == 1, "Address with unit number", "Address without unit number"),
         g_old = fifelse(old65 == 1, "Oldest member 65 or over", "Oldest member under 65"))]
Y <- c(split = "Household split", together = "Household moved together")
resB <- rbindlist(lapply(c("g_type", "g_size", "g_unit", "g_old"), function(gg) rbindlist(lapply(names(Y), function(v) {
  D[, `:=`(yy = 100 * get(v), g = get(gg))]
  m <- feols(yy ~ i(g, fp) | loc + bgc + g^cohort, D, cluster = ~tract)
  o <- tidy(m, ":fp$")[, group := sub("^g::(.*):fp$", "\\1", term)]
  lv <- sort(unique(D$g))
  m2 <- feols(as.formula(sprintf("yy ~ fp + i(g, fp, ref = '%s') | loc + bgc + g^cohort", lv[1])), D, cluster = ~tract)
  d2 <- tidy(m2, "^g::")[, group := sub("^g::(.*):fp$", "\\1", term)]
  o <- merge(o, d2[, .(group, diff_vs_first = est, diff_se = se, diff_p = p)], by = "group", all.x = TRUE)
  n <- D[post == 1, .(n_flooded_storm = sum(flooded), n_unflooded_storm = sum(1 - flooded),
                      unflooded_storm_mean = mean(yy[flooded == 0])), by = .(group = g)]
  merge(o, n, by = "group")[, `:=`(dimension = gg, outcome = Y[[v]], first_group = lv[1], term = NULL)]
}))))
fwrite(resB, file.path(TAB, "hh_hetero_helene.csv"))
print(resB[, .(dimension, outcome, group, est, se, p, diff_p, unflooded_storm_mean, n_flooded_storm, n_unflooded_storm)])
# event study by household type
REFS <- paste(unique(D$g_type), "20230101", sep = "|")
esB <- rbindlist(lapply(names(Y), function(v) { D[, `:=`(yy = 100 * get(v), tc = paste(g_type, cohort, sep = "|"))]
  m <- feols(yy ~ i(tc, flooded, ref = REFS) | loc + bgc + g_type^cohort, D, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, c("group", "term") := tstrsplit(sub("^tc::(.*):flooded$", "\\1", term), "|", fixed = TRUE)]
  rbind(o, data.table(term = "20230101", est = 0, se = 0, p = NA, group = unique(D$g_type)))[, outcome := Y[[v]]] }))
fwrite(esB, file.path(TAB, "hh_hetero_es_helene.csv"))

# ============================ figures ===========================================================
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"; COL <- "#eb6834"
th <- theme_minimal(base_size = 9) + theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
  panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3), axis.line.x = element_line(colour = INK2, linewidth = 0.3),
  axis.text = element_text(colour = INK2), strip.text = element_text(colour = INK, face = "bold", hjust = 0, size = 8.5),
  legend.position = "none", plot.title = element_text(colour = INK, face = "bold", size = 10),
  plot.caption = element_text(colour = INK2, hjust = 0, size = 6.9, lineheight = 1.05), plot.caption.position = "plot",
  panel.spacing = unit(1.1, "lines"), plot.margin = margin(6, 10, 6, 6))
wrap <- function(x, n = 150) paste(strwrap(x, n), collapse = "\n")
sv <- function(p, name, w, h) for (e in c("pdf", "png")) tryCatch(
  if (e == "pdf") ggsave(file.path(FIG, paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf)
  else ggsave(file.path(FIG, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white"),
  error = function(err) message("not written (file in use?): ", name, ".", e))
pts <- function(d) list(geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3),
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), data = d[se > 0], width = 0, linewidth = 0.5, colour = COL),
  geom_point(aes(fill = post), shape = 21, size = 2.4, stroke = 0.7, colour = COL),
  geom_text(data = d[se == 0], aes(label = "ref."), colour = INK2, size = 2.5, vjust = -1.2),
  scale_fill_manual(values = c(`FALSE` = "white", `TRUE` = COL)), scale_x_discrete(limits = levels(d$window)))
# A
WL <- c("20210101" = "Jan 2021 to\nSep 2023", "20220517" = "May 2022 to\nMay 2024", "20240514" = "May 2024 to\nNov 2026")
a <- resA[spec == "baseline"]; a[, `:=`(window = factor(WL[term], levels = WL), post = term == STORM, outcome = factor(outcome, levels = YL))]
n1 <- a[1, n_flooded_storm]; n0 <- a[1, n_unflooded_storm]
capA <- paste(wrap(paste("Coefficients on flooded address x window, relative to the last window that ends before the storm; 95% confidence intervals, standard",
  "errors clustered by tract. Hollow markers: windows ending before the storm. Filled marker: window from May 2024 to November 2026, which ends about 25 months",
  "after the storm (26-28 Sept 2024). Windows differ in length (32, 24 and 30 months); block group x window fixed effects absorb the common effect of length.",
  "Unit: household of two to six registered adults at one address at the start of the window, within 500 m of the flood boundary. Address fixed effects.",
  "Split: members registered at more than one address. Moved together: all members at one new address. Flooded: address inside the state inundation layer for Helene.")),
  wrap(sprintf("Sample sizes in the storm window: %s flooded households (treated), %s unflooded households (control); %s household-windows in all.",
               format(n1, big.mark = ","), format(n0, big.mark = ","), format(a[1, n_obs], big.mark = ","))), sep = "\n")
sv(ggplot(a, aes(window, est)) + pts(a) + facet_wrap(~outcome, scales = "free_y") +
     labs(title = "Helene: households about 25 months after the storm (flooded minus unflooded)", x = "Window (start to follow-up)",
          y = "Percentage points", caption = capA) + th, "es_helene_household_25_months", 7.6, 4.4)
# B
WS <- c("20210101" = "2021-22", "20220517" = "2022-23", "20230101" = "2023-24", "20240514" = "2024-25")
b <- copy(esB); b[, `:=`(window = factor(WS[term], levels = WS), post = term == STORM, outcome = factor(outcome, levels = Y))]
nt <- resB[dimension == "g_type" & outcome == Y[[1]]]
capB <- paste(wrap(paste("Coefficients on flooded address x window for each household type, relative to the last pre-storm window; 95% confidence intervals, standard",
  "errors clustered by tract. Hollow markers: pre-storm windows. Filled marker: window containing the storm (May 2024 to Sept 2025). One regression per outcome with",
  "address, block group x window and household type x window fixed effects. Unit: household of two to six registered adults at one address, within 500 m of the flood",
  "boundary. Couple: two members, one woman and one man, at most 15 years apart. Two generations: oldest and youngest member at least 18 years apart.",
  "Other: remaining households of similar ages. Types are inferred from age and sex; the data do not record family relationships.")),
  wrap(paste0("Sample sizes in the storm window. ", paste(sprintf("%s: %s flooded (treated), %s unflooded (control)", nt$group,
       format(nt$n_flooded_storm, big.mark = ","), format(nt$n_unflooded_storm, big.mark = ",")), collapse = "; "), ".")), sep = "\n")
sv(ggplot(b, aes(window, est)) + pts(b) + facet_grid(outcome ~ group, scales = "free_y") +
     labs(title = "Helene: household split and joint relocation by household type (flooded minus unflooded)", x = "Window (about 16 months)",
          y = "Percentage points", caption = capB) + th + theme(strip.text.y = element_text(angle = 270, face = "plain")),
   "es_helene_household_by_type", 7.8, 5.4)
