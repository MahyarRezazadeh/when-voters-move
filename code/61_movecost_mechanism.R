# Helene: moving-cost mechanism test (2026-11-22). 2021-22 window (COVID period) excluded; reference 2023-24.
# Sample as 58_sorting_split_vs_family.R: adults 18+ in households of 2-6 adults, baseline address within 500 m of the
#   flood boundary, moved to a different block group; groups split from family / moved with family.
# Per-person moving-cost proxies (60_build_movecost.py):
#   share  = 1 / (1 + n_comove): share of the move's fixed cost borne by the person (n_comove = other baseline household
#            members at the same new address). 1 = moved alone.
#   ldist  = log(1 + km moved), straight line between geocoded addresses.
# (A) Cost proxies by group, flooded and window: flooded x window event study within each group (origin bg FE + window FE),
#     and split minus with-family gap.
# (B) Mechanism: does sorting strength fall with per-person cost? Destination share ~ own x share + own x ldist, all movers.
# (C) Does the split vs with-family sorting gap after Helene (flooded movers; 58) shrink once own x cost proxies are
#     controlled? Compare own x split x Helene with and without own x share and own x ldist.
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb); library(ggplot2) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 220)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; TAB <- "output/tables"; FIG <- "output/figures"
WIN <- list(c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
STORM <- "20240514"; REF <- "20230101"
sink(file("logs/61_movecost_mechanism.log", open = "wt"), split = TRUE)
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0", file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
D <- rbindlist(lapply(WIN, function(w) as.data.table(dbGetQuery(con, sprintf("
  SELECT (c.he_fl > 0.5)::INT AS flooded, CASE WHEN c.age BETWEEN 18 AND 110 THEN c.age END AS age, trim(c.race_code) AS race,
         trim(c.ethnic_code) AS eth, trim(c.party_cd) AS party, k.bg_b, k.bg_f, m.dist_km, m.n_comove,
         CASE WHEN h.together = 1 THEN 'together' WHEN h.split = 1 THEN 'split' END AS grp, '%s' AS cohort
  FROM read_parquet('%s/cohort_helene_%s.parquet') c JOIN read_parquet('%s/hh_helene_%s.parquet') h USING (pid)
       JOIN read_parquet('%s/nb_helene_%s.parquet') k USING (pid) JOIN read_parquet('%s/mc_helene_%s.parquet') m USING (pid)
  WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0 AND abs(c.he_dist) <= 500
    AND c.county_id IN (%s) AND h.hh_n BETWEEN 2 AND 6 AND c.addr_%s AND k.bg_f IS NOT NULL AND k.bg_f <> k.bg_b
    AND (h.together = 1 OR h.split = 1)",
  w[1], PROC, w[1], PROC, w[1], PROC, w[1], PROC, w[1], paste0("'", study, "'", collapse = ","), w[2])))))
CB <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/bgcomp_%s.parquet')", PROC, d)))[, cohort := d]))
AG <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT bg, sum(n) AS na, sum(age_sum) AS asum FROM read_parquet('%s/bgage_%s.parquet') GROUP BY 1", PROC, d)))[, cohort := d]))
dbDisconnect(con, shutdown = TRUE)
NB <- merge(CB, AG, by = c("bg", "cohort"))[n >= 30, .(bg, cohort, sh_white = 100 * n_white / n, sh_black = 100 * n_black / n,
                                                       sh_dem = 100 * n_dem / n, sh_rep = 100 * n_rep / n, mage = asum / na)]
f <- copy(NB); nbv <- setdiff(names(f), c("bg", "cohort")); setnames(f, nbv, paste0(nbv, "_1")); setnames(f, "bg", "bg_f")
D <- merge(D[bg_b %in% NB$bg], f, by = c("bg_f", "cohort"))
D <- D[!is.na(age) & !is.na(grp) & !is.na(dist_km)]
D[, `:=`(white = as.integer(race == "W"), black = as.integer(race == "B"), dem = as.integer(party == "DEM"), rep = as.integer(party == "REP"),
         age10 = age / 10, agegrp = factor(fifelse(age < 35, "u35", fifelse(age >= 65, "o65", "m")), levels = c("m", "u35", "o65")),
         post = as.integer(cohort == STORM), split = as.integer(grp == "split"),
         share = 1 / (1 + n_comove), ldist = log1p(dist_km), alone = as.integer(n_comove == 0))]
D[, `:=`(fp = flooded * post, sp = split * post, tract = as.integer(factor(substr(bg_b, 1, 11))))]
tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`, n = nobs(m))] }

# ---- (A) descriptive and event study of the cost proxies
desc <- D[, .(n = .N, share = mean(share), moved_alone = mean(alone), co_movers = mean(n_comove), median_km = median(dist_km), mean_ldist = mean(ldist)),
          by = .(grp, flooded, window = fifelse(post == 1, "2024-25 (Helene)", "2022-24"))][order(grp, flooded, window)][n < 10, (3:7) := NA]
cat("== (A) per-person moving-cost proxies (means) ==\n"); print(desc)
fwrite(desc, file.path(TAB, "movecost_desc_helene.csv"))
AE <- rbindlist(lapply(c("share", "ldist"), function(v) rbindlist(lapply(c("together", "split"), function(g) {
  m <- feols(as.formula(sprintf("%s ~ i(cohort, flooded, ref = REF) + i(agegrp) | bg_b + cohort", v)), D[grp == g], cluster = ~tract)
  tidy(m, ":flooded$")[, `:=`(proxy = v, group = g, window = sub("cohort::([0-9]+):flooded", "\\1", term))] }))))
AG2 <- rbindlist(lapply(c("share", "ldist"), function(v) {
  m <- feols(as.formula(sprintf("%s ~ split + i(cohort, split, ref = REF) + i(agegrp) | bg_b + cohort", v)), D[flooded == 1], cluster = ~tract)
  rbind(tidy(m, "^split$")[, window := "all (level)"], tidy(m, ":split$")[, window := sub("cohort::([0-9]+):split", "\\1", term)])[, `:=`(proxy = v, group = "flooded: split minus with family")] }))
A <- rbind(AE, AG2, fill = TRUE)
cat("\n== (A) flooded x window by group, and split minus with-family among flooded movers ==\n")
print(A[, .(proxy, group, window, est = round(est, 3), se = round(se, 3), p = signif(p, 2), n)])
fwrite(A, file.path(TAB, "movecost_es_helene.csv"))

# ---- (B) does sorting strength fall with per-person moving cost? (all movers)
SPEC <- list(c("Party: share Democrat", "sh_dem_1", "dem"), c("Party: share Republican", "sh_rep_1", "rep"),
             c("Race: share White", "sh_white_1", "white"), c("Race: share Black", "sh_black_1", "black"), c("Age: mean age", "mage_1", "age10"))
B <- rbindlist(lapply(SPEC, function(s) {
  ctl <- if (s[3] == "age10") "" else "+ i(agegrp)"
  m <- feols(as.formula(sprintf("%s ~ %s + %s:share + %s:ldist + share + ldist + split %s | bg_b + cohort", s[2], s[3], s[3], s[3], ctl)), D, cluster = ~tract)
  tidy(m, sprintf("^%s(:share|:ldist)?$", s[3]))[, dimension := s[1]] }))
cat("\n== (B) sorting slope and its change with per-person cost (all movers within 500 m) ==\n")
print(B[, .(dimension, term, est = round(est, 3), se = round(se, 3), p = signif(p, 2), n)])
fwrite(B, file.path(TAB, "movecost_sorting_helene.csv"))

# ---- (C) flooded movers: split x Helene sorting gap with and without own x cost controls
Cc <- rbindlist(lapply(SPEC, function(s) {
  d <- copy(D[flooded == 1]); d[, xs := get(s[3]) * split]
  ctl <- if (s[3] == "age10") "" else "+ i(agegrp)"
  f0 <- sprintf("%s ~ i(cohort, %s) + i(cohort, xs, ref = REF) + i(cohort, split, ref = REF) + split %s", s[2], s[3], ctl)
  m0 <- feols(as.formula(paste(f0, "| bg_b + cohort")), d, cluster = ~tract)
  m1 <- feols(as.formula(paste(f0, sprintf("+ %s:share + %s:ldist + share + ldist | bg_b + cohort", s[3], s[3]))), d, cluster = ~tract)
  rbind(tidy(m0, "20240514:xs$")[, spec := "no cost controls"], tidy(m1, "20240514:xs$")[, spec := "own x cost controls"])[, dimension := s[1]] }))
cat("\n== (C) flooded movers: split x Helene sorting gap (vs 2023-24), with and without own x cost controls ==\n")
print(Cc[, .(dimension, spec, est = round(est, 3), se = round(se, 3), p = signif(p, 2), n)])
fwrite(Cc, file.path(TAB, "movecost_split_gap_helene.csv"))

# ---- figure: cost proxies by group (flooded x window)
ink <- "#1f2a44"; muted <- "#6b7280"; grid <- "#e5e7eb"; hel <- "#2563eb"
LAB <- c("20220517" = "2022-23", "20230101" = "2023-24\n(ref.)", "20240514" = "2024-25\n(Helene)")
P <- rbind(AE, unique(AE[, .(proxy, group)])[, `:=`(window = REF, est = 0, se = 0)], fill = TRUE)
P[, `:=`(wl = factor(LAB[window], levels = LAB), pl = fifelse(proxy == "share", "Share of the move's fixed cost borne (1 = moved alone)", "log(1 + km moved)"),
         gl = fifelse(group == "split", "Split from family", "Moved with family"))]
g <- ggplot(P, aes(wl, est, colour = window == STORM)) + geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0, linewidth = 0.7) + geom_point(size = 2) +
  scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = hel), guide = "none") + facet_grid(pl ~ gl, scales = "free_y", labeller = label_wrap_gen(30)) +
  labs(x = "16-month window (starting year)", y = "Flooded minus unflooded movers", title = "Per-person moving-cost proxies: flooded vs. unflooded movers",
       subtitle = "Adults 18+, households of 2-6, moved to a new block group, within 500 m of the flood boundary. Origin block group FE and window FE;\ntract-clustered 95% intervals; reference 2023-24; 2021-22 (COVID period) excluded.") +
  theme_minimal(base_size = 10) + theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(), panel.grid.major.y = element_line(colour = grid),
  plot.title = element_text(colour = ink, face = "bold"), plot.subtitle = element_text(colour = muted, size = 8.5), strip.text = element_text(hjust = 0, colour = ink),
  plot.title.position = "plot", plot.background = element_rect(fill = "white", colour = NA))
ggsave(file.path(FIG, "fig_movecost_es.png"), g, width = 9, height = 6, dpi = 200); ggsave(file.path(FIG, "fig_movecost_es.pdf"), g, width = 9, height = 6)
cat("\nfigures written\n"); sink()
