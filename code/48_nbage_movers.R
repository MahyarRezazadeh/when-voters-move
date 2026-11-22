# Helene: age sorting among adults who MOVED (exploratory, 2026-11-21).
# Sample: registered adults at baseline, address within 500 m of the flood boundary, baseline household of one to
#   six adults, who changed address by follow-up (addr_f), geocoded at follow-up, origin and destination block groups
#   with 30+ registered adults. Four 16-month windows; window starting May 2024 contains Helene; reference = Jan 2023.
# Question: conditional on moving, do flooded movers choose destination neighbourhoods whose age mix is closer to (or
#   farther from) their own age than unflooded movers do, relative to movers from the same addresses' areas before?
# Neighbourhood age measured at the START of the window (so the person's own move does not change it).
# Event study: y ~ i(window, flooded, ref = Jan 2023) + flooded | block group x window FE, SEs clustered by tract.
#   Robustness: add address FE (identified only from addresses with movers in several windows).
# Inputs: cohort/hh/nb_helene_<b>.parquet, bgage_<d>.parquet (45_build_nbage.py). Usage: Rscript code/48_nbage_movers.R
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb); library(ggplot2) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 230)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; TAB <- "output/tables"; FIG <- "output/figures"
WIN <- list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
STORM <- "20240514"; REF <- "20230101"
DATES <- c("20210101", "20220517", "20230101", "20230912", "20240514", "20250909")
LAB <- c("20210101" = "2021-22", "20220517" = "2022-23", "20230101" = "2023-24\n(reference)", "20240514" = "2024-25\n(Helene)")
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0",
                                 file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
pq <- function(stem, b) sprintf("read_parquet('%s')", file.path(PROC, sprintf("%s_helene_%s.parquet", stem, b)))
BG <- rbindlist(lapply(DATES, function(d) as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s')", file.path(PROC, sprintf("bgage_%s.parquet", d)))))[, d := d]))
D <- rbindlist(lapply(WIN, function(w) as.data.table(dbGetQuery(con, sprintf("
    SELECT c.lat, c.lon, substr(c.geoid,1,12) AS bg, (c.he_fl > 0.5)::INT AS flooded, c.addr_%s AS moved, c.obs_%s AS obs,
           CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END AS age, k.bg_b, k.bg_f, '%s' AS cohort
    FROM %s c JOIN %s h USING (pid) JOIN %s k USING (pid)
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0
      AND abs(c.he_dist) <= 500 AND c.county_id IN (%s) AND h.hh_n BETWEEN 1 AND 6",
    w[2], w[2], w[1], pq("cohort", w[1]), pq("hh", w[1]), pq("nb", w[1]), paste0("'", study, "'", collapse = ","))))))
dbDisconnect(con, shutdown = TRUE)

N <- BG[, .(nb_n = sum(n), nb_age = sum(age_sum) / sum(n), nb_65 = 100 * sum(n_65) / sum(n), nb_u35 = 100 * sum(n_u35) / sum(n)), by = .(bgk = bg, cohort = d)][nb_n >= 30]
D <- merge(D, setnames(copy(N), c("bgk", "nb_n", "nb_age", "nb_65", "nb_u35"), c("bg_b", "n0", "age0", "s65_0", "su35_0")), by = c("bg_b", "cohort"), all.x = TRUE)
D <- merge(D, setnames(copy(N), c("bgk", "nb_n", "nb_age", "nb_65", "nb_u35"), c("bg_f", "n1", "age1", "s65_1", "su35_1")), by = c("bg_f", "cohort"), all.x = TRUE)
D[, loc := .GRP, by = .(lat, lon)][, bgc := .GRP, by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]

cat("== counts by window: flooded adults, flooded movers, flooded movers in sample (geocoded, both neighbourhoods 30+) ==\n")
D[, insample := as.integer(moved %in% TRUE & !is.na(age) & !is.na(age0) & !is.na(age1))]
cnt <- D[, .(adults = .N, movers = sum(moved %in% TRUE), movers_in_sample = sum(insample)), by = .(cohort, flooded)][order(cohort, flooded)]
print(cnt)
S <- D[insample == 1]
S[, `:=`(newbg = as.integer(bg_f != bg_b), d_age = age1 - age0, gap0 = abs(age - age0), gap1 = abs(age - age1),
         own1 = fifelse(age < 35, su35_1, fifelse(age >= 65, s65_1, NA_real_)), own0 = fifelse(age < 35, su35_0, fifelse(age >= 65, s65_0, NA_real_)))]
S[, `:=`(d_gap = gap1 - gap0, closer = as.integer(gap1 < gap0 - 2), farther = as.integer(gap1 > gap0 + 2), d_own = own1 - own0,
         older_nb = as.integer(age1 > age0 + 2), younger_nb = as.integer(age1 < age0 - 2))]
S[, net_closer := closer - farther]

tidy <- function(m) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl("^cohort::.*:flooded$", term)]
  ct[, .(window = sub("cohort::([0-9]+):flooded", "\\1", term), est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)] }
es <- function(v, P, lab, scale, spec) { P <- copy(P)[!is.na(get(v))][, yy := scale * get(v)]
  f <- if (spec == "bg x window FE") sprintf("yy ~ i(cohort, flooded, ref = '%s') + flooded | bgc", REF) else
    sprintf("yy ~ i(cohort, flooded, ref = '%s') | loc + bgc", REF)
  m <- feols(as.formula(f), P, cluster = ~tract)
  o <- tidy(m); o <- rbind(o, data.table(window = REF, est = 0, se = 0, p = NA_real_))
  o[, `:=`(outcome = lab, spec = spec, pre_joint_p = wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]],
           mean_unflooded_storm = P[cohort == STORM & flooded == 0, mean(yy)], n_flooded_storm = P[cohort == STORM & flooded == 1, .N],
           n_unflooded_storm = P[cohort == STORM & flooded == 0, .N], n_obs_used = nobs(m))] }
Y <- list(c("d_gap", "Change in distance between own age and neighbourhood mean age (years)", 1),
          c("closer", "Moved to a neighbourhood 2+ years closer to own age (%)", 100),
          c("farther", "Moved to a neighbourhood 2+ years farther from own age (%)", 100),
          c("net_closer", "Closer minus farther (points)", 100),
          c("d_age", "Change in neighbourhood mean age (years)", 1),
          c("d_own", "Change in neighbourhood share of own age group (points; under 35 and 65+ only)", 1),
          c("older_nb", "Moved to a neighbourhood 2+ years older (%)", 100),
          c("younger_nb", "Moved to a neighbourhood 2+ years younger (%)", 100))
SAMP <- list(c("All movers", "TRUE"), c("Under 35", "age < 35"), c("35 to 64", "age >= 35 & age < 65"), c("65 or over", "age >= 65"))
res <- rbindlist(lapply(c("bg x window FE", "address + bg x window FE"), function(sp) rbindlist(lapply(SAMP, function(s)
  rbindlist(lapply(Y, function(y) tryCatch(es(y[1], S[eval(parse(text = s[2]))], y[2], as.numeric(y[3]), sp)[, sample := s[1]],
                                            error = function(e) { message(s[1], " / ", y[1], " / ", sp, ": ", conditionMessage(e)); NULL })))))))
fwrite(res, file.path(TAB, "nbage_movers_helene.csv")); fwrite(cnt, file.path(TAB, "nbage_movers_counts_helene.csv"))

# descriptive: raw means in the storm window
desc <- S[cohort == STORM, .(movers = .N, gap_before = mean(gap0), gap_after = mean(gap1), closer = 100 * mean(closer), farther = 100 * mean(farther),
                             nb_age_before = mean(age0), nb_age_after = mean(age1)), by = .(flooded, group = fifelse(age < 35, "Under 35", fifelse(age >= 65, "65 or over", "35 to 64")))][order(group, flooded)]
fwrite(desc, file.path(TAB, "nbage_movers_desc_helene.csv"))
cat("\n== storm window, raw means among movers ==\n"); print(desc[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
for (sp in unique(res$spec)) for (s in SAMP) { cat("\n==", sp, "|", s[1], "==\n")
  print(res[spec == sp & sample == s[1] & window != REF, .(outcome = substr(outcome, 1, 70), window, est = round(est, 3), se = round(se, 3), p = round(p, 3),
    pre_p = round(pre_joint_p, 3), mean_unfl = round(mean_unflooded_storm, 2), n_fl = n_flooded_storm, n_unfl = n_unflooded_storm)]) }

# ============================ figures ============================================================
ink <- "#1f2a44"; muted <- "#6b7280"; grid <- "#e5e7eb"; hel <- "#2563eb"
plot_es <- function(R, file, title, sub, ncol) {
  R <- copy(R)[, wl := factor(LAB[window], levels = LAB)][, storm := window == STORM]
  R[, panel := factor(sprintf("%s\nflooded movers in 2024-25: %s", wrap_lab(outcome), format(n_flooded_storm, big.mark = ",")), levels = unique(sprintf("%s\nflooded movers in 2024-25: %s", wrap_lab(outcome), format(n_flooded_storm, big.mark = ","))))]
  g <- ggplot(R, aes(wl, est)) + geom_hline(yintercept = 0, colour = muted, linewidth = 0.4) +
    geom_vline(xintercept = 3.5, colour = muted, linewidth = 0.4, linetype = "dashed") +
    geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se, colour = storm), width = 0, linewidth = 0.8) +
    geom_point(aes(colour = storm), size = 2.6) + scale_colour_manual(values = c(`FALSE` = ink, `TRUE` = hel), guide = "none") +
    facet_wrap(~panel, scales = "free_y", ncol = ncol) + labs(x = "16-month window (starting year)", y = "Flooded minus unflooded movers", title = title, subtitle = sub) +
    theme_minimal(base_size = 10) + theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(), panel.grid.major.y = element_line(colour = grid),
      strip.text = element_text(hjust = 0, size = 9, colour = ink), plot.title = element_text(colour = ink, face = "bold"), plot.subtitle = element_text(colour = muted, size = 8.5),
      axis.text = element_text(colour = muted), plot.title.position = "plot", plot.background = element_rect(fill = "white", colour = NA))
  ggsave(file.path(FIG, paste0(file, ".png")), g, width = 10, height = 3.3 * ceiling(uniqueN(R$panel) / ncol) + 1.1, dpi = 200)
  ggsave(file.path(FIG, paste0(file, ".pdf")), g, width = 10, height = 3.3 * ceiling(uniqueN(R$panel) / ncol) + 1.1) }
wrap_lab <- function(x) vapply(x, function(s) paste(strwrap(s, 48), collapse = "\n"), "")
SUB <- paste("Sample: adults who changed address within the 16-month window, address within 500 m of the Helene flood boundary.",
             "\nPoints: flooded x window coefficients, block group x window fixed effects, 95% intervals clustered by tract; reference window 2023-24.",
             "\nNeighbourhood = Census block group; its age mix is measured at the start of the window.")
M <- res[spec == "bg x window FE"]
plot_es(M[sample == "All movers" & outcome %in% c(Y[[1]][2], Y[[2]][2], Y[[3]][2], Y[[5]][2])], "fig_nbage_movers_es_all",
        "Do flooded movers choose neighbourhoods closer to their own age?", SUB, 2)
G <- rbind(M[sample == "Under 35" & outcome == Y[[8]][2]][, outcome := paste("Under 35:", outcome)],
           M[sample == "65 or over" & outcome == Y[[7]][2]][, outcome := paste("65 or over:", outcome)],
           M[sample == "Under 35" & outcome == Y[[1]][2]][, outcome := paste("Under 35:", outcome)],
           M[sample == "65 or over" & outcome == Y[[1]][2]][, outcome := paste("65 or over:", outcome)])
plot_es(G, "fig_nbage_movers_es_age", "Age sorting among flooded movers, by age group", SUB, 2)
cat("\nfigures written\n")
