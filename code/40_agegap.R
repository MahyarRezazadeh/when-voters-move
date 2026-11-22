# Helene: effect of flooding on the age gap among the registered adults a person lives with (exploratory, 2026-11-20).
# Unit: registered adult at baseline, address within 500 m of the flood boundary, baseline household of one to six adults,
# observed at follow-up at an address with at most 10 registered adults.
# Reduced form: y ~ flooded x window | address FE + block group x window FE, SEs clustered by tract.
# IV: change in age gap on "the set of adults the person lives with changed", instrument flooded x storm window.
# Inputs from 37_build_coresidents.py and 39_build_agegap.py. Usage: Rscript code/40_agegap.R
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 230)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; TAB <- "output/tables"
W16 <- c("20210101", "20220517", "20230101", "20240514"); STORM <- "20240514"; REF <- "20230101"
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0",
                                 file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
pq <- function(stem, b) sprintf("read_parquet('%s')", file.path(PROC, sprintf("%s_helene_%s.parquet", stem, b)))
D <- rbindlist(lapply(W16, function(b) as.data.table(dbGetQuery(con, sprintf("
    SELECT c.lat, c.lon, substr(c.geoid,1,12) AS bg, (c.he_fl > 0.5)::INT AS flooded, h.hh_n,
           CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END AS age, k.obs, k.moved, k.n_f, k.n_new,
           g.n_b, g.gap_b, g.gap_f, '%s' AS cohort
    FROM %s c JOIN %s h USING (pid) JOIN %s k USING (pid) JOIN %s g USING (pid)
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0
      AND abs(c.he_dist) <= 500 AND c.county_id IN (%s) AND h.hh_n BETWEEN 1 AND 6",
    b, pq("cohort", b), pq("hh", b), pq("cor", b), pq("gap", b), paste0("'", study, "'", collapse = ","))))))
dbDisconnect(con, shutdown = TRUE)
D[, loc := .GRP, by = .(lat, lon)][, bgc := .GRP, by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, c("lat", "lon", "bg") := NULL][, post := as.integer(cohort == STORM)][, fp := flooded * post]
D[n_b == 1 & is.na(gap_b), gap_b := 0][n_f == 1 & is.na(gap_f), gap_f := 0]
D[, insample := as.integer(obs == 1 & !is.na(n_f) & n_f <= 10 & !is.na(gap_f) & !is.na(gap_b))]
cat("adult-windows:", nrow(D), " storm window flooded:", D[fp == 1, .N], " unflooded:", D[post == 1 & flooded == 0, .N], "\n")
tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)] }
es <- function(v, P, lab, scale = 100) { P <- copy(P)[!is.na(get(v))][, yy := scale * get(v)]
  m <- feols(as.formula(sprintf("yy ~ i(cohort, flooded, ref = '%s') | loc + bgc", REF)), P, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, term := sub("cohort::([0-9]+):flooded", "\\1", term)]
  data.table(outcome = lab, pre1 = o[term == "20210101", est], pre1_se = o[term == "20210101", se], pre2 = o[term == "20220517", est],
             pre2_se = o[term == "20220517", se], pre_joint_p = wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]],
             storm = o[term == STORM, est], storm_se = o[term == STORM, se], storm_p = o[term == STORM, p],
             mean_unflooded_storm = P[post == 1 & flooded == 0, mean(yy)], n_flooded_storm = P[fp == 1, .N], n_unflooded_storm = P[post == 1 & flooded == 0, .N]) }
cat("\n== 0. is the person in the estimation sample (observed, address with at most 10 adults, ages recorded)? ==\n")
print(es("insample", D, "In sample")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
S <- D[insample == 1]
S[, `:=`(dgap = gap_f - gap_b, absd = abs(gap_f - gap_b), wider = as.integer(gap_f > gap_b), narrower = as.integer(gap_f < gap_b),
         multi_b = as.integer(gap_b >= 18), multi_f = as.integer(gap_f >= 18), same_f = as.integer(n_f >= 2 & gap_f < 18), alone_f = as.integer(n_f == 1),
         changed = as.integer(n_new >= 1 | (n_f - n_new) < n_b))]            # someone joined, or a baseline co-resident is no longer there
S[, `:=`(into_multi = as.integer(multi_b == 0 & multi_f == 1), out_multi = as.integer(multi_b == 1 & multi_f == 0))]
Y <- list(c("changed", "The set of adults lived with changed (%)", 100), c("gap_b", "Age gap at baseline (years) [placebo]", 1), c("gap_f", "Age gap at follow-up (years)", 1),
          c("dgap", "Change in age gap (years)", 1), c("absd", "Absolute change in age gap (years)", 1), c("wider", "Age gap became wider (%)", 100),
          c("narrower", "Age gap became narrower (%)", 100), c("multi_f", "Lives with an adult 18+ years apart at follow-up (%)", 100),
          c("out_multi", "Left a two-generation setting (%)", 100), c("into_multi", "Entered a two-generation setting (%)", 100),
          c("same_f", "Lives only with adults within 18 years (%)", 100), c("alone_f", "Only registered adult at the address (%)", 100))
SAMP <- list(c("All adults", "TRUE"), c("Baseline: lived alone", "n_b == 1"), c("Baseline: all adults within 18 years", "n_b >= 2 & gap_b < 18"),
             c("Baseline: adults 18+ years apart", "gap_b >= 18"), c("Under 35", "!is.na(age) & age < 35"), c("35 to 64", "!is.na(age) & age >= 35 & age < 65"),
             c("65 or over", "!is.na(age) & age >= 65"))
res <- rbindlist(lapply(SAMP, function(s) rbindlist(lapply(Y, function(y) tryCatch(es(y[1], S[eval(parse(text = s[2]))], y[2], as.numeric(y[3]))[, sample := s[1]], error = function(e) NULL)))))
fwrite(res, file.path(TAB, "agegap_helene.csv"))
for (s in SAMP) { cat("\n==", s[1], "==\n"); print(res[sample == s[1], .(outcome, pre1 = round(pre1, 2), pre2 = round(pre2, 2), pre_p = round(pre_joint_p, 3), storm = round(storm, 2),
  storm_se = round(storm_se, 2), p = round(storm_p, 3), mean_unflooded = round(mean_unflooded_storm, 2), n_flooded = n_flooded_storm, n_unflooded = n_unflooded_storm)]) }
# IV: per person whose co-residents change because of the flood
iv <- rbindlist(lapply(SAMP, function(s) { P <- S[eval(parse(text = s[2]))]
  rbindlist(lapply(c("dgap", "absd", "wider", "narrower", "out_multi", "into_multi"), function(v) { P[, yy := get(v) * (if (v %in% c("dgap", "absd")) 1 else 100)]
    tryCatch({ m <- feols(yy ~ 1 | loc + bgc | changed ~ fp, P, cluster = ~tract)
    data.table(sample = s[1], outcome = v, iv = coef(m)[["fit_changed"]], se = se(m)[["fit_changed"]], first_stage = coef(m$iv_first_stage$changed)[["fp"]],
               first_stage_F = fitstat(m, "ivf")[[1]]$stat, n_flooded = P[fp == 1, .N]) }, error = function(e) NULL) })) }))
fwrite(iv, file.path(TAB, "agegap_iv_helene.csv"))
cat("\n== IV: effect of a change in co-residents caused by the flood (years, or percentage points) ==\n"); print(iv[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
