# Helene: effect of flooding on living only with adults of the same sex (exploratory, 2026-11-20).
# Unit: registered adult at baseline with recorded sex F or M, address within 500 m of the flood boundary, baseline
# household of one to six adults, observed at follow-up at an address with at most 10 registered adults.
# Reduced form: y ~ flooded x window | address FE + block group x window FE, SEs clustered by tract.
# IV: outcome on "the set of adults the person lives with changed", instrument flooded x storm window.
# Inputs from 37_build_coresidents.py and 41_build_sexmix.py. Usage: Rscript code/42_sexmix.R
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
           CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END AS age, k.obs, k.n_new, g.*  EXCLUDE (pid), '%s' AS cohort
    FROM %s c JOIN %s h USING (pid) JOIN %s k USING (pid) JOIN %s g USING (pid)
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0
      AND abs(c.he_dist) <= 500 AND c.county_id IN (%s) AND h.hh_n BETWEEN 1 AND 6",
    b, pq("cohort", b), pq("hh", b), pq("cor", b), pq("sx", b), paste0("'", study, "'", collapse = ","))))))
dbDisconnect(con, shutdown = TRUE)
D[, loc := .GRP, by = .(lat, lon)][, bgc := .GRP, by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, c("lat", "lon", "bg") := NULL][, post := as.integer(cohort == STORM)][, fp := flooded * post]
S <- D[obs == 1 & sex %in% c("F", "M") & !is.na(f_n) & f_n <= 10]
S[, `:=`(own_b = fifelse(sex == "F", b_w, b_m), oth_b = fifelse(sex == "F", b_m, b_w), own_f = fifelse(sex == "F", f_w, f_m), oth_f = fifelse(sex == "F", f_m, f_w))]
S[, `:=`(same_b = as.integer(own_b >= 2 & oth_b == 0), same_f = as.integer(own_f >= 2 & oth_f == 0),       # lives with adults, all of own sex
         mixed_b = as.integer(oth_b >= 1), mixed_f = as.integer(oth_f >= 1), alone_f = as.integer(f_n == 1),
         changed = as.integer(n_new >= 1 | (f_n - n_new) < b_n))]
S[, `:=`(into_same = as.integer(same_b == 0 & same_f == 1), out_same = as.integer(same_b == 1 & same_f == 0),
         into_mixed = as.integer(mixed_b == 0 & mixed_f == 1), out_mixed = as.integer(mixed_b == 1 & mixed_f == 0))]
cat("adult-windows:", nrow(S), " storm window flooded:", S[fp == 1, .N], " unflooded:", S[post == 1 & flooded == 0, .N], "\n")
tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)] }
es <- function(v, P, lab) { P <- copy(P)[, yy := 100 * get(v)]
  m <- feols(as.formula(sprintf("yy ~ i(cohort, flooded, ref = '%s') | loc + bgc", REF)), P, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, term := sub("cohort::([0-9]+):flooded", "\\1", term)]
  data.table(outcome = lab, pre1 = o[term == "20210101", est], pre1_se = o[term == "20210101", se], pre2 = o[term == "20220517", est],
             pre2_se = o[term == "20220517", se], pre_joint_p = wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]],
             storm = o[term == STORM, est], storm_se = o[term == STORM, se], storm_p = o[term == STORM, p],
             mean_unflooded_storm = P[post == 1 & flooded == 0, mean(yy)], n_flooded_storm = P[fp == 1, .N], n_unflooded_storm = P[post == 1 & flooded == 0, .N]) }
Y <- c(changed = "The set of adults lived with changed", same_b = "Lived only with same-sex adults at baseline [placebo]", same_f = "Lives only with same-sex adults at follow-up",
       into_same = "Entered same-sex-only living", out_same = "Left same-sex-only living", mixed_f = "Lives with an adult of the other sex at follow-up",
       into_mixed = "Entered living with the other sex", out_mixed = "Left living with the other sex", alone_f = "Only registered adult at the address")
SAMP <- list(c("All adults", "TRUE"), c("Women", "sex == 'F'"), c("Men", "sex == 'M'"), c("Under 35", "!is.na(age) & age < 35"),
             c("35 to 64", "!is.na(age) & age >= 35 & age < 65"), c("65 or over", "!is.na(age) & age >= 65"))
res <- rbindlist(lapply(SAMP, function(s) rbindlist(lapply(names(Y), function(v) tryCatch(es(v, S[eval(parse(text = s[2]))], Y[[v]])[, sample := s[1]], error = function(e) NULL)))))
fwrite(res, file.path(TAB, "sexmix_helene.csv"))
for (s in SAMP) { cat("\n==", s[1], "(percentage points) ==\n"); print(res[sample == s[1], .(outcome, pre1 = round(pre1, 2), pre2 = round(pre2, 2), pre_p = round(pre_joint_p, 3),
  storm = round(storm, 2), storm_se = round(storm_se, 2), p = round(storm_p, 3), mean_unflooded = round(mean_unflooded_storm, 2), n_flooded = n_flooded_storm, n_unflooded = n_unflooded_storm)]) }
iv <- rbindlist(lapply(SAMP[1:3], function(s) { P <- S[eval(parse(text = s[2]))]
  rbindlist(lapply(c("into_same", "out_same", "into_mixed", "out_mixed"), function(v) { P[, yy := 100 * get(v)]
    m <- feols(yy ~ 1 | loc + bgc | changed ~ fp, P, cluster = ~tract)
    data.table(sample = s[1], outcome = Y[[v]], iv = coef(m)[["fit_changed"]], se = se(m)[["fit_changed"]],
               first_stage_F = fitstat(m, "ivf")[[1]]$stat, n_flooded = P[fp == 1, .N]) })) }))
fwrite(iv, file.path(TAB, "sexmix_iv_helene.csv"))
cat("\n== IV: per person whose co-residents change because of the flood (percentage points) ==\n"); print(iv[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
