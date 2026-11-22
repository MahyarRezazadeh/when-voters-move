# Helene area: do movers in general move toward neighbourhoods of similar age? (descriptive, not causal; 2026-11-21)
# Sample as 48_nbage_movers.R (movers, geocoded, both block groups 30+ adults), all four windows, flooded and unflooded.
# (a) raw means by age group; (b) destination neighbourhood mean age on own-age group, origin block group x window FE
#     (compares people leaving the same neighbourhood in the same window); (c) same with flooded x age-group interaction.
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 200)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; TAB <- "output/tables"
WIN <- list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
DATES <- c("20210101", "20220517", "20230101", "20230912", "20240514", "20250909")
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0", file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
BG <- rbindlist(lapply(DATES, function(d) as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/bgage_%s.parquet')", PROC, d)))[, d := d]))
D <- rbindlist(lapply(WIN, function(w) as.data.table(dbGetQuery(con, sprintf("
  SELECT substr(c.geoid,1,12) AS bg, (c.he_fl > 0.5)::INT AS flooded, CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END AS age,
         k.bg_b, k.bg_f, '%s' AS cohort
  FROM read_parquet('%s/cohort_helene_%s.parquet') c JOIN read_parquet('%s/hh_helene_%s.parquet') h USING (pid)
       JOIN read_parquet('%s/nb_helene_%s.parquet') k USING (pid)
  WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0 AND abs(c.he_dist) <= 500
    AND c.county_id IN (%s) AND h.hh_n BETWEEN 1 AND 6 AND c.addr_%s", w[1], PROC, w[1], PROC, w[1], PROC, w[1], paste0("'", study, "'", collapse = ","), w[2])))))
dbDisconnect(con, shutdown = TRUE)
N <- BG[, .(nb_n = sum(n), nb_age = sum(age_sum) / sum(n), s65 = 100 * sum(n_65) / sum(n), su35 = 100 * sum(n_u35) / sum(n)), by = .(bgk = bg, cohort = d)][nb_n >= 30]
D <- merge(D, N[, .(bg_b = bgk, cohort, age0 = nb_age, s65_0 = s65, su35_0 = su35)], by = c("bg_b", "cohort"))
D <- merge(D, N[, .(bg_f = bgk, cohort, age1 = nb_age, s65_1 = s65, su35_1 = su35)], by = c("bg_f", "cohort"))
D <- D[!is.na(age)][, grp := factor(fifelse(age < 35, "Under 35", fifelse(age >= 65, "65 or over", "35 to 64")), levels = c("35 to 64", "Under 35", "65 or over"))]
D[, `:=`(bgc = .GRP), by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, newbg := bg_f != bg_b]
cat("movers in sample:", nrow(D), " moved to a different block group:", D[, sum(newbg)], "\n")
cat("\n== (a) raw means, movers to a different block group ==\n")
A <- D[newbg == TRUE, .(movers = .N, nb_age_origin = mean(age0), nb_age_dest = mean(age1), change = mean(age1 - age0),
   share65_origin = mean(s65_0), share65_dest = mean(s65_1), shareu35_origin = mean(su35_0), shareu35_dest = mean(su35_1)), by = grp][order(grp)]
print(A[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
cat("\n== (b) destination outcome on age group (ref 35-64), origin block group x window FE, movers to a different block group ==\n")
X <- D[newbg == TRUE]
res <- rbindlist(lapply(c("age1", "s65_1", "su35_1"), function(v) { X[, yy := get(v)]
  m <- feols(yy ~ grp | bgc, X, cluster = ~tract)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[, .(outcome = v, term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`, n = nobs(m))] }))
print(res[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
m2 <- feols(age1 ~ age | bgc, X, cluster = ~tract)
cat("\nslope: destination mean age per 10 years of own age:", round(10 * coef(m2)["age"], 3), "(", round(10 * se(m2)["age"], 3), ")\n")
cat("\n== (c) does the pattern differ for flooded movers in the Helene window? ==\n")
X[, fp := as.integer(flooded == 1 & cohort == "20240514")]
m3 <- feols(age1 ~ grp + grp:fp | bgc, X, cluster = ~tract)
print(round(coeftable(m3), 3))
cat("flooded movers to new block group in Helene window by group:\n"); print(X[fp == 1, .N, by = grp])
fwrite(rbind(res, data.table(outcome = "age1", term = "own age x 10", est = 10 * coef(m2)["age"], se = 10 * se(m2)["age"], p = pvalue(m2)["age"], n = nobs(m2))),
       file.path(TAB, "movers_age_match_helene.csv"))
