# Helene: who flooded adults live with afterwards (exploratory, 2026-11-20).
# Unit: registered adult at baseline, address within 500 m of the flood boundary, baseline household of one to six adults.
# Inputs from 37_build_coresidents.py. Specification: y ~ flooded x window | address FE + block group x window FE,
# SEs clustered by tract. Usage: Rscript code/38_coresidents.R
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
           CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END AS age, c.sex_code AS sex, k.* EXCLUDE (pid), '%s' AS cohort
    FROM %s c JOIN %s h USING (pid) JOIN %s k USING (pid)
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0
      AND abs(c.he_dist) <= 500 AND c.county_id IN (%s) AND h.hh_n BETWEEN 1 AND 6",
    b, pq("cohort", b), pq("hh", b), pq("cor", b), paste0("'", study, "'", collapse = ","))))))
dbDisconnect(con, shutdown = TRUE)
D[, loc := .GRP, by = .(lat, lon)][, bgc := .GRP, by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, c("lat", "lon", "bg") := NULL][, post := as.integer(cohort == STORM)][, fp := flooded * post]
D[, big := as.integer(!is.na(n_f) & n_f > 10)]
D[, `:=`(joined = as.integer(moved == 1 & n_inc >= 1),                           # moved in with adults already registered there
         j_old = as.integer(moved == 1 & inc_old18 >= 1), j_yng = as.integer(moved == 1 & inc_yng18 >= 1),
         j_sim = as.integer(moved == 1 & inc_sim >= 1), j_partner = as.integer(moved == 1 & inc_partner >= 1),
         j_65 = as.integer(moved == 1 & inc_65 >= 1), j_diffrace = as.integer(moved == 1 & n_inc >= 1 & inc_same == 0),
         hosted = as.integer(moved == 0 & obs == 1 & n_new >= 1),                 # stayed, and someone new is registered at the address
         alone_f = as.integer(obs == 1 & n_f == 1), size_f = fifelse(obs == 1 & big == 0, as.numeric(n_f), NA_real_), mv = moved)]
cat("adult-windows:", nrow(D), " storm window flooded:", D[fp == 1, .N], " unflooded:", D[post == 1 & flooded == 0, .N],
    " flooded living alone at baseline:", D[fp == 1 & hh_n == 1, .N], "\n")
tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)] }
es <- function(v, P, lab, scale = 100) { P <- copy(P)[!is.na(get(v))][, yy := scale * get(v)]
  m <- feols(as.formula(sprintf("yy ~ i(cohort, flooded, ref = '%s') | loc + bgc", REF)), P, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, term := sub("cohort::([0-9]+):flooded", "\\1", term)]
  data.table(outcome = lab, pre1 = o[term == "20210101", est], pre1_se = o[term == "20210101", se], pre2 = o[term == "20220517", est],
             pre2_se = o[term == "20220517", se], pre_joint_p = wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]],
             storm = o[term == STORM, est], storm_se = o[term == STORM, se], storm_p = o[term == STORM, p],
             mean_unflooded_storm = P[post == 1 & flooded == 0, mean(yy)], n_flooded_storm = P[fp == 1, .N], n_unflooded_storm = P[post == 1 & flooded == 0, .N]) }
Y <- c(mv = "Moved to another address", joined = "Moved in with adults already living there", j_old = "... with an adult 18+ years older",
       j_yng = "... with an adult 18+ years younger", j_sim = "... with an adult of similar age (within 18 years)",
       j_partner = "... with an adult of the other sex within 15 years", j_65 = "... with an adult aged 65 or over",
       j_diffrace = "... with no adult of the same race", hosted = "Stayed, and a new adult is registered at the address",
       alone_f = "Only registered adult at the address at follow-up", size_f = "Registered adults at the address at follow-up (number)")
SAMP <- list(c("All adults", "TRUE"), c("Lived alone at baseline", "hh_n == 1"), c("Household of two to six at baseline", "hh_n >= 2"),
             c("Women", "sex == 'F'"), c("Men", "sex == 'M'"), c("Under 35", "!is.na(age) & age < 35"), c("35 to 64", "!is.na(age) & age >= 35 & age < 65"),
             c("65 or over", "!is.na(age) & age >= 65"))
res <- rbindlist(lapply(SAMP, function(s) rbindlist(lapply(names(Y), function(v) es(v, D[eval(parse(text = s[2]))], Y[[v]], if (v == "size_f") 1 else 100)[, sample := s[1]]))))
fwrite(res, file.path(TAB, "coresidents_helene.csv"))
for (s in SAMP) { cat("\n==", s[1], "==\n"); print(res[sample == s[1], .(outcome, pre1 = round(pre1, 2), pre2 = round(pre2, 2), pre_p = round(pre_joint_p, 3), storm = round(storm, 2),
  storm_se = round(storm_se, 2), p = round(storm_p, 3), mean_unflooded = round(mean_unflooded_storm, 2), n_flooded = n_flooded_storm, n_unflooded = n_unflooded_storm)]) }
# descriptive: the adults that movers joined, storm window (cells below 10 suppressed)
J <- D[post == 1 & joined == 1]
d2 <- J[, .(adults_who_joined = .N, own_age = round(mean(age_f, na.rm = TRUE), 1), coresident_age = round(sum(inc_age_sum, na.rm = TRUE) / sum(inc_age_n), 1),
            coresident_women = round(100 * sum(inc_f) / sum(n_inc), 1), coresident_white = round(100 * sum(inc_white) / sum(n_inc), 1),
            coresident_same_race = round(100 * sum(inc_same) / sum(n_inc), 1), coresident_65plus = round(100 * sum(inc_65) / sum(n_inc), 1),
            coresidents_per_adult = round(mean(n_inc), 2)), by = .(flooded)][order(flooded)]
d2[adults_who_joined < 10, names(d2)[-1] := NA]
fwrite(d2, file.path(TAB, "coresidents_profile_helene.csv"))
cat("\n== adults who moved in with others, storm window: profile of the adults already living there (descriptive) ==\n"); print(d2)
