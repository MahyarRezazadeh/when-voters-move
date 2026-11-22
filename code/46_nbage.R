# Helene: spatial sorting by age (exploratory, 2026-11-20).
# Version 1 (flood design). Unit: registered adult at baseline, address within 500 m of the flood boundary, baseline
#   household of one to six adults, geocoded at follow-up. Neighbourhood = Census block group; its age composition is
#   measured at the START of the window (registered adults), so it cannot be changed by the person's own move.
#   y ~ flooded x window | address FE + block group x window FE, SEs clustered by tract.
# Version 2 (no flood design). Dissimilarity index between adults under 35 and adults 65 or over across block groups,
#   by county and snapshot; counties with 1%+ of addresses flooded vs counties with none; Asheville shown separately.
# Inputs from 45_build_nbage.py. Usage: Rscript code/46_nbage.R
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 230)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; TAB <- "output/tables"
W16 <- c("20210101", "20220517", "20230101", "20240514"); STORM <- "20240514"; REF <- "20230101"
DATES <- c("20210101", "20220517", "20230101", "20230912", "20240514", "20250909")
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0",
                                 file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
pq <- function(stem, b) sprintf("read_parquet('%s')", file.path(PROC, sprintf("%s_helene_%s.parquet", stem, b)))
BG <- rbindlist(lapply(DATES, function(d) as.data.table(dbGetQuery(con, sprintf("SELECT *, '%s' AS d FROM read_parquet('%s')", d, file.path(PROC, sprintf("bgage_%s.parquet", d)))))))
D <- rbindlist(lapply(W16, function(b) as.data.table(dbGetQuery(con, sprintf("
    SELECT c.lat, c.lon, substr(c.geoid,1,12) AS bg, (c.he_fl > 0.5)::INT AS flooded, h.hh_n,
           CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END AS age, k.bg_b, k.bg_f, '%s' AS cohort
    FROM %s c JOIN %s h USING (pid) JOIN %s k USING (pid)
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0
      AND abs(c.he_dist) <= 500 AND c.county_id IN (%s) AND h.hh_n BETWEEN 1 AND 6",
    b, pq("cohort", b), pq("hh", b), pq("nb", b), paste0("'", study, "'", collapse = ","))))))
dbDisconnect(con, shutdown = TRUE)

# ============================ Version 1 ============================================================
N <- BG[, .(nb_n = sum(n), nb_age = sum(age_sum) / sum(n), nb_65 = 100 * sum(n_65) / sum(n), nb_u35 = 100 * sum(n_u35) / sum(n)), by = .(bgk = bg, cohort = d)][nb_n >= 30]
D <- merge(D, setnames(copy(N), c("bgk", "nb_n", "nb_age", "nb_65", "nb_u35"), c("bg_b", "n0", "age0", "s65_0", "su35_0")), by = c("bg_b", "cohort"), all.x = TRUE)
D <- merge(D, setnames(copy(N), c("bgk", "nb_n", "nb_age", "nb_65", "nb_u35"), c("bg_f", "n1", "age1", "s65_1", "su35_1")), by = c("bg_f", "cohort"), all.x = TRUE)
D[, loc := .GRP, by = .(lat, lon)][, bgc := .GRP, by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, post := as.integer(cohort == STORM)][, fp := flooded * post]
D[, insample := as.integer(!is.na(age) & !is.na(age0) & !is.na(age1))]
tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)] }
es <- function(v, P, lab, scale = 1) { P <- copy(P)[!is.na(get(v))][, yy := scale * get(v)]
  m <- feols(as.formula(sprintf("yy ~ i(cohort, flooded, ref = '%s') | loc + bgc", REF)), P, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, term := sub("cohort::([0-9]+):flooded", "\\1", term)]
  data.table(outcome = lab, pre1 = o[term == "20210101", est], pre1_se = o[term == "20210101", se], pre2 = o[term == "20220517", est],
             pre2_se = o[term == "20220517", se], pre_joint_p = wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]],
             storm = o[term == STORM, est], storm_se = o[term == STORM, se], storm_p = o[term == STORM, p],
             mean_unflooded_storm = P[post == 1 & flooded == 0, mean(yy)], n_flooded_storm = P[fp == 1, .N], n_unflooded_storm = P[post == 1 & flooded == 0, .N]) }
cat("== Version 1. in sample (geocoded at follow-up, neighbourhood with 30+ adults), percentage points ==\n")
print(es("insample", D, "In sample", 100)[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
S <- D[insample == 1]
S[, `:=`(newbg = as.integer(bg_f != bg_b), d_age = age1 - age0, d_65 = s65_1 - s65_0, d_u35 = su35_1 - su35_0,
         gap0 = abs(age - age0), gap1 = abs(age - age1), older_nb = as.integer(age1 > age0 + 2), younger_nb = as.integer(age1 < age0 - 2))]
S[, `:=`(d_gap = gap1 - gap0, closer = as.integer(gap1 < gap0 - 2), farther = as.integer(gap1 > gap0 + 2),
         own_share1 = fifelse(age < 35, su35_1, fifelse(age >= 65, s65_1, NA_real_)), own_share0 = fifelse(age < 35, su35_0, fifelse(age >= 65, s65_0, NA_real_)))]
S[, d_own := own_share1 - own_share0]
S[, net_closer := closer - farther]
Y <- list(c("newbg", "Lives in a different neighbourhood (%)", 100), c("d_age", "Change in neighbourhood mean age (years)", 1),
          c("older_nb", "Moved to a neighbourhood 2+ years older (%)", 100), c("younger_nb", "Moved to a neighbourhood 2+ years younger (%)", 100),
          c("d_65", "Change in neighbourhood share aged 65+ (points)", 1), c("d_u35", "Change in neighbourhood share under 35 (points)", 1),
          c("d_gap", "Change in |own age - neighbourhood mean age| (years)", 1), c("closer", "Neighbourhood 2+ years closer to own age (%)", 100),
          c("farther", "Neighbourhood 2+ years farther from own age (%)", 100), c("net_closer", "Closer minus farther (points)", 100), c("d_own", "Change in neighbourhood share of own age group (points; under 35 and 65+ only)", 1))
SAMP <- list(c("All adults", "TRUE"), c("Under 35 or 65 and over", "age < 35 | age >= 65"), c("Under 35", "age < 35"), c("35 to 64", "age >= 35 & age < 65"), c("65 or over", "age >= 65"))
res <- rbindlist(lapply(SAMP, function(s) rbindlist(lapply(Y, function(y) tryCatch(es(y[1], S[eval(parse(text = s[2]))], y[2], as.numeric(y[3]))[, sample := s[1]], error = function(e) NULL)))))
fwrite(res, file.path(TAB, "nbage_helene.csv"))
for (s in SAMP) { cat("\n==", s[1], "==\n"); print(res[sample == s[1], .(outcome, pre1 = round(pre1, 3), pre2 = round(pre2, 3), pre_p = round(pre_joint_p, 3), storm = round(storm, 3),
  storm_se = round(storm_se, 3), p = round(storm_p, 4), mean_unflooded = round(mean_unflooded_storm, 3), n_flooded = n_flooded_storm, n_unflooded = n_unflooded_storm)]) }

# ============================ Version 2 ============================================================
idx <- function(X) X[, .(n = sum(n), n_u35 = sum(n_u35), n_65 = sum(n_65), age_sum = sum(age_sum)), by = .(bg)][n >= 30,
  .(dissim = 100 * 0.5 * sum(abs(n_u35 / sum(n_u35) - n_65 / sum(n_65))), sd_age = sqrt(weighted.mean((age_sum / n - sum(age_sum) / sum(n))^2, n)), bgs = .N, adults = sum(n))]
C <- BG[, idx(.SD), by = .(county_id, d)]
ex <- fread("output/tables/helene_county_exposure.csv")[, .(county_id = as.character(cid), group)]
C[, county_id := as.character(as.integer(county_id))]; C <- merge(C, ex, by = "county_id")
A <- BG[as.integer(county_id) == 11 & muni == "ASH", idx(.SD), by = .(d)][, place := "Asheville (city)"]
B <- BG[as.integer(county_id) == 11, idx(.SD), by = .(d)][, place := "Buncombe County"]
G <- C[group != "partial", .(dissim = weighted.mean(dissim, adults), sd_age = weighted.mean(sd_age, adults), bgs = sum(bgs), adults = sum(adults)), by = .(d, place = fifelse(group == "treated", "13 flooded counties (mean)", "77 unflooded counties (mean)"))]
tr <- rbind(A, B, G)[order(place, d)]
fwrite(tr, file.path(TAB, "nbage_index_trend_helene.csv"))
cat("\n== Version 2. dissimilarity index, under 35 vs 65+ across block groups (0-100), and SD of block-group mean age (years) ==\n")
print(dcast(tr, place ~ d, value.var = "dissim")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 2) else x)])
print(dcast(tr, place ~ d, value.var = "sd_age")[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
P <- C[group != "partial"][, treated := as.integer(group == "treated")]
r2 <- rbindlist(lapply(c("dissim", "sd_age"), function(v) { P[, yy := get(v)]
  m <- feols(yy ~ i(d, treated, ref = "20240514") | county_id + d, P, cluster = ~county_id)
  tidy(m, ":treated$")[, `:=`(outcome = v, treated_counties = P[treated == 1, uniqueN(county_id)], control_counties = P[treated == 0, uniqueN(county_id)])] }))
fwrite(r2, file.path(TAB, "nbage_index_did_helene.csv"))
cat("\n== county event study: flooded vs unflooded counties, reference May 2024 ==\n"); print(r2[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
