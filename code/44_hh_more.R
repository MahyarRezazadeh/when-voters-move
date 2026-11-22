# Helene households: screen of further household-level outcomes (exploratory, 2026-11-20).
# Unit: household of two to six registered adults at one address at baseline, address within 500 m of the flood boundary.
# Specification: y ~ flooded x window | address FE + block group x window FE, SEs clustered by tract.
# Inputs from 33_build_hh_detail.py, 35_build_hh_history.py, 43_build_hh_more.py. Usage: Rscript code/44_hh_more.R
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 230)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; TAB <- "output/tables"
W16 <- c("20210101", "20220517", "20230101", "20240514"); STORM <- "20240514"; REF <- "20230101"
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0",
                                 file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
pq <- function(stem, b) sprintf("read_parquet('%s')", file.path(PROC, sprintf("%s_helene_%s.parquet", stem, b)))
D <- rbindlist(lapply(W16, function(b) as.data.table(dbGetQuery(con, sprintf("
    SELECT any_value(c.lat) AS lat, any_value(c.lon) AS lon, any_value(substr(c.geoid,1,12)) AS bg,
           max((c.he_fl > 0.5)::INT) AS flooded, max(h.hh_n) AS hh_n, any_value(d.n) AS n, any_value(d.n_stay) AS n_stay,
           any_value(d.n_move) AS n_move, any_value(k.n_arr) AS n_arr, any_value(m.k_cc) AS k_cc, any_value(m.k_dead) AS k_dead,
           any_value(m.k_rm) AS k_rm, any_value(m.k_rs) AS k_rs, any_value(m.k_exit) AS k_exit, any_value(m.k_inact) AS k_inact,
           any_value(m.k_mail) AS k_mail, any_value(m.k_mailout) AS k_mailout, any_value(m.k_tounit) AS k_tounit, any_value(m.k_near) AS k_near,
           any_value(m.k_mid) AS k_mid, any_value(m.k_far) AS k_far, any_value(m.k_destfl) AS k_destfl, any_value(m.k_desthi) AS k_desthi,
           any_value(m.k_geo) AS k_geo, any_value(m.k_moved) AS k_moved, '%s' AS cohort
    FROM %s c JOIN %s h USING (pid) JOIN %s i USING (pid) JOIN %s d ON d.hb = i.hb JOIN %s k ON k.hb = i.hb JOIN %s m ON m.hb = i.hb
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0
      AND abs(c.he_dist) <= 500 AND c.county_id IN (%s) AND h.hh_n BETWEEN 2 AND 6
    GROUP BY i.hb", b, pq("cohort", b), pq("hh", b), pq("hhid", b), pq("hhd", b), pq("hhh", b), pq("hhm", b), paste0("'", study, "'", collapse = ","))))))
dbDisconnect(con, shutdown = TRUE)
D[, loc := .GRP, by = .(lat, lon)][, bgc := .GRP, by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, c("lat", "lon", "bg") := NULL][, post := as.integer(cohort == STORM)][, fp := flooded * post]
for (v in c("n_stay", "n_move", grep("^k_", names(D), value = TRUE))) set(D, which(is.na(D[[v]])), v, 0)   # no member with the event
D[, `:=`(y_move = as.integer(k_moved >= 1), y_cc = as.integer(k_cc >= 1), y_near = as.integer(k_near >= 1), y_mid = as.integer(k_mid >= 1), y_far = as.integer(k_far >= 1),
         y_nogeo = as.integer(k_moved > k_geo), y_destfl = as.integer(k_destfl >= 1), y_desthi = as.integer(k_desthi >= 1), y_tounit = as.integer(k_tounit >= 1),
         y_dead = as.integer(k_dead >= 1), y_rm = as.integer(k_rm >= 1), y_rs = as.integer(k_rs >= 1), y_exit = as.integer(k_exit >= 1),
         y_inact = as.integer(k_inact >= 1), y_mail = as.integer(k_mail >= 1), y_mailout = as.integer(k_mailout >= 1),
         y_host = as.integer(n_stay == n & n_arr >= 1), y_intact = as.integer(n_stay == n & n_arr == 0), y_size = n_stay + n_arr)]
cat("household-windows:", nrow(D), " storm window flooded:", D[fp == 1, .N], " unflooded:", D[post == 1 & flooded == 0, .N], "\n")
tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)] }
es <- function(v, lab, scale = 100) { P <- copy(D)[, yy := scale * get(v)]
  m <- feols(as.formula(sprintf("yy ~ i(cohort, flooded, ref = '%s') | loc + bgc", REF)), P, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, term := sub("cohort::([0-9]+):flooded", "\\1", term)]
  data.table(outcome = lab, pre1 = o[term == "20210101", est], pre1_se = o[term == "20210101", se], pre2 = o[term == "20220517", est],
             pre2_se = o[term == "20220517", se], pre_joint_p = wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]],
             storm = o[term == STORM, est], storm_se = o[term == STORM, se], storm_p = o[term == STORM, p],
             mean_unflooded_storm = P[post == 1 & flooded == 0, mean(yy)], n_flooded_storm = P[fp == 1, .N], n_unflooded_storm = P[post == 1 & flooded == 0, .N]) }
Y <- list(c("y_move", "A member moved to another address"), c("y_near", "A member moved less than 5 km"), c("y_mid", "A member moved 5 to 50 km"),
          c("y_far", "A member moved more than 50 km"), c("y_nogeo", "A member moved, destination not geocoded"), c("y_cc", "A member registered in another county"),
          c("y_destfl", "A member moved to an address inside the flood layer"), c("y_desthi", "A member moved to an address 500 m+ outside the flood layer"),
          c("y_tounit", "A member moved from an address without unit number to one with"), c("y_exit", "A member no longer registered"),
          c("y_dead", "A member removed: deceased"), c("y_rm", "A member removed: moved from county"), c("y_rs", "A member removed: moved from state"),
          c("y_inact", "A member went from active to inactive status"), c("y_mail", "A member added a mailing address"),
          c("y_mailout", "A member added a mailing address outside NC"), c("y_host", "All members stayed and a new adult is registered at the address"),
          c("y_intact", "All members stayed and nobody new is registered"))
res <- rbind(rbindlist(lapply(Y, function(y) es(y[1], y[2]))), es("y_size", "Registered adults at the baseline address at follow-up (number)", 1))
fwrite(res, file.path(TAB, "hh_more_outcomes_helene.csv"))
print(res[, .(outcome, pre1 = round(pre1, 2), pre2 = round(pre2, 2), pre_p = round(pre_joint_p, 3), storm = round(storm, 2), storm_se = round(storm_se, 2),
              p = round(storm_p, 4), mean_unflooded = round(mean_unflooded_storm, 2), n_flooded = n_flooded_storm, n_unflooded = n_unflooded_storm)])
