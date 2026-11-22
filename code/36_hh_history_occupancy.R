# Helene households: (1) were the members together before the baseline, (2) is the flooded address still lived in
# (exploratory, 2026-11-20). Unit: household of two to six registered adults at one address at baseline, address
# within 500 m of the flood boundary. Inputs from 33_build_hh_detail.py and 35_build_hh_history.py.
# Specification: y ~ flooded x window | address FE + block group x window FE, SEs clustered by tract.
# Usage: Rscript code/36_hh_history_occupancy.R
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
           max((c.he_fl > 0.5)::INT) AS flooded, any_value(c.he_dist) AS he_dist, max(h.hh_n) AS hh_n, max(c.has_unit::INT) AS unit,
           any_value(d.n) AS n, any_value(d.n_stay) AS n_stay, any_value(d.n_move) AS n_move, any_value(d.n_dest) AS n_dest,
           any_value(k.n_lk) AS n_lk, any_value(k.n_lk_home) AS n_lk_home, any_value(k.n_lk_dest) AS n_lk_dest,
           any_value(k.n_arr) AS n_arr, any_value(k.n_arr_l) AS n_arr_l, any_value(k.n_stay_l) AS n_stay_l, '%s' AS cohort
    FROM %s c JOIN %s h USING (pid) JOIN %s i USING (pid) JOIN %s d ON d.hb = i.hb JOIN %s k ON k.hb = i.hb
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0
      AND abs(c.he_dist) <= 500 AND c.county_id IN (%s) AND h.hh_n BETWEEN 2 AND 6
    GROUP BY i.hb", b, pq("cohort", b), pq("hh", b), pq("hhid", b), pq("hhd", b), pq("hhh", b), paste0("'", study, "'", collapse = ","))))))
dbDisconnect(con, shutdown = TRUE)
D[, loc := .GRP, by = .(lat, lon)][, bgc := .GRP, by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, c("lat", "lon") := NULL][, post := as.integer(cohort == STORM)][, fp := flooded * post]
for (v in c("n_stay", "n_move")) set(D, which(is.na(D[[v]])), v, 0)
D[, `:=`(partial = as.integer(n_stay >= 1 & n_move >= 1), allsplit = as.integer(n_stay == 0 & n_dest >= 2),
         tog = as.integer(n_stay == 0 & n_move >= 2 & n_dest == 1))]
D[, allleft := as.integer(allsplit == 1 | tog == 1)]
cat("household-windows:", nrow(D), " storm window flooded:", D[fp == 1, .N], " unflooded:", D[post == 1 & flooded == 0, .N], "\n")
tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)] }

# ============================ 1. together before the baseline? ====================================
D[, g_hist := fifelse(n_lk == n & n_lk_home == n, "1 All at this address before",
              fifelse(n_lk == n & n_lk_dest == 1, "2 All together at another address before", "3 Not all together before"))]
D[, g_hist2 := fifelse(g_hist == "3 Not all together before", "Not all together before", "All together before")]
cat("\n== 1a. storm-window households by history (share of households, %) ==\n")
print(D[post == 1, .(households = .N), by = .(flooded, g_hist)][, share := round(100 * households / sum(households), 1), by = flooded][order(flooded, g_hist)])
YH <- c(tog = "All leave together", partial = "Split: some stay, some leave", allsplit = "Split: all leave, different addresses", allleft = "All members leave")
het <- function(gg) rbindlist(lapply(names(YH), function(v) { P <- copy(D)[, `:=`(yy = 100 * get(v), grp = get(gg))]
  P[, tc := paste(grp, cohort, sep = "|")]; refs <- paste(unique(P$grp), REF, sep = "|")
  m <- feols(yy ~ i(tc, flooded, ref = refs) | loc + bgc + grp^cohort, P, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, c("group", "win") := tstrsplit(sub("^tc::(.*):flooded$", "\\1", term), "|", fixed = TRUE)]
  w <- dcast(o, group ~ win, value.var = c("est", "se")); cf <- coef(m); V <- vcov(m)
  w[, pre_p := sapply(group, function(a) { k <- paste0("tc::", a, "|", c("20210101", "20220517"), ":flooded")
    pchisq(drop(t(cf[k]) %*% solve(V[k, k]) %*% cf[k]), 2, lower.tail = FALSE) })]
  lv <- sort(unique(P$grp)); P[, f1 := fp * (grp != lv[1])]
  dd <- if (length(lv) == 2) coeftable(feols(yy ~ i(tc, flooded, ref = c(refs, paste(lv, STORM, sep = "|"))) + fp + f1 | loc + bgc + grp^cohort, P, cluster = ~tract))["f1", "Pr(>|t|)"] else NA_real_
  nn <- P[post == 1, .(n_flooded = sum(flooded), n_unflooded = sum(1 - flooded), mean_unflooded = mean(yy[flooded == 0])), by = .(group = grp)]
  merge(w, nn, by = "group")[, `:=`(dimension = gg, outcome = YH[[v]], diff_p = dd)] }))
resH <- rbind(het("g_hist"), het("g_hist2"))
setnames(resH, c("est_20210101", "est_20220517", "est_20240514", "se_20210101", "se_20220517", "se_20240514"), c("pre1", "pre2", "storm", "pre1_se", "pre2_se", "storm_se"))
fwrite(resH, file.path(TAB, "hh_history_hetero_helene.csv"))
cat("\n== 1b. flood effect by history (percentage points) ==\n")
print(resH[, .(outcome, group, pre1 = round(pre1, 2), pre2 = round(pre2, 2), storm = round(storm, 2), storm_se = round(storm_se, 2), pre_p = round(pre_p, 3),
               diff_p = round(diff_p, 3), mean_unflooded = round(mean_unflooded, 2), n_flooded, n_unflooded)])
CH <- c(h1 = "All at this address before", h2 = "All together at another address before", h3 = "Not all together before")
D[, `:=`(h1 = as.integer(g_hist == "1 All at this address before"), h2 = as.integer(g_hist == "2 All together at another address before"), h3 = as.integer(g_hist == "3 Not all together before"))]
RS <- c(tog = "All leave together", partial = "Split: some stay, some leave")
resC <- rbindlist(lapply(names(CH), function(x) rbindlist(lapply(names(RS), function(r) { P <- copy(D)[, `:=`(R = get(r), xr = 100 * get(x) * get(r))]
  m <- feols(xr ~ 1 | loc + bgc | R ~ fp, P, cluster = ~tract)
  data.table(history = CH[[x]], response = RS[[r]], share = coef(m)[["fit_R"]], se = se(m)[["fit_R"]], share_all_flooded = 100 * P[fp == 1, mean(get(x))]) }))))
fwrite(resC, file.path(TAB, "hh_history_complier_helene.csv"))
cat("\n== 1c. history of the households whose response is caused by the flood (share, %) ==\n")
print(dcast(resC[, .(history, share_all_flooded = round(share_all_flooded, 1), response, v = sprintf("%5.1f (%.1f)", share, se))], history + share_all_flooded ~ response, value.var = "v"))

# ============================ 2. is the address still lived in? ===================================
D[, `:=`(vacant = as.integer(n_stay == 0 & n_arr == 0),                         # no registered adult at the address at follow-up
         left_new = as.integer(n_stay == 0 & n_move >= 1 & n_arr >= 1),          # all members left and someone new is registered there
         left_empty = as.integer(n_stay == 0 & n_move >= 1 & n_arr == 0),        # all members left and nobody is registered there
         part_new = as.integer(partial == 1 & n_arr >= 1), anynew = as.integer(n_arr >= 1))]
YO <- c(left_empty = "All members left, nobody registered at the address", left_new = "All members left, someone new registered at the address",
        vacant = "Nobody registered at the address", anynew = "Someone new registered at the address")
es <- function(v, P, lab) { P <- copy(P)[, yy := 100 * get(v)]
  m <- feols(as.formula(sprintf("yy ~ i(cohort, flooded, ref = '%s') | loc + bgc", REF)), P, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, term := sub("cohort::([0-9]+):flooded", "\\1", term)]
  data.table(outcome = lab, pre1 = o[term == "20210101", est], pre1_se = o[term == "20210101", se], pre2 = o[term == "20220517", est],
             pre2_se = o[term == "20220517", se], pre_joint_p = wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]],
             storm = o[term == STORM, est], storm_se = o[term == STORM, se], storm_p = o[term == STORM, p],
             mean_unflooded_storm = P[post == 1 & flooded == 0, mean(yy)], n_flooded_storm = P[fp == 1, .N], n_unflooded_storm = P[post == 1 & flooded == 0, .N]) }
SAMP <- list(c("All households", "TRUE"), c("Addresses without unit number", "unit == 0"), c("Flooded 30 m or more inside the boundary", "flooded == 0 | he_dist <= -30.2"),
             c("Flooded less than 30 m inside the boundary", "flooded == 0 | he_dist > -30.2"))
resO <- rbindlist(lapply(SAMP, function(s) rbindlist(lapply(names(YO), function(v) es(v, D[eval(parse(text = s[2]))], YO[[v]])[, sample := s[1]]))))
fwrite(resO, file.path(TAB, "hh_occupancy_helene.csv"))
cat("\n== 2a. flood effect on who is registered at the baseline address at follow-up (percentage points) ==\n")
print(resO[, .(sample, outcome, pre1 = round(pre1, 2), pre2 = round(pre2, 2), pre_joint_p = round(pre_joint_p, 3), storm = round(storm, 2), storm_se = round(storm_se, 2),
               mean_unflooded_storm = round(mean_unflooded_storm, 2), n_flooded_storm)])
cat("\n== 2b. storm window, by response at 12 months: who is at the address (descriptive, cells below 10 suppressed) ==\n")
S <- D[post == 1][, resp := fifelse(tog == 1, "All leave together", fifelse(allsplit == 1, "All leave, different addresses", fifelse(partial == 1, "Some stay, some leave", "Other")))]
d2 <- S[resp != "Other", .(households = .N, new_at_12m = round(100 * mean(n_arr >= 1), 1), new_at_25m = round(100 * mean(n_arr_l >= 1), 1),
                           member_there_25m = round(100 * mean(n_stay_l >= 1), 1), nobody_25m = round(100 * mean(n_stay_l == 0 & n_arr_l == 0), 1)), by = .(resp, flooded)][order(resp, flooded)]
d2[households < 10, c("households", "new_at_12m", "new_at_25m", "member_there_25m", "nobody_25m") := NA]
fwrite(d2, file.path(TAB, "hh_occupancy_by_response_helene.csv")); print(d2)
