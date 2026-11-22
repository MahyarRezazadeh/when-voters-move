# Helene households: what separates splitting from moving together, and checks of the identifying assumption
# (exploratory, 2026-11-20). Unit: household of two to six registered adults at one address at baseline, address
# within 500 m of the flood boundary. Inputs from 20_build_addr_hh.py, 27_build_hhid.py, 31_build_hh_long.py,
# 33_build_hh_detail.py, 10_sfha_assign.py.
# Specification: y ~ flooded x window | address FE + block group x window FE, SEs clustered by tract.
# Usage: Rscript code/34_hh_response_source.R
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 230)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; TAB <- "output/tables"
W16 <- c("20210101", "20220517", "20230101", "20240514"); STORM <- "20240514"; REF <- "20230101"
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0",
                                 file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
pq <- function(stem, b) sprintf("read_parquet('%s')", file.path(PROC, sprintf("%s_helene_%s.parquet", stem, b)))
sf <- sprintf("read_parquet('%s')", file.path(INTER, "flood", "sfha_helene_locs.parquet"))
D <- rbindlist(lapply(W16, function(b) as.data.table(dbGetQuery(con, sprintf("
    SELECT any_value(c.lat) AS lat, any_value(c.lon) AS lon, any_value(substr(c.geoid,1,12)) AS bg,
           max((c.he_fl > 0.5)::INT) AS flooded, any_value(c.he_dist) AS he_dist, any_value(c.he_sh) AS he_sh,
           max(coalesce(s.sfha100, 0)) AS sfha, max((trim(c.municipality_abbrv) <> '')::INT) AS muni,
           max(h.split) AS split, max(h.together) AS together, max(h.hh_n) AS hh_n, max(c.has_unit::INT) AS unit,
           any_value(t.htype) AS htype, max(t.old65) AS old65,
           any_value(d.n) AS n, any_value(d.nobs) AS nobs, any_value(d.n_stay) AS n_stay, any_value(d.n_move) AS n_move,
           any_value(d.n_dest) AS n_dest, any_value(d.n_out) AS n_out, any_value(d.age_move) AS age_move,
           any_value(d.age_stay) AS age_stay, any_value(d.age_min) AS age_min, any_value(d.age_max) AS age_max,
           any_value(d.n_white) AS n_white, any_value(d.n_nc) AS n_nc, any_value(d.n_bp) AS n_bp,
           any_value(d.reg_first) AS reg_first, '%s' AS cohort
    FROM %s c JOIN %s h USING (pid) JOIN %s i USING (pid) JOIN %s t USING (pid) JOIN %s d ON d.hb = i.hb
         LEFT JOIN %s s ON s.lat = c.lat AND s.lon = c.lon
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0
      AND abs(c.he_dist) <= 500 AND c.county_id IN (%s) AND h.hh_n BETWEEN 2 AND 6
    GROUP BY i.hb", b, pq("cohort", b), pq("hh", b), pq("hhid", b), pq("hht", b), pq("hhd", b), sf,
    paste0("'", study, "'", collapse = ","))))))
dbDisconnect(con, shutdown = TRUE)
D[, loc := .GRP, by = .(lat, lon)][, bgc := .GRP, by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, c("lat", "lon") := NULL]
D[, post := as.integer(cohort == STORM)][, fp := flooded * post]
# ---- response, mutually exclusive ----
for (v in c("n_stay", "n_move", "n_out")) set(D, which(is.na(D[[v]])), v, 0)   # no member observed at follow-up
D[, `:=`(partial = as.integer(n_stay >= 1 & n_move >= 1),                 # some members stay, some leave
         allsplit = as.integer(n_stay == 0 & n_dest >= 2),                # nobody stays, members at different addresses
         tog = as.integer(n_stay == 0 & n_move >= 2 & n_dest == 1))]      # nobody stays, all movers at one address
D[, `:=`(allleft = as.integer(allsplit == 1 | tog == 1), anysplit = as.integer(partial == 1 | allsplit == 1),
         allobs = as.integer(nobs == n), shobs = nobs / n,
         p_young = as.integer(partial == 1 & !is.na(age_move) & !is.na(age_stay) & age_move < age_stay - 5),
         p_old = as.integer(partial == 1 & !is.na(age_move) & !is.na(age_stay) & age_move > age_stay + 5))]
D[, p_same := as.integer(partial == 1 & p_young == 0 & p_old == 0)]
cat("household-windows:", nrow(D), " storm window flooded:", D[post == 1 & flooded == 1, .N], " unflooded:", D[post == 1 & flooded == 0, .N], "\n")
cat("agreement with earlier definitions: split", D[, mean(split == anysplit)], " together", D[, mean(together == tog)], "\n")
cat("tracts:", uniqueN(D$tract), " tracts with a flooded household in the storm window:", D[fp == 1, uniqueN(tract)], "\n")
tidy <- function(m, pat) { ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(pat, term)]
  ct[, .(term, est = Estimate, se = `Std. Error`, p = `Pr(>|t|)`)] }
es <- function(v, P = D, lab = v, scale = 100) { P <- copy(P)[, yy := scale * get(v)]
  m <- feols(as.formula(sprintf("yy ~ i(cohort, flooded, ref = '%s') | loc + bgc", REF)), P, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, term := sub("cohort::(\\d+):flooded", "\\1", term)]
  pre <- wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)
  data.table(outcome = lab, pre1 = o[term == "20210101", est], pre1_se = o[term == "20210101", se],
             pre2 = o[term == "20220517", est], pre2_se = o[term == "20220517", se], pre_joint_p = pre[["p"]],
             storm = o[term == STORM, est], storm_se = o[term == STORM, se], storm_p = o[term == STORM, p],
             mean_unflooded_storm = P[post == 1 & flooded == 0, mean(yy)], n_flooded_storm = P[post == 1 & flooded == 1, .N],
             n_unflooded_storm = P[post == 1 & flooded == 0, .N], n_obs = nobs(m)) }

# ============================ 1. identifying assumption ===========================================
cat("\n== 1a. event study, pre-storm coefficients and joint test ==\n")
OUT <- c(anysplit = "Split (any)", partial = "Split: some stay, some leave", allsplit = "Split: all leave, different addresses",
         tog = "All leave together", allleft = "All members leave", p_young = "Some stay: leavers younger (5+ years)",
         p_old = "Some stay: leavers older (5+ years)", p_same = "Some stay: leavers similar age")
r1 <- rbindlist(lapply(names(OUT), function(v) es(v, lab = OUT[[v]]))); print(r1[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("\n== 1b. baseline characteristics: does the flooded-unflooded difference change in the storm window? ==\n")
D[, `:=`(x_n = hh_n, x_agemax = age_max, x_agemin = age_min, x_couple = as.integer(htype == "couple"), x_multi = as.integer(htype == "multigen"),
         x_unit = unit, x_white = n_white / n, x_nc = fifelse(n_bp > 0, n_nc / n_bp, NA_real_),
         x_ten = as.integer(substr(cohort, 1, 4)) - reg_first)]
XB <- c(x_n = "Members", x_agemax = "Age of oldest", x_agemin = "Age of youngest", x_couple = "Couple (share)", x_multi = "Two generations (share)",
        x_unit = "Unit number (share)", x_white = "White (share)", x_nc = "Born in NC (share)", x_ten = "Years since first registration")
r2 <- rbindlist(lapply(names(XB), function(v) { P <- D[!is.na(get(v))]; sc <- if (v %in% c("x_n", "x_agemax", "x_agemin", "x_ten")) 1 else 100
  o <- es(v, P, XB[[v]], sc)
  lv <- feols(as.formula(paste0("I(", sc, "*", v, ") ~ flooded | bg")), P[post == 1], cluster = ~tract)
  o[, `:=`(level_diff_storm = coef(lv)[["flooded"]], level_se = se(lv)[["flooded"]])] }))
print(r2[, .(outcome, mean_unflooded_storm = round(mean_unflooded_storm, 2), level_diff_storm = round(level_diff_storm, 2), level_se = round(level_se, 2),
             change_storm = round(storm, 2), change_se = round(storm_se, 2), change_p = round(storm_p, 3), pre_joint_p = round(pre_joint_p, 3))])
cat("\n== 1c. observation of members at follow-up ==\n")
r3 <- rbind(es("allobs", lab = "All members observed at follow-up"), es("shobs", lab = "Share of members observed")); print(r3[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])
cat("\n== 1d. spillover to unflooded households near the boundary, and sample choices ==\n")
U <- D[flooded == 0 & (he_dist <= 100 | he_dist >= 250)]; U[, near := as.integer(he_dist <= 100)]
r4 <- rbindlist(lapply(c("anysplit", "tog"), function(v) { U[, yy := 100 * get(v)]
  m <- feols(as.formula(sprintf("yy ~ i(cohort, near, ref = '%s') | loc + bgc", REF)), U, cluster = ~tract)
  o <- tidy(m, ":near$")[, term := sub("cohort::(\\d+):near", "\\1", term)]
  o[, `:=`(outcome = OUT[[v]], check = "Unflooded only: within 100 m of boundary vs 250-500 m", n_near = U[post == 1 & near == 1, .N], n_far = U[post == 1 & near == 0, .N])] }))
print(r4)
r5 <- rbindlist(lapply(list(c("Main sample", "TRUE"), c("Drop unflooded within 100 m", "flooded == 1 | he_dist > 100"),
                            c("Within 250 m", "abs(he_dist) <= 250"), c("Drop flooded within 10 m of boundary", "flooded == 0 | he_dist < -10")), function(s)
  rbindlist(lapply(c("anysplit", "tog"), function(v) es(v, D[eval(parse(text = s[2]))], OUT[[v]])[, check := s[1]]))))
print(r5[, .(check, outcome, pre1 = round(pre1, 2), pre2 = round(pre2, 2), pre_joint_p = round(pre_joint_p, 3), storm = round(storm, 2), storm_se = round(storm_se, 2), n_flooded_storm, n_unflooded_storm)])
fwrite(rbind(r1[, check := "event study"], r3[, check := "observation"], r5, fill = TRUE), file.path(TAB, "hh_source_ident_helene.csv"))
fwrite(r2, file.path(TAB, "hh_source_balance_helene.csv")); fwrite(r4, file.path(TAB, "hh_source_spillover_helene.csv"))

# ============================ 2. heterogeneity ====================================================
med <- D[fp == 1, median(he_sh)]; cat("\nmedian flooded share within 100 m among flooded households:", round(med, 3), "\n")
D[, `:=`(g_sev = fifelse(flooded == 0, "unflooded", fifelse(he_sh >= med, "Flooded, more than half of surroundings", "Flooded, edge of flood")),
         g_type = fifelse(htype == "couple", "Couple", fifelse(htype == "multigen", "Two generations (age gap 18+)", "Other same-generation")),
         g_size = fifelse(hh_n == 2, "Two members", "Three to six members"),
         g_old = fifelse(old65 == 1, "Oldest 65 or over", "Oldest under 65"),
         g_young = fifelse(!is.na(age_min) & age_min < 30, "Youngest under 30", "Youngest 30 or over"),
         g_nc = fifelse(n_bp == 0, "Birthplace not recorded", fifelse(n_nc == n_bp, "All born in NC", "Some born elsewhere")),
         g_ten = fifelse(!is.na(x_ten) & x_ten >= 10, "First registered 10+ years ago", "First registered under 10 years ago"),
         g_unit = fifelse(unit == 1, "Unit number", "No unit number"),
         g_sfha = fifelse(sfha == 1, "In mapped floodplain", "Outside mapped floodplain"),
         g_muni = fifelse(muni == 1, "Inside a municipality", "Unincorporated"))]
YH <- c(anysplit = "Split (any)", partial = "Split: some stay, some leave", allsplit = "Split: all leave, different addresses",
        tog = "All leave together", allleft = "All members leave")
het <- function(gg) rbindlist(lapply(names(YH), function(v) { P <- copy(D)[, `:=`(yy = 100 * get(v), grp = get(gg))]
  sev <- gg %in% c("g_sev", "g_sev3", "g_dep"); if (sev) P[, grp2 := fifelse(flooded == 0, P[flooded == 1, min(grp)], grp)] else P[, grp2 := grp]
  fe <- if (sev) "loc + bgc" else "loc + bgc + grp2^cohort"
  P[, tc := paste(grp2, cohort, sep = "|")]; refs <- paste(unique(P$grp2), REF, sep = "|")
  m <- feols(as.formula(paste0("yy ~ i(tc, flooded, ref = refs) | ", fe)), P, cluster = ~tract)
  o <- tidy(m, ":flooded$")[, c("group", "win") := tstrsplit(sub("^tc::(.*):flooded$", "\\1", term), "|", fixed = TRUE)]
  w <- dcast(o, group ~ win, value.var = c("est", "se")); cf <- coef(m); V <- vcov(m)
  w[, pre_p := sapply(group, function(a) { k <- paste0("tc::", a, "|", c("20210101", "20220517"), ":flooded")
    pchisq(drop(t(cf[k]) %*% solve(V[k, k]) %*% cf[k]), 2, lower.tail = FALSE) })]
  lv <- sort(unique(P$grp2)); P[, f1 := fp * (grp2 != lv[1])]
  dd <- if (length(lv) == 2) { m2 <- feols(as.formula(paste0("yy ~ i(tc, flooded, ref = c(refs, paste(lv, STORM, sep = '|'))) + fp + f1 | ", fe)), P, cluster = ~tract)
    coeftable(m2)["f1", "Pr(>|t|)"] } else NA_real_
  nn <- P[post == 1 & (flooded == 1 | !sev), .(n_flooded = sum(flooded), n_unflooded = sum(1 - flooded)), by = .(group = grp2)]
  merge(w, nn, by = "group")[, `:=`(dimension = gg, outcome = YH[[v]], diff_p = dd)] }))
t3 <- D[fp == 1, quantile(he_sh, c(1/3, 2/3))]; md <- D[fp == 1, median(-he_dist)]
cat("terciles of flooded share:", round(t3, 3), " median distance inside the boundary (m):", round(md, 1), "
")
D[, g_sev3 := fifelse(flooded == 0, "unflooded", fifelse(he_sh < t3[1], "1 Flooded share: lowest third", fifelse(he_sh < t3[2], "2 Flooded share: middle third", "3 Flooded share: highest third")))]
D[, g_dep := fifelse(flooded == 0, "unflooded", fifelse(-he_dist >= md, "Further inside the flood boundary", "Close to the flood boundary"))]
DIM <- c("g_sev", "g_sev3", "g_dep", "g_type", "g_size", "g_old", "g_young", "g_nc", "g_ten", "g_unit", "g_sfha", "g_muni")
resH <- rbindlist(lapply(DIM, het), fill = TRUE)
setnames(resH, c("est_20210101", "est_20220517", "est_20240514", "se_20210101", "se_20220517", "se_20240514"), c("pre1", "pre2", "storm", "pre1_se", "pre2_se", "storm_se"))
fwrite(resH, file.path(TAB, "hh_source_hetero_helene.csv"))
cat("\n== 2. flood effect in the storm window by predetermined characteristic (percentage points) ==\n")
for (v in YH) { cat("\n--", v, "--\n"); print(resH[outcome == v, .(dimension, group, pre1 = round(pre1, 2), pre2 = round(pre2, 2), storm = round(storm, 2),
                                                                      storm_se = round(storm_se, 2), pre_p = round(pre_p, 3), diff_p = round(diff_p, 3), n_flooded, n_unflooded)]) }
# continuous severity: flooded share of the surroundings (0 to 1), centred at 0.5
D[, fsh := flooded * (he_sh - 0.5)]
resD <- rbindlist(lapply(names(YH), function(v) { D[, yy := 100 * get(v)]
  m <- feols(as.formula(sprintf("yy ~ i(cohort, flooded, ref = '%s') + i(cohort, fsh, ref = '%s') | loc + bgc", REF, REF)), D, cluster = ~tract)
  o <- tidy(m, ":fsh$")[, term := sub("cohort::([0-9]+):fsh", "\\1", term)]
  data.table(outcome = YH[[v]], pre1 = o[term == "20210101", est], pre1_se = o[term == "20210101", se], pre2 = o[term == "20220517", est],
             pre2_se = o[term == "20220517", se], pre_joint_p = wald(m, ":fsh$", keep = "2021|2022", print = FALSE)[["p"]],
             slope_storm = o[term == STORM, est], slope_se = o[term == STORM, se], slope_p = o[term == STORM, p]) }))
fwrite(resD, file.path(TAB, "hh_source_dose_helene.csv"))
cat("
== 2b. slope of the flood effect in the flooded share of the surroundings (pp per unit, 0 to 1) ==
"); print(resD[, lapply(.SD, function(x) if (is.numeric(x)) round(x, 3) else x)])

# ============================ 3. all characteristics together =====================================
D[, `:=`(z_deep = as.integer(flooded == 1 & he_sh >= med), z_couple = as.integer(htype == "couple"), z_multi = as.integer(htype == "multigen"),
         z_big = as.integer(hh_n >= 3), z_old = old65, z_young = as.integer(!is.na(age_min) & age_min < 30),
         z_else = as.integer(n_bp > 0 & n_nc < n_bp), z_nobp = as.integer(n_bp == 0), z_ten = as.integer(!is.na(x_ten) & x_ten >= 10),
         z_unit = unit, z_sfha = sfha, z_muni = muni)]
Z <- c("z_couple", "z_multi", "z_old", "z_young", "z_else", "z_nobp", "z_ten", "z_unit", "z_sfha", "z_muni")
rhs <- paste0("fp + I(fp*z_deep) + ", paste0("I(fp*", Z, ")", collapse = " + "), " + ", paste0("i(cohort, ", Z, ")", collapse = " + "))
resJ <- rbindlist(lapply(names(YH), function(v) { D[, yy := 100 * get(v)]
  m <- feols(as.formula(paste0("yy ~ ", rhs, " | loc + bgc")), D, cluster = ~tract)
  tidy(m, "^fp$|^I\\(fp")[, outcome := YH[[v]]] }))
resJ[, term := gsub("I\\(fp \\* |\\)", "", term)]
fwrite(resJ, file.path(TAB, "hh_source_joint_helene.csv"))
cat("\n== 3. joint model: flood effect (fp) and its interactions, all characteristics together ==\n")
print(dcast(resJ[, .(term, outcome, v = sprintf("%6.2f (%.2f)%s", est, se, fifelse(p < 0.01, "***", fifelse(p < 0.05, "**", fifelse(p < 0.1, "*", "")))))], term ~ outcome, value.var = "v"))

# ============================ 4. who are the households moved by the flood ========================
# Mean of a baseline characteristic among households whose response is caused by the flood:
# IV of (characteristic x response) on response, with flooded x storm window as the instrument.
CH <- c(z_couple = "Couple", z_multi = "Two generations", z_big = "Three to six members", z_old = "Oldest 65 or over",
        z_young = "Youngest under 30", z_else = "Some born outside NC", z_ten = "First registered 10+ years ago", z_unit = "Unit number",
        z_muni = "Inside a municipality")
RS <- c(partial = "Split: some stay, some leave", allsplit = "Split: all leave, different addresses", tog = "All leave together")
resC <- rbindlist(lapply(names(CH), function(x) rbindlist(lapply(names(RS), function(r) { P <- copy(D)[, `:=`(R = get(r), xr = 100 * get(x) * get(r))]
  m <- feols(xr ~ 1 | loc + bgc | R ~ fp, P, cluster = ~tract)
  data.table(characteristic = CH[[x]], response = RS[[r]], share = coef(m)[["fit_R"]], se = se(m)[["fit_R"]],
             share_all_flooded = 100 * P[fp == 1, mean(get(x))]) }))))
fwrite(resC, file.path(TAB, "hh_source_complier_helene.csv"))
cat("\n== 4. share (%) with each characteristic among households whose response is caused by the flood ==\n")
print(dcast(resC[, .(characteristic, response, share_all_flooded = round(share_all_flooded, 1), v = sprintf("%5.1f (%.1f)", share, se))],
            characteristic + share_all_flooded ~ response, value.var = "v"))
# share of households that the flood causes to leave entirely which stay together (IV: together on all-leave)
resR <- rbindlist(lapply(list(c("All households", "TRUE"), c("Two members", "hh_n == 2"), c("Three to six members", "hh_n >= 3"),
                              c("Couple", "htype == 'couple'"), c("Two generations", "htype == 'multigen'")), function(a) { P <- D[eval(parse(text = a[2]))]
  m <- feols(tog ~ 1 | loc + bgc | allleft ~ fp, P, cluster = ~tract)
  data.table(sample = a[1], share_together = 100 * coef(m)[["fit_allleft"]], se = 100 * se(m)[["fit_allleft"]],
             first_stage_pp = 100 * coef(m$iv_first_stage$allleft)[["fp"]], n_flooded = P[fp == 1, .N]) }))
fwrite(resR, file.path(TAB, "hh_source_together_share_helene.csv"))
cat("
== 5. of households the flood causes to leave entirely, share (%) that stay together ==
"); print(resR)
