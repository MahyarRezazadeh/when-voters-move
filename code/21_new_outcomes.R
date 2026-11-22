# New outcomes on the same flooded / unflooded comparison (exploratory, 2026-11-18).
#   addr : outcome 1, replacement at the address (unit = baseline address x cohort)
#   hh   : outcome 2, household splitting (unit = baseline voter in a 2-6 voter household x cohort)
# Inputs from code/20_build_addr_hh.py. Same specification as the primary estimate:
#   y ~ flooded x post | address FE + block group x cohort FE, SEs clustered by tract
# and a cohort event study y ~ flooded x cohort (reference: last pre-storm cohort).
# Low-memory and checkpointed (a restarted run skips finished regressions).
#
#   hhp  : household party outcomes (unit = observed voter in a 2-6 voter household x cohort)
# Usage: Rscript code/21_new_outcomes.R <event> <addr|hh|hhp>
# Output: output/tables/new_<addr|hh>_did_<event>.csv, new_<addr|hh>_es_<event>.csv, new_<addr|hh>_means_<event>.csv
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb) })
args <- commandArgs(trailingOnly = TRUE)
ev <- args[1]; what <- args[2]; bw <- 500
setFixest_nthreads(2); setDTthreads(2)
DATA_ROOT <- Sys.getenv("VOTING_DATA_ROOT", "C:/voting_data")
PROC <- file.path(DATA_ROOT, "processed"); INTER <- file.path(DATA_ROOT, "intermediate")
TAB <- "output/tables"
design <- list(
  florence = list(e = "fl", pre = list(c("20150101", "20160607"), c("20170101", "20180508")),
                  post = c("20180508", "20190910"), ctrl = "mt"),
  helene = list(e = "he", pre = list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514")),
                post = c("20240514", "20250909"), ctrl = NULL))[[ev]]
e <- design$e; cohorts <- c(design$pre, list(design$post))
ref <- design$pre[[length(design$pre)]][1]
con <- dbConnect(duckdb())
dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
if (what == "addr") {
  D <- rbindlist(lapply(cohorts, function(x) {
    d <- as.data.table(dbGetQuery(con, sprintf("SELECT *, '%s' AS cohort FROM read_parquet('%s')", x[1],
                                               file.path(PROC, sprintf("addr_%s_%s.parquet", ev, x[1])))))
    d
  }), fill = TRUE)
  Y <- list(
    "address has no registered voter at follow-up (pp)" = quote(100 * (n_f == 0)),
    "address has at least one arrival (pp)" = quote(100 * (n_arr > 0)),
    "arrivals per 100 addresses" = quote(100 * n_arr),
    "arrivals who are new registrants, per 100 addresses" = quote(100 * n_arr_new),
    "Democratic arrivals per 100 addresses" = quote(100 * arr_dem),
    "Republican arrivals per 100 addresses" = quote(100 * arr_rep),
    "unaffiliated arrivals per 100 addresses" = quote(100 * arr_una),
    "white arrivals per 100 addresses" = quote(100 * arr_white),
    "Black arrivals per 100 addresses" = quote(100 * arr_black),
    "net change in registered voters per 100 addresses" = quote(100 * (n_f - n_b)))
  ES <- names(Y)[c(1, 3)]
} else if (what %in% c("hhu", "hhuc", "hhuf", "hhufc")) {
  # household as the unit of observation (household id from 27_build_hhid.py). A household is in
  # the sample if its address is; split = members registered at more than one address at follow-up.
  study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE %s_fl > 0",
                                   file.path(INTER, "geocode", "addresspoints_flood.parquet"), e))$county_id
  D <- rbindlist(lapply(cohorts, function(x) {
    ctrl <- if (!is.null(design$ctrl)) sprintf(", max(c.%s_fl) AS ctrl_fl", design$ctrl) else ""
    as.data.table(dbGetQuery(con, sprintf("
      SELECT any_value(c.lat) AS lat, any_value(c.lon) AS lon, any_value(substr(c.geoid,1,12)) AS bg,
             max((c.%1$s_fl > 0.5)::INT) AS flooded %2$s, max(h.split) AS split, max(h.together) AS together,
             count(*) AS members, avg(CASE WHEN c.age BETWEEN 17 AND 110 THEN c.age END) AS age, avg((c.race_code = 'W')::INT) AS white,
             avg((c.sex_code = 'F')::INT) AS female, max(c.has_unit::INT) AS unit,
             CAST(i.hb AS VARCHAR) AS hb, '%3$s' AS cohort
      FROM read_parquet('%4$s') c JOIN read_parquet('%5$s') h USING (pid) JOIN read_parquet('%6$s') i USING (pid)
      WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.%1$s_fl >= 0
        AND abs(c.%1$s_dist) <= %7$f AND c.county_id IN (%8$s) AND h.hh_n BETWEEN 2 AND 6
      GROUP BY i.hb",
      e, ctrl, x[1], file.path(PROC, sprintf("cohort_%s_%s.parquet", ev, x[1])),
      file.path(PROC, sprintf("hh_%s_%s.parquet", ev, x[1])), file.path(PROC, sprintf("hhid_%s_%s.parquet", ev, x[1])),
      bw, paste0("'", study, "'", collapse = ","))))
  }))
  if (what %in% c("hhuf", "hhufc")) {
    # Household fixed effects. A household is followed across windows as the same address (with
    # unit) holding the same anchor member: the member registered there in the most windows.
    # Household-windows without the anchor get their own identifier (singletons, which drop out).
    M <- rbindlist(lapply(cohorts, function(x) as.data.table(dbGetQuery(con, sprintf("
      SELECT CAST(i.hb AS VARCHAR) AS hb, c.pid, '%2$s' AS cohort
      FROM read_parquet('%3$s') c JOIN read_parquet('%4$s') h USING (pid) JOIN read_parquet('%5$s') i USING (pid)
      WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.%1$s_fl >= 0
        AND abs(c.%1$s_dist) <= %6$f AND c.county_id IN (%7$s) AND h.hh_n BETWEEN 2 AND 6",
      e, x[1], file.path(PROC, sprintf("cohort_%s_%s.parquet", ev, x[1])),
      file.path(PROC, sprintf("hh_%s_%s.parquet", ev, x[1])), file.path(PROC, sprintf("hhid_%s_%s.parquet", ev, x[1])),
      bw, paste0("'", study, "'", collapse = ","))))))
    M[, k := .N, by = .(hb, pid)]
    anchor <- M[order(hb, -k, pid), .(anchor = pid[1]), by = hb]
    has <- merge(M, anchor, by = "hb")[pid == anchor, .(hb, cohort, has = 1L)]
    D <- merge(D, has, by = c("hb", "cohort"), all.x = TRUE)
    D[, hid := fifelse(!is.na(has), hb, paste(hb, cohort))]
    cat("household-windows:", nrow(D), " with anchor member:", D[!is.na(has), .N],
        " households seen in 2+ windows:", D[!is.na(has), .N, by = hid][N >= 2, .N], "
")
    rm(M, anchor, has)
  }
  D[, hb := NULL]
  Y <- list(
    "household split: members at more than one address at follow-up (pp)" = quote(100 * split),
    "household moved together to a new address (pp)" = quote(100 * together))
  ES <- names(Y)
} else {
  # hh: household splitting; hhp: household party outcomes (from 26_build_hh_party.py)
  hfile <- if (what == "hh") "hh" else "hhp"
  hcols <- if (what == "hh") sprintf("h.split, h.together, h.couple::INT AS couple, c.addr_%s::INT AS moved", "%3$s") else
    "h.uniform_b, h.uniform_f, h.changed"
  hwhere <- if (what == "hh") "AND h.hh_n BETWEEN 2 AND 6" else ""
  study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE %s_fl > 0",
                                   file.path(INTER, "geocode", "addresspoints_flood.parquet"), e))$county_id
  D <- rbindlist(lapply(cohorts, function(x) {
    ctrl <- if (!is.null(design$ctrl)) sprintf(", c.%s_fl AS ctrl_fl", design$ctrl) else ""
    as.data.table(dbGetQuery(con, sprintf(paste0("
      SELECT c.lat, c.lon, substr(c.geoid,1,12) AS bg, (c.%1$s_fl > 0.5)::INT AS flooded %2$s,
             ", hcols, ", '%4$s' AS cohort
      FROM read_parquet('%5$s') c JOIN read_parquet('%6$s') h USING (pid)
      WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.%1$s_fl >= 0
        AND abs(c.%1$s_dist) <= %7$f AND c.county_id IN (%8$s) ", hwhere),
      e, ctrl, x[2], x[1], file.path(PROC, sprintf("cohort_%s_%s.parquet", ev, x[1])),
      file.path(PROC, sprintf("%s_%s_%s.parquet", hfile, ev, x[1])), bw, paste0("'", study, "'", collapse = ","))))
  }))
  if (what == "hh") {
    Y <- list(
      "household split: members registered at different addresses (pp)" = quote(100 * split),
      "household moved together to a new address (pp)" = quote(100 * together),
      "any change of registration address, voters in 2-6 voter households (pp)" = quote(100 * moved),
      "couples only: split (pp)" = quote(100 * split * couple),
      "couples only: moved together (pp)" = quote(100 * together * couple))
    ES <- names(Y)[1]
  } else {
    Y <- list(
      "household all one party at baseline: mixed parties at follow-up (pp)" = quote(100 * (1 - uniform_f)),
      "household mixed parties at baseline: all one party at follow-up (pp)" = quote(100 * uniform_f),
      "voter changed party (pp)" = quote(100 * changed))
    SUB <- list(quote(uniform_b == 1), quote(uniform_b == 0), NULL); names(SUB) <- names(Y)
    ES <- names(Y); DID <- character(0)     # event study only (the storm-cohort coefficient is the effect)
  }
}
dbDisconnect(con, shutdown = TRUE)
if (!exists("SUB")) SUB <- list()
if (!exists("DID")) DID <- names(Y)
dat <- function(nm) if (is.null(SUB[[nm]])) D else D[eval(SUB[[nm]])]
D[, loc := .GRP, by = .(lat, lon)]
D[, bgc := .GRP, by = .(bg, cohort)]
D[, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, post := as.integer(cohort == design$post[1])]
D[, c("lat", "lon", "bg") := NULL]; invisible(gc())
cat("rows:", nrow(D), " flooded in storm cohort:", D[post == 1 & flooded == 1, .N], " unflooded:", D[post == 1 & flooded == 0, .N], "\n")
ctrl <- if (!is.null(design$ctrl)) sprintf(" + i(cohort, ctrl_fl, ref = '%s')", design$pre[[1]][1]) else ""
FE <- if (what %in% c("hhuf", "hhufc")) "hid + bgc" else "loc + bgc"
# hhuc: household characteristics with cohort-specific coefficients
if (what %in% c("hhuc", "hhufc")) { D <- D[!is.na(age)]
  ctrl <- paste0(ctrl, " + i(cohort, members) + i(cohort, age) + i(cohort, white) + i(cohort, female) + i(cohort, unit)") }

ckf <- file.path(TAB, sprintf(".ckpt_new_%s_%s.csv", what, ev))
done <- if (file.exists(ckf)) fread(ckf) else data.table(key = character(0))
ckpt <- function(ky, fun) {
  if (ky %in% done$key) return(done[done$key == ky])
  r <- fun(); r[, key := ky]
  fwrite(r, ckf, append = file.exists(ckf)); invisible(gc()); cat("done:", ky, "\n"); r
}
means <- rbindlist(lapply(names(Y), function(nm) {
  D[, yy := eval(Y[[nm]])]
  dat(nm)[, .(outcome = nm, mean = mean(yy), n = .N), by = .(cohort, flooded)]
}))[order(outcome, cohort, flooded)]
means[, event := ev]; fwrite(means, file.path(TAB, sprintf("new_%s_means_%s.csv", what, ev)))
res <- rbindlist(lapply(DID, function(nm) ckpt(paste("did", nm), function() {
  D[, yy := eval(Y[[nm]])]; P <- dat(nm)
  m <- feols(as.formula(sprintf("yy ~ flooded:post%s | %s", ctrl, FE)), data = P, cluster = ~tract, mem.clean = TRUE, lean = TRUE)
  k <- "flooded:post"
  data.table(kind = "did", event = ev, outcome = nm, term = "flooded x post", est = coef(m)[k], se = se(m)[k], p = pvalue(m)[k],
             base_mean = P[post == 0 & flooded == 1 & cohort == ref, mean(yy)], n_obs = nobs(m),
             n_flooded_post = P[post == 1 & flooded == 1, .N])
})), fill = TRUE)
es <- rbindlist(lapply(ES, function(nm) ckpt(paste("es", nm), function() {
  D[, yy := eval(Y[[nm]])]; P <- dat(nm)
  m <- feols(as.formula(sprintf("yy ~ i(cohort, flooded, ref = '%s')%s | %s", ref, ctrl, FE)), data = P, cluster = ~tract,
             mem.clean = TRUE, lean = TRUE)
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl(":flooded$", term)]
  out <- data.table(kind = "es", event = ev, outcome = nm, term = sub("cohort::(\\d+):flooded", "\\1", ct$term),
                    est = ct$Estimate, se = ct$`Std. Error`, p = ct$`Pr(>|t|)`, base_mean = P[post == 0 & flooded == 1 & cohort == ref, mean(yy)],
                    n_obs = nobs(m), n_flooded_post = P[post == 1 & flooded == 1, .N])
  rbind(out, data.table(kind = "es", event = ev, outcome = nm, term = ref, est = 0, se = 0, p = NA_real_, base_mean = out$base_mean[1],
                        n_obs = nobs(m), n_flooded_post = out$n_flooded_post[1]))
})), fill = TRUE)
options(width = 220)
if (nrow(res)) { print(res[, .(outcome, est, se, p, base_mean, n_obs, n_flooded_post)])
  fwrite(res[, -c("key", "kind")], file.path(TAB, sprintf("new_%s_did_%s.csv", what, ev))) }
print(es[order(outcome, term), .(outcome, term, est, se, p, base_mean, n_obs, n_flooded_post)])
fwrite(es[order(outcome, term), -c("key", "kind")], file.path(TAB, sprintf("new_%s_es_%s.csv", what, ev)))
file.remove(ckf)
