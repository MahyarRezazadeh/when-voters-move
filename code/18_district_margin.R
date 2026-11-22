# Close versus safe counties or state house districts (exploratory; added 2026-11-17 at the
# author's request). Low-memory version with checkpoints: every finished regression is appended
# to a checkpoint file, and a restarted run skips the regressions already done.
#
# unit = district: districts on one fixed map per storm, from code/17_districts.py (origin and
#   destination assigned through the census block). Outcome: registration address at follow-up
#   is in another house district. Address changes whose destination could not be geocoded are
#   counted as no district move.
# unit = county: same estimates as `07_estimate.R <event> margin`. Outcome: re-registered in
#   another county.
# Margin = Republican share of the two-party presidential vote (2016 for Florence, 2020 for
# Helene). A unit is "close" if the share is within h of 50 percent, h = 5 and 10 points.
#
# Usage: Rscript code/18_district_margin.R <event> [district|county]
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb) })
args <- commandArgs(trailingOnly = TRUE)
ev <- args[1]; unit <- if (length(args) >= 2) args[2] else "district"; bw <- 500
setFixest_nthreads(2); setDTthreads(2)
DATA_ROOT <- Sys.getenv("VOTING_DATA_ROOT", "C:/voting_data")
PROC <- file.path(DATA_ROOT, "processed"); INTER <- file.path(DATA_ROOT, "intermediate")
TAB <- "output/tables"
pre <- if (unit == "county") "margin" else "dmargin"
design <- list(
  florence = list(e = "fl", pre = list(c("20150101", "20160607"), c("20170101", "20180508")),
                  post = c("20180508", "20190910"), ctrl = "mt"),
  helene = list(e = "he", pre = list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514")),
                post = c("20240514", "20250909"), ctrl = NULL))[[ev]]
e <- design$e
if (unit == "county") {
  V <- fread(file.path(TAB, "county_pres_votes.csv"))
  V <- V[, .(hd = as.character(county_id), rep2 = if (ev == "florence") rep2_2016 else rep2_2020)]
} else {
  V <- fread(file.path(TAB, sprintf("district_votes_%s.csv", ev)), colClasses = list(character = "hd"))[, .(hd, rep2)]
}
con <- dbConnect(duckdb())
dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
ap <- file.path(INTER, "geocode", "addresspoints_flood.parquet")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE %s_fl > 0", ap, e))$county_id
load_cohort <- function(b, f) {
  ctrl <- if (!is.null(design$ctrl)) sprintf(", c.%s_fl AS ctrl_fl", design$ctrl) else ""
  if (unit == "county") {
    cols <- sprintf("c.cc_%1$s::INT AS chg, c.county_id AS hd_o, c.dcounty_%1$s AS hd_d", f); join <- ""
  } else {
    cols <- sprintf("c.addr_%s::INT AS chg, d.hd_o, d.hd_d", f)
    join <- sprintf("JOIN read_parquet('%s') d USING (pid)", file.path(PROC, sprintf("dist_%s_%s.parquet", ev, b)))
  }
  q <- sprintf("
    SELECT c.lat, c.lon, substr(c.geoid,1,12) AS bg, (c.%1$s_fl > 0.5)::INT AS flooded %2$s,
           c.party_cd, %3$s, '%4$s' AS cohort
    FROM read_parquet('%5$s') c %6$s
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous
      AND c.%1$s_fl >= 0 AND abs(c.%1$s_dist) <= %7$f
      AND c.county_id IN (%8$s)",
    e, ctrl, cols, b, file.path(PROC, sprintf("cohort_%s_%s.parquet", ev, b)), join, bw,
    paste0("'", study, "'", collapse = ","))
  d <- as.data.table(dbGetQuery(con, q))
  # keep only what the regressions need, in compact types
  d[, `:=`(v_o = V$rep2[match(hd_o, V$hd)], v_d = V$rep2[match(hd_d, V$hd)])]
  d[, mv := as.integer(chg == 1 & !is.na(v_d) & hd_d != hd_o)]
  d[, pg := fifelse(party_cd == "REP", 2L, fifelse(party_cd == "DEM", 1L, 0L))]
  d[, known := as.integer(chg == 1 & !is.na(v_d))]
  d[, c("hd_o", "hd_d", "party_cd") := NULL]
  d
}
D <- rbindlist(lapply(c(design$pre, list(design$post)), function(x) load_cohort(x[1], x[2])))
dbDisconnect(con, shutdown = TRUE)
D[, loc := .GRP, by = .(lat, lon)]
D[, bgc := .GRP, by = .(bg, cohort)]
D[, tract := as.integer(factor(substr(bg, 1, 11)))]
D[, post := as.integer(cohort == design$post[1])]
diag <- D[, .(n = .N, n_change = sum(chg), pct_dest_known = 100 * sum(known) / sum(chg)), by = .(cohort, flooded)][order(cohort, flooded)]
print(diag); fwrite(diag, file.path(TAB, sprintf("%s_diag_%s.csv", pre, ev)))
cat("rows:", nrow(D), " origin unit missing:", D[is.na(v_o), .N], "\n")
D <- D[!is.na(v_o)]
D[, c("lat", "lon", "bg", "chg", "known") := NULL]
D[mv == 0, v_d := 0.5]
# own-party share of the two-party vote (Republicans: v; everyone else: 1 - v)
D[, `:=`(own_o = fifelse(pg == 2L, v_o, 1 - v_o), own_d = fifelse(pg == 2L, v_d, 1 - v_d))]
D[, c("v_o", "v_d") := NULL]
invisible(gc())
PN <- c(DEM = 1L, REP = 2L)
cat3 <- function(own, h) fifelse(abs(own - 0.5) < h, "close", fifelse(own > 0.5, "safe_own", "safe_other"))
ctrl <- if (!is.null(design$ctrl)) sprintf(" + i(cohort, ctrl_fl, ref = '%s')", design$pre[[1]][1]) else ""
k <- "flooded:post"

# ---- checkpointing ---------------------------------------------------------------------------
ckf <- file.path(TAB, sprintf(".ckpt_%s_%s.csv", pre, ev))
done <- if (file.exists(ckf)) fread(ckf) else data.table(key = character(0))
ckpt <- function(ky, fun) {
  if (ky %in% done$key) return(done[done$key == ky])
  r <- fun(); r[, key := ky]
  fwrite(r, ckf, append = file.exists(ckf)); invisible(gc())
  cat("done:", ky, "\n")
  r
}
est <- function(label, party, h, y) ckpt(paste("did", party, h, label), function() {
  D[, yy := y]
  m <- feols(as.formula(sprintf("yy ~ flooded:post%s | loc + bgc", ctrl)), data = D, cluster = ~tract,
             mem.clean = TRUE, lean = TRUE)
  data.table(kind = "did", event = ev, party = party, h_points = 100 * h, item = label,
             est_pp = coef(m)[k], se_pp = se(m)[k], p = pvalue(m)[k], n_flooded_party = NA_integer_)
})
u <- unit
res <- list(); desc <- list()
res[[1]] <- est(sprintf("moved to another %s (all voters)", u), "ALL", NA, 100 * D$mv)
for (p in names(PN)) res[[length(res) + 1]] <- est(sprintf("moved to another %s (all destinations)", u), p, NA, 100 * D$mv * (D$pg == PN[p]))
for (h in c(0.05, 0.10)) {
  D[, `:=`(dc = cat3(own_d, h), oc = cat3(own_o, h))]
  a <- D[mv == 1 & pg > 0, .N, by = .(period = fifelse(post == 1, "storm cohort", "pre-storm cohorts"), flooded,
                                     pg = names(PN)[pg], dc)]
  a[, pct := 100 * N / sum(N), by = .(period, flooded, pg)]
  a[, `:=`(event = ev, h_points = 100 * h)]; desc[[length(desc) + 1]] <- a[N >= 10]
  s0 <- D[mv == 1 & post == 0 & flooded == 1 & pg > 0, mean(dc == "close")]
  for (p in names(PN)) {
    res[[length(res) + 1]] <- est(sprintf("moved to a close %s", u), p, h, 100 * D$mv * (D$pg == PN[p]) * (D$dc == "close"))
    res[[length(res) + 1]] <- est(sprintf("moved to a %s safe for own party", u), p, h, 100 * D$mv * (D$pg == PN[p]) * (D$dc == "safe_own"))
  }
  res[[length(res) + 1]] <- est(sprintf("partisans: moves to close units minus benchmark share (%.1f%%) of all moves", 100 * s0),
                                "DEM+REP", h, 100 * D$mv * (D$pg > 0) * ((D$dc == "close") - s0))
  # effect on moving by type of ORIGIN unit, voters of party p only
  for (p in names(PN)) res[[length(res) + 1]] <- ckpt(paste("origin", p, h), function() {
    P <- D[pg == PN[p]]
    P[, yy := 100 * mv]
    P[, `:=`(t_close = flooded * post * (oc == "close"), t_own = flooded * post * (oc == "safe_own"), t_oth = flooded * post * (oc == "safe_other"))]
    keep <- c("t_close", "t_own", "t_oth")[c(P[, sum(t_close)], P[, sum(t_own)], P[, sum(t_oth)]) > 0]
    m <- feols(as.formula(sprintf("yy ~ %s%s | loc + bgc", paste(keep, collapse = " + "), ctrl)), data = P, cluster = ~tract,
               mem.clean = TRUE)
    nn <- P[post == 1 & flooded == 1, .N, by = oc]
    o <- data.table(kind = "origin", event = ev, party = p, h_points = 100 * h, item = sub("t_", "", keep),
                    est_pp = coef(m)[keep], se_pp = se(m)[keep], p = pvalue(m)[keep],
                    n_flooded_party = nn$N[match(c(t_close = "close", t_own = "safe_own", t_oth = "safe_other")[keep], nn$oc)])
    if (all(c("t_close", "t_own") %in% keep)) {
      vc <- vcov(m); dd <- coef(m)["t_close"] - coef(m)["t_own"]
      sd <- sqrt(vc["t_close", "t_close"] + vc["t_own", "t_own"] - 2 * vc["t_close", "t_own"])
      o <- rbind(o, data.table(kind = "origin", event = ev, party = p, h_points = 100 * h, item = "close minus own", est_pp = dd,
                               se_pp = sd, p = 2 * pnorm(-abs(dd / sd)), n_flooded_party = NA_integer_))
    }
    rm(P, m); o
  })
}
res <- rbindlist(res, fill = TRUE); desc <- rbindlist(desc)[order(h_points, pg, dc, period, flooded)]
options(width = 220)
print(desc); print(res[kind == "did", -c("key", "kind", "n_flooded_party")]); print(res[kind == "origin", -c("key", "kind")])
fwrite(desc, file.path(TAB, sprintf("%s_movers_%s.csv", pre, ev)))
fwrite(res[kind == "did", .(event, party, h_points, outcome = item, est_pp, se_pp, p)], file.path(TAB, sprintf("%s_did_%s.csv", pre, ev)))
fwrite(res[kind == "origin", .(event, party, h_points, origin = item, est_pp, se_pp, p, n_flooded_party)],
       file.path(TAB, sprintf("%s_origin_%s.csv", pre, ev)))
file.remove(ckf)
