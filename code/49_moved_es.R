# Helene: did the flood displace adults? Adult-level event study, outcome = changed address within the 16-month window
# (addr_f; observed at follow-up), and not observed at follow-up (exit_f). Same sample rules as 48_nbage_movers.R but all adults.
# y ~ i(window, flooded, ref = 2023-24) | address FE + block group x window FE, tract-clustered SE. (2026-11-21)
suppressPackageStartupMessages({ library(data.table); library(fixest); library(duckdb) })
setFixest_nthreads(2); setDTthreads(2); setFixest_notes(FALSE); options(width = 200)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"
WIN <- list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0", file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
D <- rbindlist(lapply(WIN, function(w) as.data.table(dbGetQuery(con, sprintf("
  SELECT c.lat, c.lon, substr(c.geoid,1,12) AS bg, (c.he_fl > 0.5)::INT AS flooded, c.addr_%s::INT AS moved, c.exit_%s::INT AS gone, '%s' AS cohort
  FROM read_parquet('%s/cohort_helene_%s.parquet') c JOIN read_parquet('%s/hh_helene_%s.parquet') h USING (pid)
  WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0 AND abs(c.he_dist) <= 500
    AND c.county_id IN (%s) AND h.hh_n BETWEEN 1 AND 6", w[2], w[2], w[1], PROC, w[1], PROC, w[1], paste0("'", study, "'", collapse = ","))))))
dbDisconnect(con, shutdown = TRUE)
D[, loc := .GRP, by = .(lat, lon)][, bgc := .GRP, by = .(bg, cohort)][, tract := as.integer(factor(substr(bg, 1, 11)))]
for (v in c("moved", "gone")) { D[, yy := 100 * get(v)]
  m <- feols(yy ~ i(cohort, flooded, ref = "20230101") | loc + bgc, D, cluster = ~tract)
  cat("\n==", v, "(pp) ==  pre-trend joint p:", round(wald(m, "cohort::(20210101|20220517):flooded", print = FALSE)[["p"]], 3), "\n")
  print(round(coeftable(m)[, 1:4], 3))
  print(D[, .(rate = round(mean(yy), 2), n = .N), by = .(cohort, flooded)][order(cohort, flooded)]) }
