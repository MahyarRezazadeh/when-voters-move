# Helene: summary statistics of the outcome variables and number of observations, by 16-month window (2026-11-22).
# Table A (Result 1, household responses): households of 2-6 registered adults within 500 m of the flood boundary in the
#   23 study counties (same sample as 34_hh_response_source.R); share split, share moved together, by flooded/unflooded.
#   Adult changed address: rates and counts printed by 49_moved_es.R (logs/49_moved_es.log), read from the log.
# Table B (Result 2, sorting): flooded adults 18+ who moved to a new block group (same sample as 58_sorting_split_vs_family.R);
#   destination block-group characteristics, mean and SD, by group (split / moved with household).
# Outputs: output/tables/sumstats_hh_response_helene.csv, sumstats_sorting_helene.csv, slides/tab_sumstats_*.tex
# Usage: Rscript code/64_summary_stats.R
suppressPackageStartupMessages({ library(data.table); library(duckdb) })
setDTthreads(2); options(width = 220)
PROC <- "C:/voting_data/processed"; INTER <- "C:/voting_data/intermediate"; RAW <- "C:/voting_data/raw"; TAB <- "output/tables"
WIN <- list(c("20210101", "20220517"), c("20220517", "20230912"), c("20230101", "20240514"), c("20240514", "20250909"))
WL <- c("20210101" = "2021--22", "20220517" = "2022--23", "20230101" = "2023--24", "20240514" = "2024--25 (Helene)")
con <- dbConnect(duckdb()); dbExecute(con, "PRAGMA memory_limit='1GB'"); dbExecute(con, "PRAGMA threads=2")
study <- dbGetQuery(con, sprintf("SELECT DISTINCT county_id FROM read_parquet('%s') WHERE he_fl > 0", file.path(INTER, "geocode", "addresspoints_flood.parquet")))$county_id
sl <- paste0("'", study, "'", collapse = ",")
pq <- function(stem, b) sprintf("read_parquet('%s')", file.path(PROC, sprintf("%s_helene_%s.parquet", stem, b)))

# ---------------- Table A: household responses (household level) ----------------
H <- rbindlist(lapply(WIN, function(w) { b <- w[1]; as.data.table(dbGetQuery(con, sprintf("
    SELECT max((c.he_fl > 0.5)::INT) AS flooded, any_value(d.n_stay) AS n_stay, any_value(d.n_move) AS n_move,
           any_value(d.n_dest) AS n_dest, '%s' AS cohort
    FROM %s c JOIN %s h USING (pid) JOIN %s i USING (pid) JOIN %s d ON d.hb = i.hb
    WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl >= 0
      AND abs(c.he_dist) <= 500 AND c.county_id IN (%s) AND h.hh_n BETWEEN 2 AND 6
    GROUP BY i.hb", b, pq("cohort", b), pq("hh", b), pq("hhid", b), pq("hhd", b), sl))) }))
for (v in c("n_stay", "n_move")) set(H, which(is.na(H[[v]])), v, 0)
H[, `:=`(split = as.integer((n_stay >= 1 & n_move >= 1) | (n_stay == 0 & !is.na(n_dest) & n_dest >= 2)),
         tog = as.integer(n_stay == 0 & n_move >= 2 & !is.na(n_dest) & n_dest == 1))]
A <- H[, .(n_hh = .N, split = 100 * mean(split), tog = 100 * mean(tog)), by = .(cohort, flooded)]
# adult changed address: from logs/49_moved_es.log (rate in %, n adults)
lg <- readLines("logs/49_moved_es.log"); i0 <- grep("== moved", lg)[1]; i1 <- grep("== gone", lg)[1]
rows <- grep("^\\s*[0-9]+:\\s+20[0-9]{6}\\s+[01]\\s", lg[i0:i1], value = TRUE)
mv <- rbindlist(lapply(rows, function(l) { x <- strsplit(trimws(sub("^[0-9]+:", "", l)), "\\s+")[[1]]
  data.table(cohort = x[1], flooded = as.integer(x[2]), moved = as.numeric(x[3]), n_ad = as.integer(x[4])) }))
A <- merge(A, mv, by = c("cohort", "flooded"))
setorder(A, cohort, -flooded); print(A)
fwrite(A, file.path(TAB, "sumstats_hh_response_helene.csv"))

# ---------------- Table B: sorting outcomes (flooded movers) ----------------
D <- rbindlist(lapply(WIN, function(w) as.data.table(dbGetQuery(con, sprintf("
  SELECT CASE WHEN c.age BETWEEN 18 AND 110 THEN c.age END AS age, k.bg_b, k.bg_f,
         CASE WHEN h.together = 1 THEN 'together' WHEN h.split = 1 THEN 'split' END AS grp, '%s' AS cohort
  FROM read_parquet('%s/cohort_helene_%s.parquet') c JOIN read_parquet('%s/hh_helene_%s.parquet') h USING (pid)
       JOIN read_parquet('%s/nb_helene_%s.parquet') k USING (pid)
  WHERE c.tier IS NOT NULL AND c.tier <> 'unmatched' AND NOT c.ambiguous AND c.he_fl > 0.5 AND abs(c.he_dist) <= 500
    AND c.county_id IN (%s) AND h.hh_n BETWEEN 2 AND 6 AND c.addr_%s AND k.bg_f IS NOT NULL AND k.bg_f <> k.bg_b
    AND (h.together = 1 OR h.split = 1)", w[1], PROC, w[1], PROC, w[1], PROC, w[1], sl, w[2])))))
CB <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT * FROM read_parquet('%s/bgcomp_%s.parquet')", PROC, d)))[, cohort := d]))
AG <- rbindlist(lapply(sapply(WIN, `[`, 1), function(d) as.data.table(dbGetQuery(con, sprintf("SELECT bg, sum(n) AS na, sum(age_sum) AS asum FROM read_parquet('%s/bgage_%s.parquet') GROUP BY 1", PROC, d)))[, cohort := d]))
dbDisconnect(con, shutdown = TRUE)
rd <- function(f) { x <- fread(f, sep = "|", colClasses = "character"); x[grepl("^1500000US37", GEO_ID), .(bg = sub("1500000US", "", GEO_ID), v = as.numeric(get(names(x)[2])))] }
ACS <- rd(file.path(RAW, "acs/acsdt5y2023-b19013.dat"))[, .(bg, inc = fifelse(v > 0, v / 1000, NA_real_))]
ED <- fread(file.path(RAW, "acs/acsdt5y2023-b15003.dat"), sep = "|", colClasses = "character")[grepl("^1500000US37", GEO_ID)]
ED <- ED[, .(bg = sub("1500000US", "", GEO_ID), tot = as.numeric(B15003_E001),
             ba = as.numeric(B15003_E022) + as.numeric(B15003_E023) + as.numeric(B15003_E024) + as.numeric(B15003_E025))]
ACS <- merge(ACS, ED[tot > 0, .(bg, sh_ba = 100 * ba / tot)], by = "bg", all = TRUE)
NB <- merge(CB, AG, by = c("bg", "cohort"))[n >= 30, .(bg, cohort, sh_dem = 100 * n_dem / n, sh_rep = 100 * n_rep / n,
       sh_white = 100 * n_white / n, sh_black = 100 * n_black / n, sh_hisp = 100 * n_hisp / n, mage = asum / na)]
NB <- merge(NB, ACS[, .(bg, inc, sh_ba)], by = "bg", all.x = TRUE)
o <- NB[, .(bg_b = bg, cohort)]                    # origin must have 30+ adults (as in 58)
f <- copy(NB); setnames(f, "bg", "bg_f")
D <- merge(merge(D, o, by = c("bg_b", "cohort")), f, by = c("bg_f", "cohort"))[!is.na(age) & !is.na(grp)]
V <- c(sh_dem = "Share Democrat (\\%)", sh_rep = "Share Republican (\\%)", sh_white = "Share White (\\%)", sh_black = "Share Black (\\%)",
       sh_hisp = "Share Hispanic (\\%)", mage = "Mean age of adults (years)", inc = "Median household income (\\$1,000)",
       sh_ba = "Bachelor's degree or more (\\%)")
B <- rbindlist(lapply(names(V), function(v) D[, .(var = v, mean = mean(get(v), na.rm = TRUE), sd = sd(get(v), na.rm = TRUE),
                                                   n = sum(!is.na(get(v)))), by = .(cohort, grp)]))
B <- rbind(B, D[, .(var = "N", mean = .N, sd = NA_real_, n = .N), by = .(cohort, grp)])
setorder(B, cohort, -grp); print(dcast(B, var ~ cohort + grp, value.var = "mean"), digits = 3)
fwrite(B, file.path(TAB, "sumstats_sorting_helene.csv"))

# ---------------- LaTeX (slides) ----------------
fm <- function(x, d = 1) formatC(x, format = "f", digits = d, big.mark = ",")
ws <- names(WL)
hdr <- function(sub) paste0(" & ", paste0("\\multicolumn{2}{c}{", WL, "}", collapse = " & "), " \\\\\n",
  paste0("\\cmidrule(lr){", seq(2, 8, 2), "-", seq(3, 9, 2), "}", collapse = ""), "\n & ", paste(rep(sub, 4), collapse = " & "), " \\\\\n\\midrule\n")
# Table A
ra <- function(lab, v, d) paste0(lab, " & ", paste(sapply(ws, function(w) paste(fm(A[cohort == w & flooded == 1, get(v)], d), fm(A[cohort == w & flooded == 0, get(v)], d), sep = " & ")), collapse = " & "), " \\\\\n")
ta <- paste0("\\begin{tabular}{l", strrep("rr", 4), "}\n\\toprule\n", hdr("Flooded & Unflooded"),
  ra("Adult changed address (\\%)", "moved", 1), ra("Household split (\\%)", "split", 1), ra("Household moved together (\\%)", "tog", 1),
  "\\midrule\n", ra("Adults (N)", "n_ad", 0), ra("Households (N)", "n_hh", 0), "\\bottomrule\n\\end{tabular}\n")
writeLines(ta, "slides/tab_sumstats_hh.tex")
# Table B: mean with SD below
gl <- c("split", "together")
rb <- function(v) { m <- paste0(V[[v]], " & ", paste(sapply(ws, function(w) paste(sapply(gl, function(g) fm(B[cohort == w & grp == g & var == v, mean])), collapse = " & ")), collapse = " & "), " \\\\\n")
  s <- paste0(" & ", paste(sapply(ws, function(w) paste(sapply(gl, function(g) paste0("(", fm(B[cohort == w & grp == g & var == v, sd]), ")")), collapse = " & ")), collapse = " & "), " \\\\\n")
  paste0(m, s) }
tb <- paste0("\\begin{tabular}{l", strrep("rr", 4), "}\n\\toprule\n", hdr("Split & With hh"), paste(sapply(names(V), rb), collapse = ""),
  "\\midrule\nFlooded movers (N) & ", paste(sapply(ws, function(w) paste(sapply(gl, function(g) fm(B[cohort == w & grp == g & var == "N", mean], 0)), collapse = " & ")), collapse = " & "),
  " \\\\\n\\bottomrule\n\\end{tabular}\n")
writeLines(tb, "slides/tab_sumstats_sorting.tex")
