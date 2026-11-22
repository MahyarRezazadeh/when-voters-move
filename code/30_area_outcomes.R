# Area-level outcomes after Hurricane Helene (exploratory, 2026-11-19). Public aggregate data only.
# Treated: 13 NC counties with at least 1% of address points inside the NCEM Helene inundation layer.
# Control: the 77 NC counties with no address point in the layer. The 10 counties in between are dropped.
# (A satellite flood map was tested and could not be used; see PROJECT_STATUS.md.)
# Event study: y ~ sum_k b_k * treated x 1[period = k] | unit FE + period FE, standard errors clustered by county.
# Counts: Poisson with log population offset, reported as percent change 100*(exp(b)-1). Logs: 100*b.
# Usage: Rscript code/30_area_outcomes.R [hospital|labor|conflict|education|mental|all]
suppressPackageStartupMessages({ library(data.table); library(fixest); library(ggplot2) })
setFixest_nthreads(2); setFixest_notes(FALSE)
what <- commandArgs(trailingOnly = TRUE); if (!length(what)) what <- "all"
on <- function(x) what %in% c(x, "all")
PRJ <- getwd()  # run from the project root
RAW <- "C:/voting_data/raw"
TAB <- file.path(PRJ, "output/tables"); FIG <- file.path(PRJ, "output/figures/helene_outcomes")
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

ex <- fread(file.path(TAB, "helene_county_exposure.csv"))[group != "partial"]
ex[, tr := as.integer(group == "treated")]
med <- ex[tr == 1, median(physicians_total_active_per_10k)]
ex[, doc := fifelse(tr == 0, "control", fifelse(physicians_total_active_per_10k >= med, "high", "low"))]
ex <- ex[, .(county_fips, tr, doc, pct_flooded)]
pop <- fread(file.path(RAW, "hospital/county_physicians.csv"))[year >= 2013, .(county_fips, year, population)]
pop <- pop[CJ(county_fips = unique(pop$county_fips), year = 2013:2026), on = .(county_fips, year)]
# population is missing in the source for 2020-21 and not yet published for 2026: linear interpolation / last value
pop[, population := approx(year[!is.na(population)], population[!is.na(population)], xout = year, rule = 2)$y, by = county_fips]

# ---- estimation helpers ----------------------------------------------------------------------
es <- function(d, y, ref, fam = "ols", off = NULL, outcome, series = "All treated counties", unit = "county_fips",
               fe = paste(unit, "+ t"), scale = 100, w = NULL, post = NULL, drop = NULL) {
  d <- d[is.finite(get(y))]
  if (!is.null(off)) d <- d[is.finite(get(off)) & get(off) > 0]
  fit <- function(f, dd) {
    if (fam == "pois") fepois(f, dd, offset = if (!is.null(off)) as.formula(paste0("~log(", off, ")")), cluster = ~county_fips)
    else feols(f, dd, cluster = ~county_fips, weights = if (!is.null(w)) as.formula(paste0("~", w)))
  }
  m <- fit(as.formula(sprintf("%s ~ i(t, tr, ref = %d) | %s", y, ref, fe)), d)
  du <- if (length(m$obs_selection)) d[obs(m)] else d
  ct <- as.data.table(coeftable(m), keep.rownames = "term")[grepl("^t::", term)]
  ct[, t := as.integer(sub("^t::(-?[0-9]+):tr$", "\\1", term))]
  tf <- function(b, s) if (fam == "pois") list(100 * (exp(b) - 1), 100 * exp(b) * s) else list(scale * b, scale * s)
  o <- ct[, c(list(t = t), setNames(tf(Estimate, `Std. Error`), c("est", "se")))]
  o <- rbind(o, data.table(t = ref, est = 0, se = 0))[order(t)]
  o[, `:=`(outcome = outcome, series = series, n_obs = nrow(du), tr_units = uniqueN(du[tr == 1][[unit]]),
           ct_units = uniqueN(du[tr == 0][[unit]]), tr_obs = du[tr == 1, .N], ct_obs = du[tr == 0, .N], did = NA_real_, did_se = NA_real_)]
  if (!is.null(post)) {                         # pooled difference-in-differences over fully post-storm periods
    dd <- d[!(t %in% drop)]; dd[, trpost := tr * (t %in% post)]
    m2 <- fit(as.formula(sprintf("%s ~ trpost | %s", y, fe)), dd)
    z <- tf(coef(m2)[["trpost"]], se(m2)[["trpost"]]); o[, `:=`(did = z[[1]], did_se = z[[2]])]
  }
  o[]
}
split3 <- function(d, ...) rbind(es(d, ...), es(d[doc != "high"], ..., series = "Fewer physicians per capita"),
                                 es(d[doc != "low"], ..., series = "More physicians per capita"))
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"
PAL <- c("#eb6834", "#2a78d6", "#3d8f5a", "#7a4fa3")
plot_es <- function(co, title, ylab, cap, file, xlab, labs, h = 5, w = 7.4, ncol = 2, every = 1, storm = NULL) {
  co <- copy(co)[, `:=`(lo = est - 1.96 * se, hi = est + 1.96 * se, ref = se == 0)]
  co[ref == TRUE, `:=`(lo = NA, hi = NA)]
  co[, outcome := factor(outcome, levels = unique(outcome))][, series := factor(series, levels = unique(series))]
  ns <- nlevels(co$series); pd <- position_dodge(width = if (ns > 1) 0.5 else 0)
  br <- sort(unique(co$t)); br <- br[seq(1, length(br), by = every)]
  p <- ggplot(co, aes(t, est, colour = series, group = series)) +
    geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3)
  if (!is.null(storm)) p <- p + geom_vline(xintercept = storm, colour = INK2, linewidth = 0.3, linetype = "dashed")
  p <- p + geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.45, position = pd, na.rm = TRUE) +
    geom_point(size = 1.7, position = pd) +
    scale_colour_manual(values = PAL[seq_len(ns)], name = NULL) +
    scale_x_continuous(breaks = br, labels = labs[as.character(br)]) +
    facet_wrap(~outcome, scales = "free_y", ncol = ncol) +
    labs(title = title, x = xlab, y = ylab, caption = cap) +
    theme_minimal(base_size = 9) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3),
          axis.line.x = element_line(colour = INK2, linewidth = 0.3), axis.text = element_text(colour = INK2),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          strip.text = element_text(colour = INK, face = "bold", hjust = 0, size = 8.5),
          legend.position = if (ns > 1) "top" else "none", legend.justification = "left", legend.margin = margin(0, 0, 0, 0),
          plot.title = element_text(colour = INK, face = "bold", size = 10),
          plot.caption = element_text(colour = INK2, hjust = 0, size = 6.8, lineheight = 1.05),
          plot.caption.position = "plot", plot.margin = margin(6, 10, 6, 6))
  for (e in c("pdf", "png")) tryCatch(
    if (e == "pdf") ggsave(file.path(FIG, paste0(file, ".pdf")), p, width = w, height = h, device = cairo_pdf)
    else ggsave(file.path(FIG, paste0(file, ".png")), p, width = w, height = h, dpi = 300, bg = "white"),
    error = function(err) message("not written (file in use?): ", file, ".", e))
}
ncap <- function(co, unit = "counties") {       # sample sizes per panel and series, for the caption
  u <- unique(co[, .(outcome, series, tr_units, ct_units, tr_obs, ct_obs)])
  paste0("Sample sizes. ", paste(sprintf("%s%s: treated %s %s (%s obs.), control %s %s (%s obs.)", u$outcome,
         ifelse(uniqueN(u$series) > 1, paste0(" [", u$series, "]"), ""), u$tr_units, unit, format(u$tr_obs, big.mark = ","),
         u$ct_units, unit, format(u$ct_obs, big.mark = ",")), collapse = "; "), ".")
}
wrap <- function(x, n = 150) paste(strwrap(x, n), collapse = "\n")
BASE <- paste("Coefficients on treated county x period, relative to the reference period (coefficient 0, no interval);",
              "95% confidence intervals, standard errors clustered by county; unit and period fixed effects.",
              "Treated: 13 NC counties with at least 1% of addresses inside the state's Helene inundation layer.",
              "Control: 77 NC counties with none. Dashed line: storm (26-28 Sept 2024).")
save_co <- function(co, name) fwrite(co, file.path(TAB, paste0("area_es_", name, ".csv")))

# ================================ 1. HOSPITALS ================================================
if (on("hospital")) {
  cr <- fread(file.path(RAW, "hospital/county_residence_panel.csv"))
  cr[, t := as.integer(sub("FY", "", fy))]
  cr <- merge(merge(cr[t >= 2016], ex, by = "county_fips"), pop[, .(county_fips, t = year, population)], by = c("county_fips", "t"))
  setnames(cr, c("ed_visits_by_county_residents", "acute_inpatient_admissions_by_county_residents"), c("ed", "adm"))
  cr[county_fips == 37021 & t == 2019, ed := NA]   # Mission Hospital FY2019 ED count is implausible in the source
  A <- list(ref = 2024, fam = "pois", off = "population", post = 2025)
  co <- rbind(do.call(split3, c(list(cr, "ed", outcome = "ED visits by county residents (% change)"), A)),
              do.call(split3, c(list(cr, "adm", outcome = "Acute inpatient admissions by county residents (% change)"), A)))
  hp <- fread(file.path(RAW, "hospital/hospital_panel.csv"))
  hp <- hp[grepl("^CMS Timely", source) & !is.na(op18b_median_min_ed_arrival_to_departure_discharged)]
  hp <- hp[(substr(period_label, 6, 10) == "01-01" & substr(period_label, 17, 21) == "12-31") | period_label == "2020-07-01_2020-12-31"]
  hp[, `:=`(t = as.integer(substr(period_label, 1, 4)), mins = op18b_median_min_ed_arrival_to_departure_discharged)]
  hp <- merge(hp, ex, by = "county_fips")
  c2 <- split3(hp, "mins", ref = 2023, outcome = "Median minutes in ED before discharge home, by hospital (minutes)", unit = "ccn",
               scale = 1, post = 2025, drop = 2024)
  c2[, t := t + 1L]                                # shift so calendar 2023 lines up with fiscal 2024 on the common axis
  co <- rbind(co, c2); save_co(co, "hospital")
  labs <- c(setNames(paste0("FY", 2016:2025, "\n(CY", 2015:2024, ")"), 2016:2025), "2026" = "\n(CY2025)")
  u <- unique(co[, .(outcome, series, tr_units, ct_units, tr_obs, ct_obs)])
  cap <- paste(wrap(paste(BASE, "Top two panels: Poisson with population offset, county by fiscal year (October-September; reference FY2024,",
        "which ends 30 Sept 2024). Bottom panel: hospitals, calendar-year windows (label in brackets; reference 2023; 2024 contains the storm; 2020 is July-December).",
        "Physician split: treated counties above or below the treated-county median of", sprintf("%.1f", med),
        "active physicians per 10,000 residents in 2023, each against all control counties.")),
        wrap(paste0("Sample sizes (all treated counties). ED visits: treated ", u[1, tr_units], " counties (", u[1, tr_obs], " obs.), control ", u[1, ct_units],
        " counties (", u[1, ct_obs], " obs.). Admissions: treated ", u[4, tr_units], " (", u[4, tr_obs], "), control ", u[4, ct_units], " (", u[4, ct_obs],
        "). ED minutes: treated ", u[7, tr_units], " hospitals (", u[7, tr_obs], " obs.), control ", u[7, ct_units], " hospitals (", u[7, ct_obs],
        " obs.). Split groups: ", u[2, tr_units], " counties with fewer physicians, ", u[3, tr_units], " with more; ", u[8, tr_units], " and ", u[9, tr_units], " hospitals.")),
        "Sources: NC DHSR patient origin reports; CMS Timely and Effective Care (OP-18b); UNC Sheps Center health workforce data.", sep = "\n")
  plot_es(co, "Helene and hospital use: emergency visits, admissions and time in the emergency department",
          "Treated minus control", cap, "es_hospital", "Fiscal year (calendar-year window for ED minutes)", labs, h = 7.6, ncol = 1, storm = 2024.5)
}

# ================================ 2. LABOUR MARKET BY SEX =====================================
if (on("labor")) {
  q <- fread(file.path(RAW, "labor/qwi_nc_county_sex_2019on.csv"))[(industry == "00" | industry == 0) & year <= 2025]
  q[, `:=`(county_fips = as.integer(geography), t = (year - 2019L) * 4L + quarter)]
  q <- merge(q, ex, by = "county_fips")
  q[, `:=`(lemp = log(Emp), learn = log(EarnS), hire = HirA / Emp, sep = Sep / Emp)]
  OUT <- c(lemp = "Employment, private-sector jobs (%)", learn = "Average monthly earnings (%)",
           hire = "Hires per 100 jobs (points)", sep = "Separations per 100 jobs (points)")
  gap <- dcast(q, county_fips + t + tr + doc ~ sex, value.var = names(OUT))
  for (v in names(OUT)) gap[, (v) := get(paste0(v, "_2")) - get(paste0(v, "_1"))]
  REF <- 22L; POST <- 24:28; DROP <- 23L          # reference 2024Q2; 2024Q3 contains the storm
  co <- rbindlist(lapply(names(OUT), function(v) rbind(
    es(q[sex == 1], v, REF, outcome = OUT[[v]], series = "Men", post = POST, drop = DROP),
    es(q[sex == 2], v, REF, outcome = OUT[[v]], series = "Women", post = POST, drop = DROP),
    es(gap, v, REF, outcome = OUT[[v]], series = "Gap (women minus men)", post = POST, drop = DROP))))
  save_co(co, "labor_qwi")
  labs <- setNames(paste0(rep(2019:2025, each = 4), "Q", 1:4), 1:28)
  u <- unique(co[series == "Men", .(tr_units, ct_units, tr_obs, ct_obs)])[1]
  cap <- paste(wrap(paste(BASE, "County by quarter, 2019Q1-2025Q4; reference 2024Q2. Employment and earnings in logs x 100.",
        "Jobs are counted in the county of the workplace; private sector; earnings are for workers employed the full quarter. The source has no hours.")),
        wrap(sprintf("Sample sizes, each sex: treated %d counties (%s county-quarters), control %d counties (%s county-quarters). Source: Census Bureau Quarterly Workforce Indicators.",
                     u$tr_units, format(u$tr_obs, big.mark = ","), u$ct_units, format(u$ct_obs, big.mark = ","))), sep = "\n")
  plot_es(co[series != "Gap (women minus men)"], "Helene and the labour market, by sex", "Treated minus control", cap, "es_labor_by_sex",
          "Quarter", labs, h = 6.4, every = 2, storm = 23)
  plot_es(co[series == "Gap (women minus men)"], "Helene and the gender gap in the labour market (women minus men)",
          "Treated minus control", cap, "es_labor_gender_gap", "Quarter", labs, h = 6.2, every = 2, storm = 23)

  # ---- weekly hours, employment and hourly wage from ACS microdata (PUMA, 2019-2024) ----------
  f <- file.path(RAW, "labor/pums_nc_25_64.csv")
  if (file.exists(f)) {
    p <- fread(f, colClasses = list(character = "PUMA"))
    pm <- function(file, vint) { x <- fread(file.path(RAW, "labor", file), colClasses = "character")[STATEFP == "37"]
      x[, county_fips := 37000L + as.integer(COUNTYFP)]
      x <- merge(x, fread(file.path(TAB, "helene_county_exposure.csv"))[, .(county_fips, group)], by = "county_fips")
      x[, .(sh_tr = mean(group == "treated"), sh_any = mean(group != "control"), vint = vint), by = .(PUMA = PUMA5CE)] }
    pu <- rbind(pm("tract_puma_2010.txt", 2010L), pm("tract_puma_2020.txt", 2020L))
    pu <- pu[sh_tr >= 0.5 | sh_any == 0][, tr := as.integer(sh_tr >= 0.5)]
    p[, vint := fifelse(year <= 2021, 2010L, 2020L)]
    p <- merge(p, pu[, .(PUMA, vint, tr)], by = c("PUMA", "vint"))
    p[, `:=`(county_fips = paste(vint, PUMA), t = year, fem = as.integer(SEX == 2), emp100 = 100 * emp, lhw = log(hw))]
    p[, `:=`(pid = county_fips)][, trsave := tr]
    O2 <- c(hrs = "Usual weekly hours, employed (hours)", emp100 = "Employed (percentage points)", lhw = "Hourly wage (%)")
    one <- function(v, s, ser) es(p[SEX == s], v, 2023, outcome = O2[[v]], series = ser, fe = "pid + t + AGEP", w = "PWGTP",
                                  scale = if (v == "lhw") 100 else 1)
    gp <- function(v) { d <- copy(p); d[, tr := trsave * fem]
      o <- es(d, v, 2023, outcome = O2[[v]], series = "Gap (women minus men)", fe = "pid^fem + t^fem + pid^t + AGEP", w = "PWGTP",
              scale = if (v == "lhw") 100 else 1)
      o[, `:=`(tr_units = uniqueN(p[trsave == 1, pid]), ct_units = uniqueN(p[trsave == 0, pid]),
               tr_obs = p[trsave == 1 & is.finite(get(v)), .N], ct_obs = p[trsave == 0 & is.finite(get(v)), .N])]; o }
    c3 <- rbindlist(lapply(names(O2), function(v) rbind(one(v, 1, "Men"), one(v, 2, "Women"), gp(v))))
    save_co(c3, "labor_acs")
    u <- unique(c3[series == "Gap (women minus men)", .(outcome, tr_units, ct_units, tr_obs, ct_obs)])
    cap <- paste(wrap(paste("Coefficients on treated area x survey year, relative to 2023; 95% confidence intervals, standard errors clustered by area.",
          "Unit: person aged 25-64 in the American Community Survey public-use file; areas are public-use microdata areas (2010 boundaries",
          "to 2021, 2020 boundaries from 2022; no 2020 survey). Treated: areas with at least half of their tracts in the 13 treated counties.",
          "Area, year and age fixed effects; person weights. The 2024 survey was collected January-December 2024, so only about one quarter of its",
          "interviews follow the storm; the 2025 file is not yet published. This figure is therefore mainly a check of pre-storm trends.")),
          wrap(paste0("Sample sizes (men and women together). ", paste(sprintf("%s: treated %d areas (%s persons), control %d areas (%s persons)", u$outcome, u$tr_units,
                 format(u$tr_obs, big.mark = ","), u$ct_units, format(u$ct_obs, big.mark = ",")), collapse = "; "), ".")), sep = "\n")
    plot_es(c3, "Weekly hours, employment and hourly wage by sex (survey data; 2024 is only partly after the storm)",
            "Treated minus control", cap, "es_labor_hours_acs", "Survey year", setNames(as.character(2019:2024), 2019:2024), h = 7.4, ncol = 1, storm = 2023.75)
  }
}

# ================================ 3. CONFLICT AT HOME =========================================
if (on("conflict")) {
  rd <- function(f, ms) merge(fread(file.path(RAW, "courts", f))[measure %in% ms], ex, by = "county_fips")
  fy <- function(d) { d[, t := as.integer(substr(period, 3, 6)) + 1L]; merge(d, pop[, .(county_fips, t = year, population)], by = c("county_fips", "t")) }
  dv <- fy(rd("panel_cfwyi_dv_sa_services_county_fy.csv", c("dv_clients_served", "dv_calls")))[t >= 2017 & t <= 2026]
  cps <- fy(rd("panel_cip_cps_investigated_county_fy.csv", c("cps_children_investigated_total",
            "cps_children_investigated_mostsevere_finding_abuse", "cps_children_investigated_mostsevere_finding_neglect",
            "cps_children_investigated_mostsevere_finding_abuse_and_neglect")))[t >= 2017 & t <= 2025]
  cps[, m2 := fifelse(measure == "cps_children_investigated_total", "inv", "sub")]
  cps <- cps[, .(value = sum(value)), by = .(county_fips, t, tr, doc, population, measure = m2)]
  fo <- fy(rd("panel_cip_foster_first_entries_county_fy.csv", "foster_children_entering_custody_first_time"))[t >= 2017 & t <= 2025]
  A <- list(ref = 2024, fam = "pois", off = "population", post = 2025)
  co <- rbind(do.call(es, c(list(dv[measure == "dv_clients_served"], "value", outcome = "Domestic violence programme clients (% change)"), A)),
              do.call(es, c(list(dv[measure == "dv_calls"], "value", outcome = "Domestic violence crisis calls (% change)"), A)),
              do.call(es, c(list(cps[measure == "inv"], "value", outcome = "Children in investigated maltreatment reports (% change)"), A)),
              do.call(es, c(list(cps[measure == "sub"], "value", outcome = "Children with substantiated abuse or neglect (% change)"), A)),
              do.call(es, c(list(fo, "value", outcome = "Children entering foster care, first time (% change)"), A)))
  ar <- rd("panel_sbi_arrests_county_year.csv", "arrests_offenses_against_the_family_and_children")
  ar[, t := as.integer(substr(period, 3, 6))]; ar <- ar[t >= 2019]; ar <- merge(ar, pop[, .(county_fips, t = year, population)], by = c("county_fips", "t"))
  c2 <- es(ar, "value", 2023, "pois", "population", outcome = "Arrests, family offences (% change; calendar year)", post = 2025, drop = 2024)
  c2[, t := t + 1L]
  co <- rbind(co, c2); save_co(co, "conflict_annual")
  labs <- setNames(paste0(2015:2025, "-", substr(2016:2026, 3, 4)), 2016:2026)
  cap <- paste(wrap(paste(BASE, "Poisson with population offset, county by state fiscal year (July-June); reference 2023-24, the last year before the storm;",
        "2024-25 contains the storm from its third month. Arrests are by calendar year, plotted one position to the right (reference 2023; 2024 contains the storm).",
        "Programme clients and calls are counted where the agency is located. These are county totals: they cannot separate households that moved together from households that split.")),
        wrap(ncap(co)), "Sources: NC Council for Women and Youth Involvement; UNC-CH child welfare indicators (NC DSS); NC State Bureau of Investigation.", sep = "\n")
  plot_es(co, "Helene and conflict at home: domestic violence services, child maltreatment, family-offence arrests",
          "Treated minus control (% change)", cap, "es_home_conflict", "State fiscal year", labs, h = 8.6, storm = 2024.5)

  mo <- rd("panel_sbi_offenses_county_month.csv", c("offenses_other_assaults_not_aggravated", "offenses_aggravated_assault"))
  mo[, `:=`(yr = as.integer(substr(period, 1, 4)), mth = as.integer(substr(period, 6, 7)))]
  bad <- mo[measure == "offenses_other_assaults_not_aggravated", .(z = mean(value == 0)), by = county_fips][z > 0.25, county_fips]
  mo <- mo[!(county_fips %in% bad)]
  mo <- mo[, .(value = sum(value)), by = .(county_fips, tr, doc, measure, yr, t = (yr - 2018L) * 4L + (mth - 1L) %/% 3L + 1L)]
  mo <- merge(mo, pop[, .(county_fips, yr = year, population)], by = c("county_fips", "yr"))
  REF <- 26L; POST <- 28:32; DROP <- 27L
  c4 <- rbind(es(mo[measure == "offenses_other_assaults_not_aggravated"], "value", REF, "pois", "population", outcome = "Simple assaults reported to police (% change)", post = POST, drop = DROP),
              es(mo[measure == "offenses_aggravated_assault"], "value", REF, "pois", "population", outcome = "Aggravated assaults reported to police (% change)", post = POST, drop = DROP))
  save_co(c4, "conflict_assaults")
  cap <- paste(wrap(paste(BASE, "Poisson with population offset, county by quarter, 2018Q1-2025Q4; reference 2024Q2; 2024Q3 contains the storm.",
        "All assaults known to police, not only domestic ones.", length(bad), "counties with no simple assaults recorded in over a quarter of months (agencies not reporting) are dropped.")),
        wrap(ncap(c4)), "Source: NC State Bureau of Investigation, index offences by month.", sep = "\n")
  plot_es(c4, "Helene and assaults reported to police", "Treated minus control (% change)", cap, "es_assaults",
          "Quarter", setNames(paste0(rep(2018:2025, each = 4), "Q", 1:4), 1:32), h = 4.6, every = 2, storm = 27)
}

# ================================ 4. MENTAL HEALTH ============================================
if (on("mental")) {
  h <- fread(file.path(RAW, "mentalhealth/ncdhhs_ivpb_injury_selfharm_overdose_county_year_nc.csv"))
  h <- h[geo_level == "county" & (is.na(is_projected_partial_year) | is_projected_partial_year == FALSE)]
  h[, t := as.integer(substr(as.character(period_start), 1, 4))]
  h <- merge(h[t >= 2017 & t <= 2025], ex, by = "county_fips")
  h <- merge(h, pop[, .(county_fips, t = year, population)], by = c("county_fips", "t"))
  M <- c(ED_SYND_SUICIDAL = "ED visits for suicidal thoughts (% change)", ED_INTENTIONAL_SELFHARM = "ED visits for intentional self-harm (% change)",
         ED_SYND_ALCRELATED = "Alcohol-related ED visits (% change)", ED_ANY_MED_DRUG = "ED visits for drug overdose (% change)",
         DEATH_INTENTIONAL_SELFINFLICTED = "Suicide deaths (% change; no post-storm year yet)", ED_TOTAL = "All ED visits recorded by surveillance (% change)")
  co <- rbindlist(lapply(names(M), function(v) {
    d <- h[measure == v]; po <- if (2025 %in% d$t) 2025 else NULL
    es(d, "value", 2023, "pois", "population", outcome = M[[v]], post = po, drop = 2024) }))
  save_co(co, "mental")
  cap <- paste(wrap(paste(BASE, "Poisson with population offset, county of residence by calendar year; reference 2023; 2024 contains the storm (three months after it);",
        "2025 is the first full year after it. No county measure of diagnosed depression exists for any year after the storm: the published county",
        "depression rates are model-based and stop at 2023. The outcomes shown are emergency department visits and deaths that are related to mental",
        "distress, not depression itself. These are county totals: they cannot separate households that split from households that did not.")),
        wrap(ncap(co)), "Source: NC DHHS Injury and Violence Prevention Branch indicators (NC DETECT emergency department data; death certificates).", sep = "
")
  plot_es(co, "Helene and mental distress: emergency department visits and suicide deaths", "Treated minus control (% change)", cap,
          "es_mental_distress", "Calendar year", setNames(as.character(2017:2025), 2017:2025), h = 8.6, storm = 2024.2)
}

# ================================ 5. COLLEGE-GOING ============================================
if (on("education")) {
  m <- fread(file.path(RAW, "education/nc_myfuturenc_trends_county.csv"))[level == "county" & subgroup == "All"]
  m[, t := as.integer(substr(school_year, 1, 4)) + 1L]
  m <- merge(m[nchar(school_year) == 7 & t >= 2018 & (is.na(rate) | rate <= 100)], ex, by = "county_fips")  # two source rates above 100 are errors
  M <- c("FAFSA Completion Rate (as of Sep 30)" = "Seniors completing the federal aid form (points)",
         "UNC Enrollments (first-time undergrad)" = "Entering a UNC campus (points; entry term for 2025 unconfirmed)",
         "High School Graduation" = "Four-year high school graduation rate (points)",
         "ACT (19 or higher)" = "ACT 19 or higher (points; class of 2025 tested before storm)")
  co <- rbindlist(lapply(names(M), function(v) es(m[measure == v], "rate", 2024, outcome = M[[v]], scale = 1, post = 2025)))
  dr <- fread(file.path(RAW, "education/nc_dropout_counts_school.csv"))[level == "school"]
  dr <- dr[, .(value = sum(numerator, na.rm = TRUE)), by = .(county_fips, t = school_year_end)]
  dr <- merge(merge(dr, ex, by = "county_fips"), pop[, .(county_fips, t = year, population)], by = c("county_fips", "t"))
  co <- rbind(co, es(dr, "value", 2024, "pois", "population", outcome = "School dropouts, grades 1-13 (% change)", post = 2025))
  s <- merge(fread(file.path(RAW, "education/nc_src_college_enrollment_county.csv"))[cohort >= 2017], ex, by = "county_fips")
  s[, t := cohort]
  co <- rbind(co, es(s, "rate", 2024, outcome = "College enrolment after graduation (points; pre-storm only)", scale = 1))
  save_co(co, "education")
  cap <- paste(wrap(paste(BASE, "County by graduating class (school year ending in the year shown); reference: class of 2024, the last to finish high school before the storm.",
        "The class of 2025 was in grade 12 when the storm hit. Rates in percentage points, unweighted; dropouts by Poisson with population offset.",
        "College enrolment after graduation is published only up to the class of 2024, and no degree attainment can exist yet for the class of 2025;",
        "the aid form, graduation and dropout are the outcomes observed after the storm for the exposed class. The class of 2025 sat the ACT in spring 2024, before the storm, and the source does not say whether its 2024-25 UNC figure is autumn 2024 or autumn 2025 entry. The class of 2024 (reference) was the year of the federal aid form redesign.")),
        wrap(ncap(co)), "Sources: myFutureNC county attainment workbook (July 2026); NC Department of Public Instruction report cards and dropout reports.", sep = "
")
  plot_es(co, "Helene and the class of 2025: steps toward college", "Treated minus control", cap, "es_college_going",
          "Graduating class", setNames(as.character(2017:2025), 2017:2025), h = 8.8, storm = 2024.5)
}
