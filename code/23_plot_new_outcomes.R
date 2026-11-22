# Event-study figures for the new outcomes (exploratory, 2026-11-18). Same visual design as
# plot_event_study.R: small multiples by storm, pre-storm cohorts hollow, storm cohort filled,
# reference cohort pinned at 0, 95% confidence intervals from tract-clustered standard errors.
# Reads output/tables/new_<addr|hh>_es_<event>.csv and taxdelinq_did_buncombe.csv.
# Writes output/figures/new_outcomes/*.pdf and *.png. Storms without estimates are omitted.
suppressPackageStartupMessages({ library(ggplot2); library(data.table) })
COL <- c(Florence = "#2a78d6", Helene = "#eb6834")
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"
TAB <- "output/tables"; FIG <- "output/figures/new_outcomes"
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)
theme_paper <- function() {
  theme_minimal(base_size = 9, base_family = "sans") +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3),
          axis.line.x = element_line(colour = INK2, linewidth = 0.3),
          axis.ticks.x = element_line(colour = INK2, linewidth = 0.3),
          axis.text = element_text(colour = INK2), axis.title = element_text(colour = INK),
          strip.text = element_text(colour = INK, face = "bold", hjust = 0, size = 9),
          legend.position = "none", plot.title = element_text(colour = INK, face = "bold", size = 10),
          plot.caption = element_text(colour = INK2, hjust = 0, size = 7),
          panel.spacing = unit(1.2, "lines"), plot.margin = margin(6, 10, 6, 6))
}
WIN <- c("20150101" = "2015–16", "20170101" = "2017–18", "20180508" = "2018–19",
         "20210101" = "2021–22", "20220517" = "2022–23", "20230101" = "2023–24", "20240514" = "2024–25")
POST <- c(florence = "20180508", helene = "20240514")
read_es <- function(what) {
  fs <- list.files(TAB, sprintf("^new_%s_es_(florence|helene)\\.csv$", what), full.names = TRUE)
  d <- rbindlist(lapply(fs, fread, colClasses = list(character = "term")))
  d[, `:=`(storm = factor(tools::toTitleCase(event), levels = c("Florence", "Helene")), window = WIN[term],
           post = term == POST[event], ref = se == 0, lo = est - 1.96 * se, hi = est + 1.96 * se)]
  d[ref == TRUE, `:=`(lo = NA, hi = NA)]
  d[, window := factor(window, levels = unname(WIN))]
  d
}
es_plot <- function(d, title, ylab, caption) {
  ggplot(d, aes(window, est, colour = storm)) +
    geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.5, na.rm = TRUE) +
    geom_point(aes(fill = interaction(post, storm)), shape = 21, size = 2.4, stroke = 0.7) +
    geom_text(data = d[ref == TRUE], aes(label = "ref."), colour = INK2, size = 2.5, vjust = -1.2) +
    scale_colour_manual(values = COL) +
    scale_fill_manual(values = c("FALSE.Florence" = "white", "TRUE.Florence" = COL[["Florence"]],
                                 "FALSE.Helene" = "white", "TRUE.Helene" = COL[["Helene"]])) +
    labs(title = title, x = "Cohort window (baseline to follow-up, about 16 months)", y = ylab, caption = caption) +
    theme_paper()
}
save_fig <- function(p, name, w, h) {
  # a file that is open in a viewer cannot be overwritten on Windows: skip it and continue
  tryCatch(ggsave(file.path(FIG, paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf),
           error = function(e) message("not written (file in use?): ", name, ".pdf"))
  tryCatch(ggsave(file.path(FIG, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white"),
           error = function(e) message("not written (file in use?): ", name, ".png"))
}
CAP <- paste0("Coefficients on flooded address x cohort, relative to the last pre-storm cohort; 95% confidence intervals,\n",
              "standard errors clustered by tract. Hollow markers: pre-storm cohorts. Filled marker: cohort whose window\n",
              "contains the storm. Addresses within 500 m of the flood boundary; address and block group x cohort fixed effects.")

# ---- Outcome 1: replacement at the address -------------------------------------------------
a <- read_es("addr")
if (nrow(a)) {
  a[, measure := fifelse(grepl("no registered voter", outcome), "No registered voter at follow-up\n(percentage points)",
                         "Arrivals per 100 addresses\n(new registrants at the address)")]
  p <- es_plot(a, "Outcome 1. Replacement at flooded addresses (flooded minus unflooded)", NULL, CAP) +
    facet_grid(measure ~ storm, scales = "free", switch = "y") +
    theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, face = "plain", size = 8, hjust = 0.5))
  save_fig(p, "es_outcome1_replacement", 7.2, 5.6)
}
# ---- Outcome 2: household splitting ---------------------------------------------------------
h <- read_es("hh")
if (nrow(h)) {
  p <- es_plot(h, "Outcome 2. Household split: members registered at different addresses",
               "Flooded minus unflooded (percentage points)",
               paste0(CAP, "\nVoters in households of two to six registered voters at baseline.")) +
    facet_wrap(~ storm, scales = "free_x")
  save_fig(p, "es_outcome2_household_split", 7.2, 3.9)
}
# ---- Outcome 2 with the household as the unit ------------------------------------------------
hu <- read_es("hhu")
if (nrow(hu)) {
  hu[, measure := fifelse(grepl("split", outcome), "Household split:\nmembers at more than one address",
                          "Household moved together\nto a new address")]
  hu[, measure := factor(measure, levels = c("Household split:\nmembers at more than one address",
                                             "Household moved together\nto a new address"))]
  p <- es_plot(hu, "Households after flooding (flooded minus unflooded households, percentage points)", NULL,
               paste0(CAP, "\nUnit: household of two to six registered adults at one address at baseline.")) +
    facet_grid(measure ~ storm, scales = "free", switch = "y") +
    theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, face = "plain", size = 8, hjust = 0.5))
  save_fig(p, "es_household_split_household_level", 7.2, 5.6)
}
# ---- Helene, household level, all controls: household FE + household characteristics x cohort ----
fc <- file.path(TAB, "new_hhufc_es_helene.csv")
if (file.exists(fc)) {
  d <- fread(fc, colClasses = list(character = "term"))
  d[, `:=`(storm = factor("Helene", levels = c("Florence", "Helene")), window = factor(WIN[term], levels = unname(WIN)),
           post = term == POST["helene"], ref = se == 0, lo = est - 1.96 * se, hi = est + 1.96 * se)]
  d[ref == TRUE, `:=`(lo = NA, hi = NA)]
  d[, measure := factor(fifelse(grepl("split", outcome), "Household split", "Household moved together"),
                        levels = c("Household split", "Household moved together"))]
  p <- es_plot(d, "Helene: households after flooding, all controls (flooded minus unflooded)",
               "Percentage points",
               paste0("Coefficients on flooded address x cohort, relative to the last pre-storm cohort; 95% confidence intervals,\n",
                      "standard errors clustered by tract. Hollow markers: pre-storm cohorts. Filled marker: cohort whose window contains\n",
                      "the storm. Unit: household of two to six registered adults at one address, within 500 m of the flood boundary.\n",
                      "Split: members at more than one address at follow-up. Moved together: all members at one new address.\n",
                      "Controls: household fixed effects; block group x cohort fixed effects; household size, mean age, share white,\n",
                      "share female and unit-number indicator, each interacted with cohort.")) +
    facet_wrap(~ measure, scales = "free_y")
  save_fig(p, "es_helene_household_all_controls", 7.2, 4.2)
}
# ---- Household party outcomes ----------------------------------------------------------------
hp <- read_es("hhp")
if (nrow(hp)) {
  hp[, measure := fifelse(grepl("^household all one party", outcome), "All one party at start:\nmixed parties afterwards",
                   fifelse(grepl("^household mixed", outcome), "Mixed parties at start:\nall one party afterwards",
                           "Voter changed party"))]
  hp[, measure := factor(measure, levels = c("All one party at start:\nmixed parties afterwards",
                                             "Mixed parties at start:\nall one party afterwards", "Voter changed party"))]
  p <- es_plot(hp, "Party registration within households (flooded minus unflooded, percentage points)", NULL,
               paste0(CAP, "\nObserved voters in households of two to six registered voters; party of registration, not votes.")) +
    facet_grid(measure ~ storm, scales = "free", switch = "y") +
    theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, face = "plain", size = 8, hjust = 0.5))
  save_fig(p, "es_household_party", 7.2, 7.4)
}
# ---- Outcome 3: property tax delinquency (one cross-section; not an event study) -------------
f <- file.path(TAB, "taxdelinq_did_buncombe.csv")
if (file.exists(f)) {
  t <- fread(f)[term == "flooded"]
  t[, `:=`(lo = est - 1.96 * se, hi = est + 1.96 * se, spec = factor(spec, levels = spec))]
  p <- ggplot(t, aes(spec, est)) +
    geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.5, colour = COL[["Helene"]]) +
    geom_point(shape = 21, size = 2.6, stroke = 0.7, colour = COL[["Helene"]], fill = COL[["Helene"]]) +
    labs(title = "Outcome 3. Property tax delinquency after Helene, Buncombe County\n(one cross-section; not an event study)",
         x = "Specification", y = "Flooded minus unflooded\n(percentage points)",
         caption = paste0("Share of sample addresses on the county's list of unpaid 2024 real property tax bills as of 3 February 2025.\n",
                          "Mean at unflooded addresses: ", sprintf("%.1f", t$mean_unflooded[1]), " percent. ",
                          "No list from before the storm is available, so pre-storm differences\ncannot be checked. ",
                          "95% confidence intervals, standard errors clustered by tract.")) +
    theme_paper()
  save_fig(p, "outcome3_tax_delinquency_buncombe", 7.2, 3.9)
}
