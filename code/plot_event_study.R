# Event-study figures for "When Voters Move" (design, 2026-11-14).
#
# The figures read estimates written by 07_estimate.R to output/tables/*.csv with columns:
#   event   "Florence" | "Helene"
#   k_label x-axis label (cohort window, e.g. "2017–18")   [cohort figure]
#   k_num   numeric x (cohort order, or months since landfall)
#   est, lo, hi   coefficient and 95% CI in percentage points
#   ref     TRUE for the omitted (normalized) cohort
#   post    TRUE for windows spanning the storm
#   bw      bandwidth in metres                             [sensitivity figure]
# If a file is missing, the figure is drawn as an empty, labelled template (no invented points).
#
# Design choices
#  * Fig 3a, cohort leads: small multiples by storm (each storm has its own cohort calendar,
#    so a shared x-axis would be misleading). One ink colour; pre-storm cohorts hollow, storm
#    cohort filled; the reference cohort is a hollow marker pinned at 0 and labelled "ref.".
#    A dashed vertical rule marks the storm between the last pre and the post cohort.
#  * Fig 3b, dynamics: one panel, months since landfall on x, both storms as two series
#    (categorical slots 1–2), legend + direct labels at the right end, so identity is never
#    colour-alone; the prespecified ~12-month primary horizon is a dotted rule.
#  * Fig 4, bandwidth sensitivity: small multiples by storm, x = bandwidth (log scale), the
#    prespecified 500 m estimate filled, alternatives hollow.
#  * 95% CIs as thin error bars (0.5 pt), recessive grid (major y only), zero line in
#    secondary ink, y in percentage points, sans-serif 9 pt to match the Springer page.
suppressPackageStartupMessages({ library(ggplot2); library(data.table) })

COL <- c(Florence = "#2a78d6", Helene = "#eb6834")   # validated categorical slots 1-2
INK <- "#0b0b0b"; INK2 <- "#52514e"; GRID <- "#e6e5e1"
FIG <- "output/figures"; TAB <- "output/tables"
dir.create(FIG, showWarnings = FALSE, recursive = TRUE)

theme_paper <- function() {
  theme_minimal(base_size = 9, base_family = "sans") +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.y = element_line(colour = GRID, linewidth = 0.3),
          axis.line.x = element_line(colour = INK2, linewidth = 0.3),
          axis.ticks.x = element_line(colour = INK2, linewidth = 0.3),
          axis.text = element_text(colour = INK2), axis.title = element_text(colour = INK),
          strip.text = element_text(colour = INK, face = "bold", hjust = 0, size = 9),
          legend.position = "top", legend.justification = "left", legend.title = element_blank(),
          plot.caption = element_text(colour = INK2, hjust = 0, size = 7),
          panel.spacing = unit(1.2, "lines"), plot.margin = margin(6, 10, 6, 6))
}

read_or_template <- function(f, template) {
  p <- file.path(TAB, f)
  if (file.exists(p)) list(d = fread(p), pending = FALSE) else list(d = template, pending = TRUE)
}
pending_layer <- function(pending, x = -Inf) if (pending)
  annotate("text", x = x, y = Inf, label = "Template: estimates pending", hjust = -0.05, vjust = 1.6,
           colour = INK2, size = 2.8, fontface = "italic")

save_fig <- function(p, name, w, h) {
  ggsave(file.path(FIG, paste0(name, ".pdf")), p, width = w, height = h, device = cairo_pdf)
  ggsave(file.path(FIG, paste0(name, ".png")), p, width = w, height = h, dpi = 300, bg = "white")
  # manuscript copies: journals ask for no caption text inside the artwork (notes go in the caption)
  q <- p + labs(caption = NULL)
  ggsave(file.path(FIG, paste0(name, "_nocap.pdf")), q, width = w, height = h - 0.3, device = cairo_pdf)
}

# ---- Figure 3a: cohort event study -------------------------------------------------------
tmpl_a <- rbind(
  data.table(event = "Florence", k_label = c("2015–16", "2017–18", "2018–19"), k_num = 1:3,
             ref = c(FALSE, TRUE, FALSE), post = c(FALSE, FALSE, TRUE)),
  data.table(event = "Helene", k_label = c("2021–22", "2022–23", "2023–24", "2024–25"), k_num = 1:4,
             ref = c(FALSE, FALSE, TRUE, FALSE), post = c(FALSE, FALSE, FALSE, TRUE)))
tmpl_a[, `:=`(est = NA_real_, lo = NA_real_, hi = NA_real_)]
A <- read_or_template("es_cohort.csv", tmpl_a)
da <- A$d; da[ref == TRUE, `:=`(est = 0, lo = NA, hi = NA)]
setorder(da, event, k_num)
da[, xl := factor(k_label, levels = unique(k_label))]          # discrete, per-facet axis
da[, xi := seq_len(.N), by = event]                              # position within facet
storm_x <- da[, .(x = max(xi[!post]) + 0.5), by = event]
fig3a <- ggplot(da, aes(xl, est)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_point(aes(colour = event, fill = ifelse(post, event, "none")), shape = 21, size = 2.4,
             stroke = 0.8, na.rm = TRUE) +   # first x-mapped layer => discrete x scale
  geom_vline(data = storm_x, aes(xintercept = x), linetype = "22", colour = INK2, linewidth = 0.3) +
  geom_text(data = storm_x, aes(x = x, y = Inf, label = "storm"), vjust = 1.4, hjust = -0.15,
            colour = INK2, size = 2.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi, colour = event), width = 0, linewidth = 0.5, na.rm = TRUE) +
  geom_text(data = da[ref == TRUE], aes(label = "ref."), vjust = 2.2, colour = INK2, size = 2.5) +
  scale_colour_manual(values = COL, guide = "none") +
  scale_fill_manual(values = c(COL, none = "white"), guide = "none") +
  facet_wrap(~event, scales = "free_x") +
  labs(x = "Cohort window (baseline → follow-up, ~16 months)",
       y = "Effect on cross-county\nre-registration (pp)",
       caption = paste0("Points: flooded × cohort coefficients relative to the reference cohort; bars: 95% CI, SEs clustered by census tract.\n",
                        "Hollow = pre-storm cohort (placebo test); filled = cohort whose window spans the storm.")) +
  theme_paper() + pending_layer(A$pending)
if (A$pending) fig3a <- fig3a + scale_y_continuous(limits = c(-1, 1))
save_fig(fig3a, "fig3a_event_study_cohorts", 6.5, 2.9)

# ---- Figure 3b: dynamics after landfall ---------------------------------------------------
tmpl_b <- data.table(event = rep(c("Florence", "Helene"), c(5, 5)),
                     k_num = c(2, 3.5, 12, 15.5, 26, 1.5, 3.5, 11.5, 17, 25.5),
                     est = NA_real_, lo = NA_real_, hi = NA_real_)
B <- read_or_template("es_dynamics.csv", tmpl_b)
db <- B$d
dodge <- position_dodge(width = 0.9)
lab_b <- db[!is.na(est), .SD[which.max(k_num)], by = event]
fig3b <- ggplot(db, aes(k_num, est, colour = event)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_vline(xintercept = 12, linetype = "13", colour = INK2, linewidth = 0.3) +
  annotate("text", x = 12, y = Inf, label = "primary horizon", vjust = 1.4, hjust = -0.06, colour = INK2, size = 2.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.5, position = dodge, na.rm = TRUE) +
  geom_line(linewidth = 0.7, position = dodge, na.rm = TRUE) +
  geom_point(shape = 21, fill = "white", size = 2.4, stroke = 0.8, position = dodge, na.rm = TRUE) +
  geom_text(data = lab_b, aes(label = event), hjust = -0.25, size = 2.8, show.legend = FALSE) +
  scale_colour_manual(values = COL) +
  scale_x_continuous(breaks = c(0, 6, 12, 18, 24, 30), limits = c(0, 31), expand = expansion(add = c(0, 1))) +
  labs(x = "Months since landfall (follow-up snapshot)", y = "Effect on cross-county
re-registration (pp)",
       caption = paste0("Flooded minus unflooded voters in the storm cohort, same block group, at each follow-up snapshot\n",
                        "(cross-section, not differenced against a pre-storm cohort); 95% CI, tract-clustered.")) +
  theme_paper() + pending_layer(B$pending)
if (B$pending) fig3b <- fig3b + scale_y_continuous(limits = c(-1, 1))
save_fig(fig3b, "fig3b_event_study_dynamics", 4.6, 3.0)

# ---- Figure 4: bandwidth sensitivity ------------------------------------------------------
tmpl_c <- CJ(event = c("Florence", "Helene"), bw = c(250, 500, 1000, 2000))[, `:=`(est = NA_real_, lo = NA_real_, hi = NA_real_)]
C <- read_or_template("sens_bandwidth.csv", tmpl_c)
dc <- C$d
fig4 <- ggplot(dc, aes(bw, est, colour = event)) +
  geom_hline(yintercept = 0, colour = INK2, linewidth = 0.3) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0, linewidth = 0.5, na.rm = TRUE) +
  geom_point(aes(fill = ifelse(bw == 500, event, "none")), shape = 21, size = 2.4, stroke = 0.8, na.rm = TRUE) +
  scale_colour_manual(values = COL, guide = "none") +
  scale_fill_manual(values = c(COL, none = "white"), guide = "none") +
  scale_x_log10(breaks = c(250, 500, 1000, 2000), labels = c("250", "500\n(primary)", "1,000", "2,000")) +
  facet_wrap(~event) +
  labs(x = "Distance band around the flood boundary (m)", y = "Effect on cross-county
re-registration (pp)",
       caption = "Each point is a separate regression restricted to addresses within the band; 95% CI, tract-clustered.") +
  theme_paper() + pending_layer(C$pending, x = 230)
if (C$pending) fig4 <- fig4 + scale_y_continuous(limits = c(-1, 1))
save_fig(fig4, "fig4_bandwidth_sensitivity", 6.5, 2.7)
cat("figures written; pending:", A$pending, B$pending, C$pending, "\n")
