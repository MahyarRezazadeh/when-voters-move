# Helene: event-study figure for the household responses shown on slide "Result 1" (2026-11-22).
# Estimates are taken from existing outputs (no re-estimation):
#   split and moved together: output/tables/hh_source_ident_helene.csv (code/34_hh_response_source.R, rows "event study")
#   adult changed address: logs/49_moved_es.log (code/49_moved_es.R)
# Flooded x window, reference 2023-24, address FE + block group x window FE, tract-clustered SE, 95% CI.
# Usage: Rscript code/63_fig_hh_response_es.R
suppressPackageStartupMessages({ library(data.table); library(ggplot2) })
H <- fread("output/tables/hh_source_ident_helene.csv")[check == "event study" & outcome %in% c("Split (any)", "All leave together")]
H <- rbind(H[, .(outcome, w = "2021-22", est = pre1, se = pre1_se)], H[, .(outcome, w = "2022-23", est = pre2, se = pre2_se)],
           H[, .(outcome, w = "2023-24", est = 0, se = 0)], H[, .(outcome, w = "2024-25", est = storm, se = storm_se)])
M <- data.table(outcome = "Adult changed address", w = c("2021-22", "2022-23", "2023-24", "2024-25"),
                est = c(-0.057, -0.309, 0, 3.614), se = c(0.367, 0.333, 0, 0.863))
D <- rbind(M, H)
D[, outcome := factor(fcase(outcome == "Split (any)", "Household split", outcome == "All leave together", "Whole household moved together",
                            default = outcome), levels = c("Adult changed address", "Household split", "Whole household moved together"))]
D[, x := match(w, c("2021-22", "2022-23", "2023-24", "2024-25")) + (as.integer(outcome) - 2) * 0.18]
g <- ggplot(D, aes(x, est, colour = outcome, shape = outcome)) +
  geom_hline(yintercept = 0, colour = "grey55", linewidth = 0.4) +
  geom_vline(xintercept = 3.5, colour = "grey55", linetype = "dashed", linewidth = 0.4) +
  geom_errorbar(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se), width = 0, linewidth = 0.7) +
  geom_point(size = 2.6) +
  annotate("text", x = 3.55, y = 5.9, label = "Helene\n(Sept 2024)", hjust = 0, size = 3.2, colour = "grey30") +
  scale_x_continuous(breaks = 1:4, labels = c("2021-22\n(COVID period)", "2022-23", "2023-24\n(reference)", "2024-25")) +
  scale_colour_manual(values = c("#1f4e79", "#c55a11", "#548235")) + scale_shape_manual(values = c(16, 17, 15)) +
  labs(x = "16-month window", y = "Flooded minus unflooded (percentage points)", colour = NULL, shape = NULL) +
  theme_minimal(base_size = 12) + theme(legend.position = "top", panel.grid.minor = element_blank(), panel.grid.major.x = element_blank())
ggsave("output/figures/fig_hh_response_es.png", g, width = 7.5, height = 4.6, dpi = 220)
ggsave("output/figures/fig_hh_response_es.pdf", g, width = 7.5, height = 4.6)
print(D[, .(outcome, w, est = round(est, 2), se = round(se, 2))])
