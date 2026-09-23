# Standalone reproduction of Marion's main figure
# (figures/acp/main_p_reform_beats_repeal_by_country.pdf), built from the CSV
# that 8_Conjoint_ACP.R writes. Change colours / theme / sizes freely here.

library(tidyverse)

# ---- Data --------------------------------------------------------------------
countries <- c("Spain", "France", "Germany", "Romania")          # top to bottom
samples   <- c("Non-committed sample", "Opposers to carbon pricing")  # top to bottom within a country
packages  <- c("Minimal package", "Maximal package")
sample_gap  <- 0.16   # vertical offset between the two samples (Marion: 0.1375)
package_gap <- 0.04   # vertical offset between minimal and maximal package (0 = same line)

d <- read_csv("figures/acp/main_p_reform_beats_repeal_all_samples.csv",
              show_col_types = FALSE) |>
  mutate(
    Country       = factor(Country, levels = countries),
    sample_label  = factor(sample_label, levels = samples),
    package_label = factor(package_label, levels = packages),
    # Numeric y: Spain on top; within a country the first sample sits above
    # the second (+/- sample_gap). Within a sample, the minimal package sits
    # slightly above the maximal one (+/- package_gap) so overlapping error
    # bars stay distinguishable. Applied to all rows for consistency.
    y_row = length(countries) + 1 - as.integer(Country),
    y_pos = y_row +
      ifelse(sample_label == samples[1], sample_gap, -sample_gap) +
      ifelse(package_label == packages[1], package_gap, -package_gap)
  )

# ---- Styling -----------------------------------------------------------------
# Colours from ggsci::pal_npg()(10):
#   #E64B35FF red, #4DBBD5FF light blue, #00A087FF green, #3C5488FF dark blue,
#   #F39B7FFF salmon, #8491B4FF grey-blue, #91D1C2FF mint, #DC0000FF dark red, ...
package_fills  <- c("Minimal package" = "#E64B35FF", "Maximal package" = "#3C5488FF")
sample_shapes  <- c("Non-committed sample" = 22, "Opposers to carbon pricing" = 21)
package_legend <- c(
  "Minimal package" = "Carbon revenue only - government-managed - lower investment",
  "Maximal package" = "Expanded budget with wealth tax - protected fund with citizen oversight - higher investment"
) |> str_wrap(width = 50)
x_limits <- c(0.20, 0.78)

# ---- Plot --------------------------------------------------------------------
p <- ggplot(d, aes(x = p_reform_beats_repeal, y = y_pos)) +
  geom_vline(xintercept = 0.5, linewidth = 0.25) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi, colour = package_label),
                orientation = "y", width = 0.12, linewidth = 0.25) +
  geom_point(aes(fill = package_label, shape = sample_label),
             stroke = 0.3, size = 2.5) +
  scale_colour_manual(values = package_fills, guide = "none") +
  scale_fill_manual(values = package_fills, labels = package_legend, name = NULL) +
  scale_shape_manual(values = sample_shapes, name = NULL) +
  guides(
    fill  = guide_legend(order = 1, override.aes = list(shape = 22, size = 4)),
    shape = guide_legend(order = 2, override.aes = list(fill = "white", size = 3.5))
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     limits = x_limits, breaks = seq(0.2, 0.7, by = 0.1),
                     expand = expansion(mult = c(0.02, 0.02))) +
  scale_y_continuous(breaks = seq_along(countries), labels = rev(countries),
                     expand = expansion(add = 0.45)) +
  labs(x = "Probability to prefer the reform over repealing EU ETS2", y = NULL) +
  theme_bw() +
  theme(
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(colour = "black", fill = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.2),
    axis.ticks         = element_line(linewidth = 0.2),
    axis.text.x        = element_text(size = 6),
    axis.text.y        = element_text(size = 6),
    axis.title         = element_text(size = 7),
    legend.position    = "bottom",
    legend.box         = "vertical",
    legend.spacing.y   = unit(0, "pt"),
    legend.text        = element_text(size = 6, lineheight = 0.95),
    legend.key.height  = unit(0.45, "cm"),
    legend.key.width   = unit(0.4, "cm"),
    legend.margin      = margin(0, 0, 0, 0),
    plot.margin        = margin(6, 10, 4, 6, "pt")
  )

pdf("../6_EUETS2_Citizens_Survey/1_Figures/Figure_5_new.pdf", width = 160/25.4, height = 80/25.4)
print(p)
dev.off()


# =============================================================================
# Figure: pooled ACP by attribute level
# Standalone reproduction of figures/acp/panel_a_acp_pooled_inclrepealfst_inclrepeallst.pdf
# (full sample), restyled like the figure above. Built from the CSVs that
# 8_Conjoint_ACP.R writes (panel_a_acp_pooled_*.csv, panel_a_acp_importance_pooled_*.csv).
# =============================================================================

# ---- Shared data and styling for all ACP figures -------------------------------
# Level order within each attribute (1 = lowest level, drawn at the bottom)
level_order_lookup <- readRDS("output/acp/conjoint_level_lookup.rds") |>
  distinct(attribute, level = level_short, level_order)

acp_fill <- "#3C5488FF"   # NPG dark blue, as in the main figure

# Theme shared by the ACP figures (theme_bw defaults; sizes as in Figure 4/5)
theme_acp <- theme_bw() +
  theme(
    panel.grid.minor   = element_blank(),
    panel.border       = element_rect(colour = "black", fill = NA),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(linewidth = 0.2),
    axis.ticks         = element_line(linewidth = 0.2),
    axis.text.x        = element_text(size = 6),
    axis.text.y        = element_text(size = 6),
    axis.title         = element_text(size = 7),
    strip.text         = element_text(size = 7),
    plot.margin        = margin(6, 10, 4, 6, "pt")
  )

acp_xlab <- "Average component preference (pp, 0 = indifference)"

# ---- Pooled ACP figure (one sample) --------------------------------------------
# acp_suffix: "inclrepealfst_inclrepeallst" (full sample) or
#             "inclrepealfst_inclrepeallst_opposers_q46_2" (Q46_2 opposers)
plot_acp_pooled <- function(acp_suffix) {
  importance <- read_csv(paste0("figures/acp/panel_a_acp_importance_pooled_", acp_suffix, ".csv"),
                         show_col_types = FALSE)

  d_acp <- read_csv(paste0("figures/acp/panel_a_acp_pooled_", acp_suffix, ".csv"),
                    show_col_types = FALSE) |>
    dplyr::select(attribute, attribute_lab, level, estimate, ci_lo, ci_hi) |>
    left_join(level_order_lookup, by = c("attribute", "level")) |>
    left_join(importance |> dplyr::select(attribute, range, range_ci_lo, range_ci_hi),
              by = "attribute") |>
    mutate(
      # Panel title: attribute name + ACP range in brackets (highest minus lowest
      # level, in pp, 95% simulation CI from conjacp.var()). Panels ordered by range.
      facet_lab = sprintf("%s (range: %.1f pp, 95%% CI: %.1f-%.1f)",
                          attribute_lab, 100 * range, 100 * range_ci_lo, 100 * range_ci_hi),
      facet_lab = fct_reorder(facet_lab, -range),
      level     = fct_reorder(level, level_order)
    )

  ggplot(d_acp, aes(x = estimate, y = level)) +
    geom_vline(xintercept = 0, linewidth = 0.25) +
    geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi), orientation = "y",
                  width = 0.2, linewidth = 0.25, colour = acp_fill) +
    geom_point(shape = 22, fill = acp_fill, stroke = 0.3, size = 2.5) +
    facet_wrap(~ facet_lab, ncol = 1, scales = "free_y") +
    scale_x_continuous(labels = scales::label_number(scale = 100)) +   # ACP in pp
    labs(x = acp_xlab, y = NULL) +
    theme_acp
}

# Full sample (replaces panel_a_acp_pooled_inclrepealfst_inclrepeallst.pdf)
p_acp <- plot_acp_pooled("inclrepealfst_inclrepeallst")

pdf("../6_EUETS2_Citizens_Survey/1_Figures/Figure_4_new.pdf", width = 160/25.4, height = 190/25.4)
print(p_acp)
dev.off()

# Q46_2 opposers (replaces panel_a_acp_pooled_inclrepealfst_inclrepeallst_opposers_q46_2.pdf).
# Panels are ordered by the opposers' own ranges, as in Marion's version.
p_acp_opp <- plot_acp_pooled("inclrepealfst_inclrepeallst_opposers_q46_2")

pdf("../6_EUETS2_Citizens_Survey/1_Figures/Figure_SI_panel_a_acp_pooled_inclrepealfst_inclrepeallst_opposers_q46_2.pdf", width = 160/25.4, height = 190/25.4)
print(p_acp)
dev.off()


# =============================================================================
# Figure: ACP by country (attributes x countries)
# Standalone reproduction of figures/acp/panel_a_acp_countries_inclrepealfst_inclrepeallst.pdf
# (full sample), restyled like the figures above.
# =============================================================================

# ---- Data --------------------------------------------------------------------
acp_countries <- c("France", "Germany", "Romania", "Spain")   # Marion's order (left to right)

# Short row labels (as in Marion's version)
acp_attr_short <- c(
  budget_and_funding         = "Budget &\nfunding",
  budget_control             = "Budget\ncontrol",
  household_support          = "Household\nsupport",
  information                = "Information",
  infrastructure_ownership   = "Infrastructure",
  worker_support             = "Worker\nsupport",
  community_mobility_support = "Community\nmobility"
)

# Row order = order of the pooled full-sample figure (largest range on top)
attr_order <- read_csv("figures/acp/panel_a_acp_importance_pooled_inclrepealfst_inclrepeallst.csv",
                       show_col_types = FALSE) |>
  arrange(desc(range)) |>
  pull(attribute)

d_acp_co <- read_csv("figures/acp/panel_a_acp_countries_inclrepealfst_inclrepeallst.csv",
                     show_col_types = FALSE) |>
  dplyr::select(Country, attribute, level, level_order, estimate, ci_lo, ci_hi) |>
  mutate(
    Country   = factor(Country, levels = acp_countries),
    attribute = factor(attribute, levels = attr_order),
    attr_lab  = factor(acp_attr_short[as.character(attribute)],
                       levels = acp_attr_short[attr_order]),
    # Level names are unique across attributes, so one factor orders all rows
    level     = fct_reorder(level, as.integer(attribute) * 10 + level_order)
  )

# ---- Plot --------------------------------------------------------------------
p_acp_co <- ggplot(d_acp_co, aes(x = estimate, y = level)) +
  geom_vline(xintercept = 0, linewidth = 0.25) +
  geom_errorbar(aes(xmin = ci_lo, xmax = ci_hi), orientation = "y",
                width = 0.3, linewidth = 0.25, colour = acp_fill) +
  geom_point(shape = 22, fill = acp_fill, stroke = 0.3, size = 1.8) +
  facet_grid(attr_lab ~ Country, scales = "free_y", space = "free_y", switch = "y") +
  scale_x_continuous(labels = scales::label_number(scale = 100)) +   # ACP in pp
  labs(x = acp_xlab, y = NULL) +
  theme_acp +
  theme(
    # Row labels on the left, horizontal (needed for readability)
    strip.placement   = "outside",
    strip.text.y.left = element_text(size = 7, angle = 0),
    panel.spacing.y   = unit(0.15, "lines")
  )


pdf("../6_EUETS2_Citizens_Survey/1_Figures/Figure_SI_panel_a_acp_countries_inclrepealfst_inclrepeallst.pdf", width = 160/25.4, height = 175/25.4)
print(p_acp_co)
dev.off()
