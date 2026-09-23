# =============================================================================
# conjoint_acp.R — Ganter ACP and P(reform ≻ repeal)
# =============================================================================
# Run after conjoint_prepare.R (or with cached RDS in output/acp/).
#
#   1. Load long data
#   2. Respondent repeal-rank flags
#   3. Reform–repeal pairs
#   4. Reform–reform pairs and V tables
#   5. ACP (non-committed, console) and package P(reform beats repeal)
#   6. (ACP figures: full sample only, written in §8)
#   7. Main P(reform beats repeal) figure (non-committed + Q46_2 opposers)
#   8. Full-sample ACP figures and DPP SI tables; other subgroups
#
# Cached inputs from conjoint_prepare.R
#   data_conjoint_all.rds     one row per respondent × task × profile (A/B/C)
#   conjoint_level_lookup.rds harmonised attribute level labels
# =============================================================================

# ---- 0. Setup ----------------------------------------------------------------

USE_SAVED_INTERMEDIATES <- TRUE   # TRUE: load RDS only (skip source("conjoint_prepare.R"))

# Sensitivity: drop respondents who always rank repeal 1st and/or 3rd (4 tasks).
EXCLUDE_REPEAL_ALWAYS_FIRST <- TRUE
EXCLUDE_REPEAL_ALWAYS_LAST  <- TRUE

required_pkgs <- c("tidyverse", "sandwich", "stringr")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

source("8_Conjoint_ACP_Helpers.R")
ensure_conjacp_loaded()   # Ganter functions: conjacp.estimation(), etc.

# conjacp.var() simulates importance CIs (1,000 draws); fix the seed so the
# numbers are reproducible across runs (also when cached RDS are loaded).
set.seed(2026)

dir.create("output/acp", recursive = TRUE, showWarnings = FALSE)
dir.create("figures/acp", recursive = TRUE, showWarnings = FALSE)

PATH_CONJOINT_ALL <- "output/acp/data_conjoint_all.rds"
PATH_LEVEL_LOOKUP <- "output/acp/conjoint_level_lookup.rds"

REPEAL_FLAG_COLS <- c(
  "n_repeal_tasks", "n_repeal_first", "n_repeal_last",
  "repeal_always_first", "repeal_always_last"
)

# ---- 1. Load long conjoint data + level lookup --------------------------------

if (USE_SAVED_INTERMEDIATES &&
    file.exists(PATH_CONJOINT_ALL) &&
    file.exists(PATH_LEVEL_LOOKUP)) {
  message("Loading cached intermediates from output/acp/")
  data_conjoint_all     <- readRDS(PATH_CONJOINT_ALL)
  conjoint_level_lookup <- readRDS(PATH_LEVEL_LOOKUP)
} else {
  message("Sourcing conjoint_prepare.R (writes output/acp/*.rds)")
  source("8_Conjoint_Prepare.R")
  data_conjoint_all     <- readRDS(PATH_CONJOINT_ALL)
  conjoint_level_lookup <- readRDS(PATH_LEVEL_LOOKUP)
}

if (!exists("data_conjoint_all") || !exists("conjoint_level_lookup")) {
  stop("Expected data_conjoint_all and conjoint_level_lookup.", call. = FALSE)
}

# ---- 2. Repeal ranking flags --------------------------------------------------

repeal_flags <- build_repeal_respondent_flags(data_conjoint_all)

data_conjoint_all <- data_conjoint_all %>%
  dplyr::select(-dplyr::any_of(REPEAL_FLAG_COLS)) %>%
  dplyr::left_join(repeal_flags, by = c("Country", "ID"))

# One row per respondent, for summaries and sample filters.
conjoint_respondent_meta <- repeal_flags %>%
  dplyr::left_join(
    data_conjoint_all %>% dplyr::distinct(Country, ID, d1, d2),
    by = c("Country", "ID")
  )

repeal_flag_summary <- conjoint_respondent_meta %>%
  dplyr::group_by(Country) %>%
  dplyr::summarise(
    n_respondents        = dplyr::n(),
    n_always_repeal_1st  = sum(repeal_always_first, na.rm = TRUE),
    n_always_repeal_last = sum(repeal_always_last, na.rm = TRUE),
    share_always_1st     = mean(repeal_always_first, na.rm = TRUE),
    share_always_last    = mean(repeal_always_last, na.rm = TRUE),
    .groups              = "drop"
  )

cat("\n--- Respondent repeal flags (4 tasks complete) ---\n")
print(repeal_flag_summary)

# ---- 3. Reform–repeal pairs ---------------------------------------------------
# P(reform ≻ repeal) is the mean of pair `outcome`. No Ganter V coding here.

PATH_PAIRS_REFORM_REPEAL <- "output/acp/reform_repeal_pairs.rds"

pairs_reform_repeal_raw <- build_reform_repeal_pairs(data_conjoint_all) %>%
  dplyr::left_join(repeal_flags, by = c("Country", "ID"))

pairs_reform_repeal <- filter_pairs_by_repeal_rankers(
  pairs_reform_repeal_raw,
  exclude_always_first = EXCLUDE_REPEAL_ALWAYS_FIRST,
  exclude_always_last  = EXCLUDE_REPEAL_ALWAYS_LAST
)

if (EXCLUDE_REPEAL_ALWAYS_FIRST || EXCLUDE_REPEAL_ALWAYS_LAST) {
  cat("\n--- Reform–repeal pair exclusions (repeal rankers) ---\n")
  print(summarise_repeal_ranker_exclusions(pairs_reform_repeal_raw, pairs_reform_repeal))
}

saveRDS(pairs_reform_repeal, PATH_PAIRS_REFORM_REPEAL)

# ---- 4. Reform–reform pairs and V tables --------------------------------------

PATH_PAIRS_REFORM_REFORM <- "output/acp/reform_reform_pairs.rds"
PATH_VTABLES_REFORM_REFORM <- "output/acp/reform_reform_v_tables.rds"

pairs_reform_reform_raw <- build_reform_reform_pairs(data_conjoint_all) %>%
  dplyr::left_join(repeal_flags, by = c("Country", "ID"))

pairs_reform_reform <- filter_pairs_by_repeal_rankers(
  pairs_reform_reform_raw,
  exclude_always_first = EXCLUDE_REPEAL_ALWAYS_FIRST,
  exclude_always_last  = EXCLUDE_REPEAL_ALWAYS_LAST
)

if (EXCLUDE_REPEAL_ALWAYS_FIRST || EXCLUDE_REPEAL_ALWAYS_LAST) {
  cat("\n--- Reform–reform pair exclusions (repeal rankers) ---\n")
  print(summarise_repeal_ranker_exclusions(pairs_reform_reform_raw, pairs_reform_reform))
}

v_tables_reform_reform <- build_all_v_tables_reform_reform(
  pairs_reform_reform,
  attributes   = CONJOINT_ATTR_COLS,
  level_lookup = conjoint_level_lookup
)

saveRDS(pairs_reform_reform, PATH_PAIRS_REFORM_REFORM)
saveRDS(v_tables_reform_reform, PATH_VTABLES_REFORM_REFORM)

cat("\n--- Reform–reform V tables by attribute ---\n")
print(summarise_v_tables(v_tables_reform_reform))
cat("\nSaved:\n")
cat("  ", PATH_PAIRS_REFORM_REPEAL, "\n")
cat("  ", PATH_PAIRS_REFORM_REFORM, "\n")
cat("  ", PATH_VTABLES_REFORM_REFORM, "\n")

# ---- 5. Reform–reform ACP and package P(reform beats repeal) ------------------
# Non-committed sample (repeal-ranker exclusions). ACP is estimated and printed
# for the console; SI DPP tables and ACP figures are written from the full
# sample in §8 (matching the main-text ACP).

ensure_conjacp_loaded()

PATH_ACP_REFORM_REFORM <- "output/acp/acp_results_reform_reform.rds"

acp_countries <- sort(unique(data_conjoint_all$Country))

acp_results_reform_reform <- estimate_acp_from_v_tables(
  v_tables_reform_reform,
  level_lookup     = conjoint_level_lookup,
  countries        = acp_countries,
  estimand         = "acp",
  adjust           = FALSE
)

saveRDS(acp_results_reform_reform, PATH_ACP_REFORM_REFORM)

acp_pooled <- wrap_pooled_acp_as_country(
  estimate_pooled_acp_from_v_tables(
    v_tables_reform_reform, conjoint_level_lookup
  ),
  "Pooled"
)
tab_acp <- tidy_acp_si_table(
  acp_results_reform_reform,
  conjoint_level_lookup,
  pooled = acp_pooled
)

# Package P(reform ≻ repeal) is the mean of pair `outcome`, not Ganter V / ACP.
PATH_P_REFORM_BEATS_REPEAL <- "output/acp/p_reform_beats_repeal.csv"
tab_p_reform_beats_repeal <- summarise_p_reform_beats_repeal_main(
  pairs_reform_repeal,
  level_lookup = conjoint_level_lookup,
  countries    = acp_countries
)
readr::write_csv(tab_p_reform_beats_repeal, PATH_P_REFORM_BEATS_REPEAL)

cat("\n--- Ganter ACP (reform–reform, non-committed), pooled + by country ---\n")
print(tab_acp, n = 40)

cat("\n--- P(reform beats repeal): budget control (marginal) + packages ---\n")
print(tab_p_reform_beats_repeal %>% dplyr::arrange(Country, scenario_type, scenario, d1))

cat("\nSaved:\n")
cat("  ", PATH_ACP_REFORM_REFORM, "\n")
cat("  ", PATH_P_REFORM_BEATS_REPEAL, "\n")

# ---- 6. (ACP figures: full sample only, written in §8) ------------------------

# ---- 7. Main P(reform beats repeal) figure ------------------------------------
# Two samples: non-committed (repeal-ranker exclusions) and Q46_2 opposers
# (strongly/rather oppose after the information block). Opposers are the full
# subgroup: dropping always-rank-repeal-first would remove committed opposers.

opposer_ids_q46_2 <- data_conjoint_all %>%
  dplyr::filter(.data$Q46_2_opposer) %>%
  dplyr::distinct(Country, ID)

pairs_reform_repeal_opp_q46_2 <- pairs_reform_repeal_raw %>%
  dplyr::semi_join(opposer_ids_q46_2, by = c("Country", "ID"))

cat("\n--- Opposer subgroup (Q46_2 = strongly/rather oppose): respondents per country ---\n")
print(opposer_ids_q46_2 %>% dplyr::count(Country))

sample_opposers_q46_2 <- build_acp_subgroup_bundle(
  pairs_reform_repeal_raw,
  pairs_reform_reform_raw,
  opposer_ids_q46_2,
  level_lookup                = conjoint_level_lookup,
  countries                   = acp_countries,
  exclude_repeal_always_first = FALSE,
  exclude_repeal_always_last  = FALSE
)

cat("\n--- Main figure: bundled min vs max packages ---\n")
save_main_p_reform_beats_repeal_figures(
  pairs_non_committed = pairs_reform_repeal,
  pairs_opposers      = pairs_reform_repeal_opp_q46_2,
  level_lookup        = conjoint_level_lookup,
  countries           = acp_countries
)

save_aux_p_reform_beats_repeal_figures(
  pairs_reform_repeal         = pairs_reform_repeal,
  level_lookup                = conjoint_level_lookup,
  countries                   = acp_countries,
  exclude_repeal_always_first = EXCLUDE_REPEAL_ALWAYS_FIRST,
  exclude_repeal_always_last  = EXCLUDE_REPEAL_ALWAYS_LAST
)

save_aux_p_reform_beats_repeal_figures(
  pairs_reform_repeal         = sample_opposers_q46_2$pairs_reform_repeal,
  level_lookup                = conjoint_level_lookup,
  countries                   = acp_countries,
  exclude_repeal_always_first = FALSE,
  exclude_repeal_always_last  = FALSE,
  file_suffix                 = "_opposers_q46_2",
  panel_b_x_limits            = c(0.12, 0.68),
  min_n                       = 25L
)

# ACP level-by-level panels for the Q46_2 opposer subgroup (pooled + A4 country
# page), mirroring the full-sample ACP figures written in §8.
save_acp_reform_reform_figures(
  sample_opposers_q46_2$acp_results_reform_reform,
  pairs_reform_reform         = sample_opposers_q46_2$pairs_reform_reform,
  level_lookup                = conjoint_level_lookup,
  exclude_repeal_always_first = FALSE,
  exclude_repeal_always_last  = FALSE,
  countries                   = acp_countries,
  file_suffix                 = "_opposers_q46_2"
)

# ---- 8. Full-sample ACP figures, DPP SI tables, and other subgroups -----------
# ACP figures (pooled + A4 country page) and DPP SI tables use the full sample
# (no repeal-ranker exclusions), matching the main-text ACP. The main P(reform
# beats repeal) figure in §7 does not include the full sample (too many overlays).
sample_full <- build_acp_sample_bundle(
  pairs_reform_repeal_raw,
  pairs_reform_reform_raw,
  level_lookup                = conjoint_level_lookup,
  countries                   = acp_countries,
  exclude_repeal_always_first = FALSE,
  exclude_repeal_always_last  = FALSE
)

cat("\n--- Full sample (incl always-rank-repeal-first & -last): pair counts ---\n")
print(summarise_repeal_ranker_exclusions(
  pairs_reform_repeal_raw, sample_full$pairs_reform_repeal
))

v_tables_full <- build_all_v_tables_reform_reform(
  sample_full$pairs_reform_reform,
  attributes   = CONJOINT_ATTR_COLS,
  level_lookup = conjoint_level_lookup
)
tab_dpp <- tidy_dpp_si_table(
  build_dpp_results_table(
    v_tables_full, conjoint_level_lookup, acp_countries
  ),
  conjoint_level_lookup
)
dpp_tex_paths <- write_dpp_si_tex_tables(tab_dpp)

cat("\n--- Ganter DPP (reform–reform), full sample, pooled + by country ---\n")
print(tab_dpp, n = 30)

save_acp_reform_reform_figures(
  sample_full$acp_results_reform_reform,
  pairs_reform_reform         = sample_full$pairs_reform_reform,
  level_lookup                = conjoint_level_lookup,
  exclude_repeal_always_first = FALSE,
  exclude_repeal_always_last  = FALSE,
  countries                   = acp_countries
)

cat("\nSaved DPP SI tables:\n")
for (p in dpp_tex_paths) cat("  ", p, "\n")

save_aux_p_reform_beats_repeal_figures(
  pairs_reform_repeal         = sample_full$pairs_reform_repeal,
  level_lookup                = conjoint_level_lookup,
  countries                   = acp_countries,
  exclude_repeal_always_first = FALSE,
  exclude_repeal_always_last  = FALSE
)

# Q61A vote-to-abolish is a robustness opposer definition (vs Q46_2 in §7).
# Full subgroup, no repeal-ranker exclusions. X-axis widened because opposer
# P(beat repeal) sits well below 50%.
q61a_lookup <- data_conjoint_all %>%
  dplyr::distinct(Country, ID, Q61A_response)

opposer_ids_q61a <- q61a_lookup %>%
  dplyr::filter(.data$Q61A_response == "Yes") %>%
  dplyr::distinct(Country, ID)

pairs_reform_repeal_opp_q61a <- pairs_reform_repeal_raw %>%
  dplyr::semi_join(opposer_ids_q61a, by = c("Country", "ID"))

cat("\n--- Opposer subgroup (Q61A = vote to abolish): respondents per country ---\n")
print(opposer_ids_q61a %>% dplyr::count(Country))

save_aux_p_reform_beats_repeal_figures(
  pairs_reform_repeal         = pairs_reform_repeal_opp_q61a,
  level_lookup                = conjoint_level_lookup,
  countries                   = acp_countries,
  exclude_repeal_always_first = FALSE,
  exclude_repeal_always_last  = FALSE,
  file_suffix                 = "_opposers_q61a",
  panel_b_x_limits            = c(0.12, 0.68),
  min_n                       = 25L
)

# Crosstab of the two opposition measures (respondent-level, all countries).
cat("\n--- Crosstab: Q46_2 support (rows) x Q61A repeal vote (cols) ---\n")
print(
  data_conjoint_all %>%
    dplyr::distinct(Country, ID, Q61A_response, Q46_2_cat) %>%
    dplyr::count(Q46_2_cat, Q61A_response) %>%
    tidyr::pivot_wider(names_from = Q61A_response, values_from = n, values_fill = 0)
)

# ---- 9. Subgroup-vs-rest DPP (targeted attribute × subgroup) ------------------
# For each policy-relevant match, test whether the DPP level contrasts differ
# between a subgroup and the rest (full sample; respondent-clustered SEs):
#   household_support           × cost share ≥ country average   (vs below-avg)
#   community_mobility_support  × poor local transport (Q16 bottom 2)
#   worker_support              × high-C worker (High+Medium, vs low-C worker)
# est_diff = subgroup − rest; p_diff tests that difference.
PATH_DPP_SUBGROUP <- "output/acp/dpp_subgroup_diff.csv"

respondent_covs <- data_conjoint_all %>%
  dplyr::distinct(Country, ID, cost_share_above_avg, transport_poor, high_c_broad)

subgroup_specs <- list(
  list(attribute = "household_support",          group_col = "cost_share_above_avg",
       na_as_rest = FALSE, label = "Cost share above country avg"),
  list(attribute = "community_mobility_support", group_col = "transport_poor",
       na_as_rest = FALSE, label = "Poor local transport"),
  list(attribute = "worker_support",             group_col = "high_c_broad",
       na_as_rest = FALSE, label = "High-C worker (High+Medium)")
)

dpp_subgroup_diff <- build_dpp_subgroup_diff_all(
  sample_full$pairs_reform_reform,
  respondent_covs,
  subgroup_specs,
  level_lookup = conjoint_level_lookup,
  countries    = acp_countries
)

readr::write_csv(dpp_subgroup_diff, PATH_DPP_SUBGROUP)

cat("\n--- Subgroup-vs-rest DPP: respondents per group (pooled) ---\n")
print(
  dpp_subgroup_diff %>%
    dplyr::filter(Country == "Pooled") %>%
    dplyr::distinct(subgroup, n_sub, n_rest)
)

cat("\n--- Subgroup-vs-rest DPP contrasts (pooled; est_diff = subgroup - rest) ---\n")
print(
  dpp_subgroup_diff %>%
    dplyr::filter(Country == "Pooled") %>%
    dplyr::transmute(
      subgroup,
      contrast = paste(level_a, "vs", level_b),
      est_rest = round(est_rest, 3), est_sub = round(est_sub, 3),
      est_diff = round(est_diff, 3), se_diff = round(se_diff, 3),
      p_diff = signif(p_diff, 2)
    ),
  n = 60
)

dpp_subgroup_tex <- write_dpp_subgroup_diff_tex(dpp_subgroup_diff)

cat("\nSaved:\n  ", PATH_DPP_SUBGROUP, "\n  ", dpp_subgroup_tex, "\n")

# ---- 9b. Subgroup-vs-rest difference in P(reform > repeal) --------------------
# Complements §9: DPP tests within-reform level trade-offs; this tests the
# reform-vs-repeal margin (does the subgroup support reform overall more/less?).
# Full sample (no repeal-ranker exclusions), all reform–repeal pairs — for the
# high-C worker spec this is NOT restricted to the worker_support arm, since
# overall reform support is not attribute-specific. Same subgroup definitions.
PATH_P_REFORM_DIFF <- "output/acp/p_reform_diff_subgroup.csv"

p_reform_diff <- build_p_reform_diff_all(
  pairs_reform_repeal_raw,
  respondent_covs,
  subgroup_specs,
  countries = acp_countries
)

readr::write_csv(p_reform_diff, PATH_P_REFORM_DIFF)

cat("\n--- Subgroup-vs-rest P(reform > repeal): diff = subgroup - rest ---\n")
print(
  p_reform_diff %>%
    dplyr::transmute(
      subgroup, Country,
      p_rest = round(p_rest, 3), p_sub = round(p_sub, 3),
      diff = round(diff, 3), se_diff = round(se_diff, 3),
      p_value = signif(p_value, 2), n_sub, n_rest
    ),
  n = 30
)

p_reform_diff_tex <- write_p_reform_diff_tex(p_reform_diff)

cat("\nSaved:\n  ", PATH_P_REFORM_DIFF, "\n  ", p_reform_diff_tex, "\n")

# Plots for Script ####

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

