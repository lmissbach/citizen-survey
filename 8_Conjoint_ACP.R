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
