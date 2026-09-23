# conjoint_prepare.R — analysis-sample long data for conjoint_acp.R
#
# Qualtrics → quality filters → inclusion (speed / empty attitude blocks /
# no conjoint) → long extract → attribute factors → d1/d2, Q61A, Q46_2.
# Inclusion is applied once on wide data, before extract_conjoint().
# Always-B (ranked B first on all 4 tasks) is dropped from the conjoint long
# file and from the QT7–QT11 timing CDFs (not from Q62–Q68 shares).
# Also: Q62–Q68 shares, QT7–QT11 page-time CDFs, rank-option check by task,
# repeal-rank vs opposition coherence.

PATH_CONJOINT_ALL <- "output/acp/data_conjoint_all.rds"
PATH_LEVEL_LOOKUP <- "output/acp/conjoint_level_lookup.rds"

# Quality filter on the long conjoint file: drop people who ranked B first
# on all four tasks (B is always the middle option in the A/B/C list).
EXCLUDE_ALWAYS_PICK_B <- TRUE

required_pkgs <- c("tidyverse", "jsonlite", "stringi")
for (pkg in required_pkgs) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

source("8_Conjoint_Prepare_Helpers.R")
source("8_Subgroup_Covariates_Helpers.R")

dir.create("output/acp", recursive = TRUE, showWarnings = FALSE)
set.seed(2026)

# ---- 1. Wide country files + inclusion ---------------------------------------

wide_list <- lapply(COUNTRY_LEVELS_4, function(country) {
  spec <- COUNTRY_SURVEY[[country]]
  path <- resolve_survey_path(spec$file)
  message("Reading ", path)
  raw <- readr::read_csv(path, show_col_types = FALSE)
  prep_country_wide(raw, country, spec)
})
names(wide_list) <- COUNTRY_LEVELS_4

inclusion_n <- dplyr::bind_rows(
  lapply(COUNTRY_LEVELS_4, function(country) {
    inclusion_attrition(wide_list[[country]], country)
  })
)
cat("\n--- Inclusion attrition (wide data, before extract_conjoint) ---\n")
print(inclusion_n)

wide_list <- lapply(wide_list, apply_analysis_inclusion)

# ---- 2. Long conjoint + attribute factors ------------------------------------

long_list <- lapply(COUNTRY_LEVELS_4, function(country) {
  extract_conjoint(wide_list[[country]]) %>%
    apply_conjoint_attribute_factors(country) %>%
    dplyr::mutate(Country = country)
})
names(long_list) <- COUNTRY_LEVELS_4

na_level_check <- purrr::map_dfr(COUNTRY_LEVELS_4, function(country) {
  dat <- long_list[[country]] %>% dplyr::filter(.data$Profile != "C")
  purrr::map_dfr(CONJOINT_ATTR_COLS, function(attr) {
    tibble::tibble(
      Country   = country,
      attribute = attr,
      n_na      = sum(is.na(dat[[attr]]))
    )
  })
}) %>%
  dplyr::filter(.data$n_na > 0)
if (nrow(na_level_check)) {
  cat("\n--- Non-repeal rows with NA after factor() (unmatched JSON strings) ---\n")
  print(na_level_check)
}

# ---- 3. d1/d2, Q61A, Q46_2, level lookup -------------------------------------

wide_all <- dplyr::bind_rows(wide_list)

conjoint_design_split <- wide_all %>%
  dplyr::transmute(Country, ID, d1, d2) %>%
  dplyr::filter(
    .data$d1 %in% c("worker_support", "community_mobility_support"),
    .data$d2 %in% c("information", "infrastructure_ownership")
  )

q62_q68_label_map <- readr::read_csv(
  "data/q62_q68_response_labels.csv",
  show_col_types = FALSE
)
level_short <- build_level_short_harmonised(q62_q68_label_map)

conjoint_level_lookup <- dplyr::bind_rows(
  lapply(COUNTRY_LEVELS_4, function(country) {
    build_conjoint_level_lookup(long_list[[country]], country, level_short)
  })
)

data_conjoint_all <- dplyr::bind_rows(long_list) %>%
  dplyr::left_join(conjoint_design_split, by = c("Country", "ID")) %>%
  dplyr::left_join(
    wide_all %>% dplyr::distinct(Country, ID, ResponseId),
    by = c("Country", "ID")
  ) %>%
  dplyr::left_join(harmonise_q61a(wide_all), by = c("Country", "ID")) %>%
  dplyr::left_join(harmonise_q46_2(wide_all), by = c("Country", "ID")) %>%
  dplyr::left_join(
    build_subgroup_covariates(wide_all, coded_path = "data/q14_sector_coded.csv",
                              cost_threshold = "mean", cost_by_country = TRUE),
    by = c("Country", "ID")
  ) %>%
  dplyr::mutate(
    Q46_2_opposer = .data$Q46_2_cat %in% c("Strongly oppose", "Rather oppose")
  ) %>%
  dplyr::select(dplyr::any_of(ACP_LONG_COLS)) %>%
  dplyr::mutate(Country = factor(.data$Country, levels = COUNTRY_LEVELS_4))

n_missing_rid <- sum(is.na(data_conjoint_all$ResponseId))
n_dup_rid <- data_conjoint_all %>%
  dplyr::distinct(Country, ID, ResponseId) %>%
  dplyr::count(Country, ResponseId) %>%
  dplyr::filter(.data$n > 1L) %>%
  nrow()
if (n_missing_rid > 0 || n_dup_rid > 0) {
  stop(
    "ResponseId join failed: missing=", n_missing_rid,
    " duplicate Country×ResponseId=", n_dup_rid,
    call. = FALSE
  )
}

cat("\n--- Prepared long data (respondents) ---\n")
print(data_conjoint_all %>% dplyr::distinct(Country, ID) %>% dplyr::count(Country))
cat("Rows (ID × task × profile): ", nrow(data_conjoint_all), "\n", sep = "")

cat("\n--- SI subgroup covariates: respondents per country ---\n")
print(
  data_conjoint_all %>%
    dplyr::distinct(Country, ID, high_c_narrow, high_c_broad, is_worker,
                    transport_poor, cost_share_above_avg) %>%
    dplyr::group_by(Country) %>%
    dplyr::summarise(
      n                    = dplyr::n(),
      workers              = sum(.data$is_worker, na.rm = TRUE),
      high_c_narrow        = sum(.data$high_c_narrow, na.rm = TRUE),
      high_c_broad         = sum(.data$high_c_broad, na.rm = TRUE),
      transport_poor       = sum(.data$transport_poor, na.rm = TRUE),
      cost_share_above_avg = sum(.data$cost_share_above_avg, na.rm = TRUE),
      cost_share_missing   = sum(is.na(.data$cost_share_above_avg)),
      .groups = "drop"
    )
)

always_b_flags <- build_always_pick_b_flags(data_conjoint_all)
always_b_drop  <- summarise_always_pick_b_drop(always_b_flags)
PATH_ALWAYS_B  <- "output/acp/always_pick_b_attrition.csv"
readr::write_csv(always_b_drop, PATH_ALWAYS_B)
cat("\n--- Always-B first rank (4 complete tasks) ---\n")
print(
  always_b_drop %>%
    dplyr::mutate(share_always_b = round(.data$share_always_b, 3))
)
if (EXCLUDE_ALWAYS_PICK_B) {
  data_conjoint_all <- filter_always_pick_b(data_conjoint_all, always_b_flags)
  cat(
    "Dropped always-B from conjoint long data. Remaining respondents: ",
    data_conjoint_all %>%
      dplyr::distinct(.data$Country, .data$ID) %>%
      nrow(),
    "\n",
    sep = ""
  )
}

# ---- 4. Q62–Q68 shares, conjoint page timing, display-order check ------------

q62_q68_long <- tidy_q62_q68(wide_all, q62_q68_label_map)
n_unmapped <- sum(q62_q68_long$response_label == "Unmapped")
if (n_unmapped > 0) {
  warning("Q62–Q68: ", n_unmapped, " responses not in the label map.", call. = FALSE)
}
tab_q62_q68 <- summarise_q62_q68(q62_q68_long)
q62_q68_tex_paths <- write_q62_q68_tex_tables(tab_q62_q68)
saveRDS(q62_q68_long, "output/acp/q62_q68_respondent.rds")

cat("\n--- Q62–Q68 shares (among those assigned to the question) ---\n")
print_q62_q68_shares(tab_q62_q68)

conjoint_timing <- extract_conjoint_timing(wide_all)
if (EXCLUDE_ALWAYS_PICK_B) {
  conjoint_timing <- conjoint_timing %>%
    dplyr::semi_join(
      data_conjoint_all %>% dplyr::distinct(.data$Country, .data$ID),
      by = c("Country", "ID")
    )
}
tab_timing <- summarise_conjoint_timing(conjoint_timing)
PATH_CONJOINT_TIMING <- "output/acp/conjoint_timing.csv"
readr::write_csv(conjoint_timing, PATH_CONJOINT_TIMING)
readr::write_csv(tab_timing, "output/acp/conjoint_timing_summary.csv")
saveRDS(conjoint_timing, "output/acp/conjoint_timing.rds")

cat("\n--- Conjoint page time (seconds, Page Submit), by country ---\n")
print(tab_timing, n = 25)

dir.create("figures/acp", recursive = TRUE, showWarnings = FALSE)
p_timing <- plot_conjoint_timing_cdf(
  conjoint_timing,
  sample_note = if (EXCLUDE_ALWAYS_PICK_B) {
    "Respondents who ranked B first on all four tasks are excluded."
  } else {
    NULL
  }
)
PATH_TIMING_CDF <- "figures/acp/conjoint_page_time_cdf.pdf"
ggplot2::ggsave(PATH_TIMING_CDF, p_timing, width = 170 / 25.4, height = 220 / 25.4)

order_check <- summarise_display_order(data_conjoint_all)
cat("\n--- Step-1 display order (C*_1_DO → display_order_step1) ---\n")
cat(
  "Tasks: ", order_check$n_tasks,
  "; missing order: ", order_check$n_missing_order, "\n",
  sep = ""
)
print(order_check$order_counts)

tab_first_pos <- summarise_first_position_check(data_conjoint_all)
PATH_FIRST_POS_TEX <- write_first_position_tex(tab_first_pos, always_b_drop)
p_first_pos <- plot_first_position_check(tab_first_pos)
PATH_FIRST_POS_FIG <- "figures/acp/conjoint_first_position.pdf"
ggplot2::ggsave(PATH_FIRST_POS_FIG, p_first_pos, width = 170 / 25.4, height = 95 / 25.4)
cat("\n--- First-rank shares by task (after always-B drop) ---\n")
print(
  tab_first_pos$by_task %>%
    dplyr::mutate(dplyr::across(dplyr::starts_with("p_"), ~ round(.x, 3)))
)

repeal_opp_resp <- build_repeal_opposition_respondent(data_conjoint_all)
tab_repeal_opp <- summarise_repeal_opposition(repeal_opp_resp)
PATH_REPEAL_OPP_TEX <- write_repeal_opposition_tex(tab_repeal_opp)
PATH_REPEAL_OPP_CSV <- "output/acp/repeal_opposition.csv"
readr::write_csv(
  dplyr::bind_rows(
    tab_repeal_opp$q46 %>% dplyr::mutate(source = "Q46_2", .before = 1),
    tab_repeal_opp$q61a %>% dplyr::mutate(source = "Q61A", .before = 1)
  ),
  PATH_REPEAL_OPP_CSV
)
p_repeal_opp <- plot_repeal_opposition(tab_repeal_opp)
PATH_REPEAL_OPP_FIG <- "figures/acp/repeal_opposition.pdf"
ggplot2::ggsave(PATH_REPEAL_OPP_FIG, p_repeal_opp, width = 170 / 25.4, height = 95 / 25.4)
cat("\n--- Repeal rank vs opposition (after always-B drop) ---\n")
print(tab_repeal_opp$q46 %>% dplyr::mutate(dplyr::across(dplyr::starts_with("p_"), ~ round(.x, 3))))
print(tab_repeal_opp$q61a %>% dplyr::mutate(dplyr::across(dplyr::starts_with("p_"), ~ round(.x, 3))))

cat("\n--- Profile share by on-screen position (1 = left … 3 = right) ---\n")
print(
  order_check$position_share %>%
    dplyr::mutate(share = round(.data$share, 3)),
  n = 40
)

# ---- 5. Save -----------------------------------------------------------------

saveRDS(data_conjoint_all, PATH_CONJOINT_ALL)
saveRDS(conjoint_level_lookup, PATH_LEVEL_LOOKUP)
cat("\nWrote:\n  ", PATH_CONJOINT_ALL, "\n  ", PATH_LEVEL_LOOKUP, "\n", sep = "")
cat("  ", paste(q62_q68_tex_paths, collapse = "\n  "), "\n", sep = "")
cat("  ", PATH_CONJOINT_TIMING, "\n", sep = "")
cat("  ", PATH_TIMING_CDF, "\n", sep = "")
cat("  ", PATH_ALWAYS_B, "\n", sep = "")
cat("  ", PATH_FIRST_POS_TEX, "\n", sep = "")
cat("  ", PATH_FIRST_POS_FIG, "\n", sep = "")
cat("  ", PATH_REPEAL_OPP_TEX, "\n", sep = "")
cat("  ", PATH_REPEAL_OPP_FIG, "\n", sep = "")
cat("  ", PATH_REPEAL_OPP_CSV, "\n", sep = "")
