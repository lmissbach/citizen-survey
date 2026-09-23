# subgroup_covariates_helpers.R
# Respondent-level covariates for the SI subgroup analyses, built from the wide
# analysis sample (wide_all in conjoint_prepare.R) so IDs match data_conjoint_all.
#
# Provides:
#   normalize_key()            Layer-1 string normalisation (shared with q14_sector_coding.R)
#   attach_q14_sector()        join data/q14_sector_coded.csv -> sector tier + high-C flags
#   harmonise_transport_q16()  local public-transport availability (5-pt) -> transport_poor
#   compute_cost_share()       expected annual cost (Q42_2) / annualised spend (Q28) -> above-average flag
#   build_subgroup_covariates() one respondent-level table joining all of the above
#
# Definitions (locked with co-author):
#   high_c  = tier %in% c("High","Medium")   (broad; narrow = "High" also kept)
#   transport_poor = Q16 in bottom two ("bad"/"very bad")
#   cost_share_above_avg = cost_share > mean(cost_share) over non-missing (pooled)

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
  library(stringi)
  library(readr)
})

# ---- Layer-1 normalisation (identical on coding and merge sides) -------------
normalize_key <- function(x) {
  x <- as.character(x)
  x <- stri_trans_nfc(x)
  x <- str_replace_all(x, "\u00a0", " ")
  x <- str_replace_all(x, "[\u2018\u2019\u201c\u201d]", "'")
  x <- str_trim(x)
  x <- stri_trans_tolower(x)
  x <- stri_trans_general(x, "Latin-ASCII")
  x <- str_replace_all(x, "[/,;.|]+", " ")
  x <- str_replace_all(x, "\\s*-\\s*", " ")
  x <- str_replace_all(x, "\\s+", " ")
  str_trim(x)
}

# ---- Q14 sector -> tier + high-C flags ---------------------------------------
# Left-joins the coded table on (Country, normalized_key). Blank/Unclassifiable
# tiers -> NA (unknown), so they are excluded from the high-C subgroup rather
# than silently counted as "not high-C".
attach_q14_sector <- function(wide, coded_path = "data/q14_sector_coded.csv") {
  if (!file.exists(coded_path)) {
    stop("Sector coding not found: ", coded_path,
         " — run q14_sector_coding.R first.", call. = FALSE)
  }
  coded <- readr::read_csv(coded_path, show_col_types = FALSE) %>%
    dplyr::select("country", "normalized_key", "nace_div", "sector_en", "tier") %>%
    dplyr::distinct(.data$country, .data$normalized_key, .keep_all = TRUE)

  wide %>%
    dplyr::transmute(
      .data$Country, .data$ID,
      Q14_raw        = str_trim(as.character(.data$Q14)),
      normalized_key = normalize_key(.data$Q14)
    ) %>%
    dplyr::left_join(
      coded, by = c("Country" = "country", "normalized_key")
    ) %>%
    dplyr::mutate(
      is_worker   = !is.na(.data$Q14_raw) & .data$Q14_raw != "",
      sector_tier = dplyr::na_if(.data$tier, ""),
      sector_tier = ifelse(.data$sector_tier == "Unclassifiable", NA, .data$sector_tier),
      high_c_narrow = dplyr::case_when(
        .data$sector_tier == "High"               ~ TRUE,
        .data$sector_tier %in% c("Medium", "Low") ~ FALSE,
        TRUE                                      ~ NA
      ),
      high_c_broad = dplyr::case_when(
        .data$sector_tier %in% c("High", "Medium") ~ TRUE,
        .data$sector_tier == "Low"                 ~ FALSE,
        TRUE                                       ~ NA
      )
    ) %>%
    dplyr::select("Country", "ID", "sector_en", "sector_tier",
                  "is_worker", "high_c_narrow", "high_c_broad")
}

# ---- Q16 local public-transport availability (5-pt) --------------------------
# Bottom two categories ("bad"/"very bad") = poor local transport.
Q16_LEVEL_MAP <- c(
  # very good = 5
  "muy buena" = 5L, "tres bon" = 5L, "sehr gut" = 5L, "foarte buna" = 5L,
  # good = 4
  "buena" = 4L, "bon" = 4L, "gut" = 4L, "buna" = 4L,
  # neither = 3
  "regular (ni buena ni mala)" = 3L, "ni bon ni mauvais" = 3L,
  "weder gut noch schlecht" = 3L, "nici buna, nici proasta" = 3L,
  # bad = 2
  "mala" = 2L, "mauvais" = 2L, "schlecht" = 2L, "proasta" = 2L,
  # very bad = 1
  "muy mala" = 1L, "tres mauvais" = 1L, "sehr schlecht" = 1L, "foarte proasta" = 1L
)

harmonise_transport_q16 <- function(wide) {
  wide %>%
    dplyr::transmute(
      .data$Country, .data$ID,
      .q16 = stri_trans_general(stri_trans_tolower(str_trim(as.character(.data$Q16))),
                                "Latin-ASCII")
    ) %>%
    dplyr::mutate(
      transport_level = unname(Q16_LEVEL_MAP[.data$.q16]),
      transport_poor  = dplyr::case_when(
        is.na(.data$transport_level) ~ NA,
        .data$transport_level <= 2L  ~ TRUE,
        TRUE                         ~ FALSE
      )
    ) %>%
    dplyr::select("Country", "ID", "transport_level", "transport_poor")
}

# ---- Expected cost as a share of income (Q42_2 / annualised Q28) -------------
# Q42_2: expected ANNUAL cost increase, bracketed by respondent thresholds t1..t6.
#   below t1 -> t1/2 ; between t_i,t_j -> mean ; above t6 -> t6*1.25
# Q28: average MONTHLY household spend (income proxy), bracketed with numeric
#   labels; annualised (x12). Same currency as t within each country (checked:
#   RO both in lei, others in EUR), so the share is a unit-free within-respondent ratio.
.CS_BELOW <- "menos|moins|weniger|mai putin|\\bsub\\b|unter|less than"
.CS_ABOVE <- "\\bmas|plus|mehr|mai mult|peste|uber|more than"

parse_expected_cost <- function(q42, t1, t2, t3, t4, t5, t6) {
  s <- stri_trans_general(stri_trans_tolower(as.character(q42)), "Latin-ASCII")
  if (is.na(s) || s == "") return(NA_real_)
  tv <- suppressWarnings(as.numeric(c(t1, t2, t3, t4, t5, t6)))
  idx <- as.integer(str_remove(unlist(str_extract_all(s, "t[1-6]")), "t"))
  idx <- sort(unique(idx))
  if (length(idx) == 0 || all(is.na(tv))) return(NA_real_)
  if (str_detect(s, .CS_BELOW)) return(tv[min(idx)] / 2)
  if (str_detect(s, .CS_ABOVE)) return(tv[max(idx)] * 1.25)
  if (length(idx) >= 2)         return(mean(tv[idx[1:2]], na.rm = TRUE))
  NA_real_
}

parse_income_monthly <- function(q28) {
  s <- stri_trans_general(stri_trans_tolower(str_trim(as.character(q28))), "Latin-ASCII")
  if (is.na(s) || s == "" ||
      str_detect(s, "no lo se|je ne sais|weiss nicht|nu stiu|don t know|dont know")) {
    return(NA_real_)
  }
  nums <- suppressWarnings(as.numeric(
    str_remove_all(unlist(str_extract_all(s, "[0-9][0-9.,]*")), "[.,]")
  ))
  nums <- nums[!is.na(nums)]
  if (length(nums) == 0) return(NA_real_)
  if (str_detect(s, .CS_BELOW)) return(nums[1] * 0.75)
  if (str_detect(s, .CS_ABOVE)) return(nums[length(nums)] * 1.25)
  mean(nums[1:2], na.rm = TRUE)
}

compute_cost_share <- function(wide, threshold = c("mean", "median"),
                               by_country = FALSE) {
  threshold <- match.arg(threshold)
  d <- wide %>%
    dplyr::transmute(
      .data$Country, .data$ID,
      Q42_2 = as.character(.data$Q42_2), Q28 = as.character(.data$Q28),
      t1 = .data$t1, t2 = .data$t2, t3 = .data$t3,
      t4 = .data$t4, t5 = .data$t5, t6 = .data$t6
    )
  d$expected_cost_amt <- purrr::pmap_dbl(
    list(d$Q42_2, d$t1, d$t2, d$t3, d$t4, d$t5, d$t6), parse_expected_cost
  )
  d$income_monthly <- vapply(d$Q28, parse_income_monthly, numeric(1))
  d <- d %>%
    dplyr::mutate(
      income_annual = .data$income_monthly * 12,
      cost_share    = ifelse(!is.na(.data$income_annual) & .data$income_annual > 0,
                             .data$expected_cost_amt / .data$income_annual, NA_real_)
    )
  cut_fun <- if (threshold == "mean") function(x) mean(x, na.rm = TRUE) else
    function(x) stats::median(x, na.rm = TRUE)
  d <- if (by_country) {
    d %>% dplyr::group_by(.data$Country) %>%
      dplyr::mutate(.cut = cut_fun(.data$cost_share)) %>% dplyr::ungroup()
  } else {
    d %>% dplyr::mutate(.cut = cut_fun(.data$cost_share))
  }
  d %>%
    dplyr::mutate(
      cost_share_above_avg = dplyr::case_when(
        is.na(.data$cost_share) ~ NA,
        .data$cost_share > .data$.cut ~ TRUE,
        TRUE ~ FALSE
      )
    ) %>%
    dplyr::select("Country", "ID", "expected_cost_amt", "income_annual",
                  "cost_share", "cost_share_above_avg")
}

# ---- One respondent-level covariate table -----------------------------------
build_subgroup_covariates <- function(wide,
                                      coded_path      = "data/q14_sector_coded.csv",
                                      cost_threshold  = "mean",
                                      cost_by_country = FALSE) {
  sector    <- attach_q14_sector(wide, coded_path)
  transport <- harmonise_transport_q16(wide)
  cost      <- compute_cost_share(wide, threshold = cost_threshold,
                                  by_country = cost_by_country)
  sector %>%
    dplyr::left_join(transport, by = c("Country", "ID")) %>%
    dplyr::left_join(cost,      by = c("Country", "ID"))
}
