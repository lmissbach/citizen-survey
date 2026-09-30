# =============================================================================
# 10_Survey_Weights.R — Raking weights for the final survey samples
# =============================================================================
# Three weight sets per country, raked (iterative proportional fitting) to
#   (1) age band x gender   (Eurostat demo_pjan, population on 1 January)
#   (2) education, 3 levels (Eurostat EDAT_LFS_9901, ISCED 0-2 / 3-4 / 5-8)
#   (3) household expenditure tercile from Q28 (10 bands, household deciles):
#       bands 1-4 = tercile 1, bands 5-7 = tercile 2, bands 8-10 = tercile 3
#
#   w_income      (1) + (2) + (3), income targets 1/3 each (the panel quota)
#   w_income_pop  (1) + (2) + (3), income targets = actual population shares of
#                 the band groups. The bands are household deciles: 40/30/30.
#   w_demo        (1) + (2) only, no income margin
#
# All three sets use the same respondents: those answering "don't know" to Q28
# (or with missing age, gender or education) are excluded and get NA weights,
# so differences between the sets come from the income margin alone.
# Weights are capped to [WEIGHT_MIN, WEIGHT_MAX] (mean 1) inside the raking.
#
# The sample is rebuilt with the same filters as 7_Analysis.R (section 1), so
# Country + ID match the IDs used there and in the conjoint scripts.
#
# Output
#   ../2_Data/1_Support_Datasets/Survey_Weights.csv   one row per respondent
#   output/weights/weights_diagnostics.csv            sample / target / weighted shares
#   output/weights/weights_summary.csv                effective n, weight range
# =============================================================================

# ---- 0. Setup ----------------------------------------------------------------

library(tidyverse)

dir.create("output/weights", recursive = TRUE, showWarnings = FALSE)

PATH_WEIGHTS        <- "../2_Data/1_Support_Datasets/Survey_Weights.csv"
PATH_EUROSTAT_POP   <- "../2_Data/Supplementary/Eurostat_demo_pjan.csv"
PATH_EUROSTAT_EDUC  <- "../2_Data/Supplementary/Eurostat_edat_lfs_9901.csv"

WEIGHT_MIN <- 0.2   # trimming bounds (weights normalised to mean 1)
WEIGHT_MAX <- 5

WEIGHT_SETS <- c("w_income", "w_income_pop", "w_demo")

# Income targets per weight set (w_demo has no income margin)
target_income <- tribble(
  ~weight_set,     ~category, ~target,
  "w_income",      "T1",      1/3,
  "w_income",      "T2",      1/3,
  "w_income",      "T3",      1/3,
  "w_income_pop",  "T1",      0.4,
  "w_income_pop",  "T2",      0.3,
  "w_income_pop",  "T3",      0.3
) %>%
  mutate(variable = "income_tercile")

countries <- tibble(
  Country = c("Spain", "France", "Germany", "Romania"),
  geo     = c("ES",    "FR",     "DE",      "RO")
)

# ---- 1. Survey data (same filters as 7_Analysis.R) ---------------------------

# Country-specific answer labels used in the filters of 7_Analysis.R
survey_files <- tribble(
  ~Country,  ~file,                                                                                 ~no,    ~attention_1, ~attention_2,
  "Spain",   "../2_Data/0_Qualtrics_Output/20260309_Final/Spanish/Spanish_5.+Mai+2026_15.02.csv",   "No",   "De acuerdo", "Energía",
  "France",  "../2_Data/0_Qualtrics_Output/20260309_Final/French/French_9.+März+2026_09.31.csv",    "Non",  "D’accord",   "Énergie",
  "Germany", "../2_Data/0_Qualtrics_Output/20260309_Final/German/German_9.+März+2026_09.31.csv",    "Nein", "Stimme zu",  "Energie",
  "Romania", "../2_Data/0_Qualtrics_Output/20260309_Final/Romanian/Romanian_5.+Mai+2026_15.02.csv", "Nu",   "De acord",   "Energie"
)

read_survey <- function(Country, file, no, attention_1, attention_2) {
  country <- Country
  read_csv(file, show_col_types = FALSE) %>%
    filter(Status == "IP-Adresse") %>%
    mutate(StartDate = as.POSIXct(StartDate, format = "%Y-%m-%d %H:%M:%S"),
           Date      = format(StartDate, "%Y-%m-%d")) %>%
    filter(as.Date(Date) > as.Date("2025-12-17")) %>%
    mutate(Country = country,
           ID      = row_number()) %>%   # IDs assigned at this step, as in 7_Analysis.R
    filter(Q02 != no,
           Finished == "Wahr",
           Q38A == attention_1,
           Q60A == attention_2) %>%
    select(Country, ID, Q10, Q11, Q12, Q28)
}

data_0 <- pmap_dfr(survey_files, read_survey)

# ---- 2. Harmonise weighting variables ----------------------------------------

# Age: 6 bands. Spain asked 55-63 / 64+ instead of 55-64 / 65+; the targets
# below use Spain's own cut-offs. "Other" gender is coded as female, as in
# 7_Analysis.R.
data_1 <- data_0 %>%
  mutate(
    age_lower = as.numeric(str_extract(Q10, "[0-9]+")),
    under_18  = str_detect(Q10, "Menos|Moins|Unter|Sub"),
    age_band  = case_when(
      under_18 | is.na(age_lower) ~ NA_character_,
      age_lower == 18             ~ "18-24",
      age_lower == 25             ~ "25-34",
      age_lower == 35             ~ "35-44",
      age_lower == 45             ~ "45-54",
      age_lower == 55             ~ "55-64",
      age_lower >= 63             ~ "65+"      # Spain: "Más de 63 años" = 64+
    ),
    gender = case_when(
      Q11 %in% c("Hombre", "Masculin", "Männlich")                  ~ "Male",
      Q11 %in% c("Mujer", "Féminin", "Feminin", "Weiblich")         ~ "Female",
      Q11 %in% c("Otros", "Autre", "Andere", "Altul")               ~ "Female"
    ),
    age_gender = if_else(is.na(age_band) | is.na(gender), NA_character_,
                         paste(gender, age_band))
  ) %>%
  select(-age_lower, -under_18)

# Education: ISCED 2011 levels 0-2 / 3-4 / 5-8
educ_map <- tribble(
  ~Q12,                                                                   ~educ,
  # Spain
  "No he completado la enseñanza básica",                                 "ISCED 0-2",
  "Educación primaria",                                                   "ISCED 0-2",
  "Educación secundaria obligatoria (ESO)",                               "ISCED 0-2",
  "Formación profesional básica (FP)",                                    "ISCED 3-4",
  "Bachillerato",                                                         "ISCED 3-4",
  "Formación profesional de grado medio",                                 "ISCED 3-4",
  "Formación profesional de grado superior",                              "ISCED 5-8",
  "Grado universitario",                                                  "ISCED 5-8",
  "Máster/doctorado",                                                     "ISCED 5-8",
  # France
  "Aucun",                                                                "ISCED 0-2",
  "Ecole primaire",                                                       "ISCED 0-2",
  "Brevet",                                                               "ISCED 0-2",
  "CAP ou BEP",                                                           "ISCED 3-4",
  "Baccalauréat",                                                         "ISCED 3-4",
  "Bac +2 ou Bac +3 (license, BTS, DUT, DEUG...)",                        "ISCED 5-8",
  "Bac +5 ou plus (master, école d'ingénieur ou de commerce, doctorat, médecine, maîtrise, DEA, DESS...)", "ISCED 5-8",
  # Germany
  "Keine abgeschlossene Schulbildung",                                    "ISCED 0-2",
  "Grundschule",                                                          "ISCED 0-2",
  "Untere Sekundarstufe (z.B. Haupt- oder Realschulabschluss)",           "ISCED 0-2",
  "Abitur",                                                               "ISCED 3-4",
  "Beruflicher Abschluss / Ausbildung",                                   "ISCED 3-4",
  "Hochschulabschluss (z.B. Bachelor)",                                   "ISCED 5-8",
  "Master-Abschluss oder höher",                                          "ISCED 5-8",
  # Romania
  "Fără educație formală",                                                "ISCED 0-2",
  "Educație primară (Clasele I–IV)",                                      "ISCED 0-2",
  "Educație secundară inferioră (Gimnaziu, Clasele V–VIII)",              "ISCED 0-2",
  "Liceu (Liceu – Clasele IX–XII/XIII)",                                  "ISCED 3-4",
  "Școală profesională",                                                  "ISCED 3-4",
  "Educație postsecundară non-terțiară (Școală postliceală)",             "ISCED 3-4",
  "Diplomă de licență",                                                   "ISCED 5-8",
  "Diplomă de master sau de doctorat",                                    "ISCED 5-8"
)

unmapped_educ <- data_1 %>%
  filter(!is.na(Q12)) %>%
  distinct(Q12) %>%
  anti_join(educ_map, by = "Q12")
if (nrow(unmapped_educ) > 0) {
  print(unmapped_educ)
  stop("Unmapped Q12 labels (printed above).")
}

data_1 <- left_join(data_1, educ_map, by = "Q12")

# Income: Q28 has 10 expenditure bands per country plus "don't know". Bands
# are ordered by their first number ("less than 1,000" < "1,001 to 1,400").
income_bands <- data_1 %>%
  filter(!is.na(Q28)) %>%
  distinct(Country, Q28) %>%
  mutate(lower = as.numeric(str_extract(str_remove_all(Q28, "[.,]"), "[0-9]+"))) %>%
  filter(!is.na(lower)) %>%                     # drops "don't know"
  group_by(Country) %>%
  mutate(income_band = min_rank(lower),
         n_bands     = n()) %>%
  ungroup()

if (any(income_bands$n_bands != 10)) stop("Expected 10 Q28 bands in every country.")

data_1 <- data_1 %>%
  left_join(select(income_bands, Country, Q28, income_band), by = c("Country", "Q28")) %>%
  mutate(income_tercile = case_when(income_band <= 4  ~ "T1",
                                    income_band <= 7  ~ "T2",
                                    income_band <= 10 ~ "T3"),
         income_dk      = !is.na(Q28) & is.na(income_band))

# ---- 3. Population targets (Eurostat) ----------------------------------------
# Downloaded once and cached as CSV in 2_Data/Supplementary.

# The API returns all countries; filtering happens after download.
eurostat_url <- function(dataset) {
  paste0("https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/", dataset,
         "?format=SDMX-CSV&startPeriod=2023")
}

if (!file.exists(PATH_EUROSTAT_POP)) {
  read_csv(eurostat_url("demo_pjan"), show_col_types = FALSE) %>%
    filter(geo %in% countries$geo, sex %in% c("M", "F")) %>%
    select(geo, sex, age, TIME_PERIOD, OBS_VALUE) %>%
    write_csv(PATH_EUROSTAT_POP)
}
if (!file.exists(PATH_EUROSTAT_EDUC)) {
  read_csv(eurostat_url("EDAT_LFS_9901"), show_col_types = FALSE) %>%
    filter(geo %in% countries$geo, sex == "T", unit == "THS_PER") %>%
    select(geo, isced11, age, TIME_PERIOD, OBS_VALUE) %>%
    write_csv(PATH_EUROSTAT_EDUC)
}

pop_raw  <- read_csv(PATH_EUROSTAT_POP,  show_col_types = FALSE)
educ_raw <- read_csv(PATH_EUROSTAT_EDUC, show_col_types = FALSE)

# Latest year available for all four countries
latest_year <- function(d) {
  d %>%
    filter(!is.na(OBS_VALUE)) %>%
    distinct(geo, TIME_PERIOD) %>%
    count(TIME_PERIOD) %>%
    filter(n == nrow(countries)) %>%
    summarise(year = max(TIME_PERIOD)) %>%
    pull(year)
}
year_pop  <- latest_year(pop_raw)
year_educ <- latest_year(educ_raw)

# Age x gender: single years of age, summed to the survey's bands
pop <- pop_raw %>%
  filter(TIME_PERIOD == year_pop, str_detect(age, "^Y[0-9]+$|^Y_OPEN$")) %>%
  mutate(age_num = as.numeric(if_else(age == "Y_OPEN", "100", str_remove(age, "Y")))) %>%
  filter(age_num >= 18) %>%
  left_join(countries, by = "geo") %>%
  mutate(
    top_start = if_else(Country == "Spain", 64, 65),
    age_band  = case_when(age_num <= 24        ~ "18-24",
                          age_num <= 34        ~ "25-34",
                          age_num <= 44        ~ "35-44",
                          age_num <= 54        ~ "45-54",
                          age_num < top_start  ~ "55-64",
                          TRUE                 ~ "65+"),
    gender    = if_else(sex == "M", "Male", "Female")
  )

target_age_gender <- pop %>%
  group_by(Country, category = paste(gender, age_band)) %>%
  summarise(pop = sum(OBS_VALUE), .groups = "drop_last") %>%
  mutate(target = pop / sum(pop)) %>%
  ungroup() %>%
  transmute(Country, variable = "age_gender", category, target)

# Education: LFS attainment shares by age band (population aged 18-74),
# weighted by the population of each band. LFS stops at 74, so the 65-74
# shares (Y55-74 minus Y55-64) are applied to everyone aged 65+.
educ_by_age <- educ_raw %>%
  filter(TIME_PERIOD == year_educ,
         isced11 %in% c("ED0-2", "ED3_4", "ED5-8"),
         age %in% c("Y18-24", "Y25-34", "Y35-44", "Y45-54", "Y55-64", "Y55-74")) %>%
  select(geo, isced11, age, OBS_VALUE) %>%
  pivot_wider(names_from = age, values_from = OBS_VALUE) %>%
  mutate(`Y65-74` = `Y55-74` - `Y55-64`) %>%
  select(-`Y55-74`) %>%
  pivot_longer(starts_with("Y"), names_to = "age", values_to = "ths") %>%
  group_by(geo, age) %>%
  mutate(share = ths / sum(ths)) %>%
  ungroup() %>%
  mutate(age_band = recode(str_remove(age, "Y"), "65-74" = "65+"),
         educ     = recode(isced11, "ED0-2" = "ISCED 0-2", "ED3_4" = "ISCED 3-4",
                           "ED5-8" = "ISCED 5-8"))

# Standard age bands for this step (Spain's 64-year-olds are counted in 55-64)
pop_by_age <- pop %>%
  mutate(age_band = if_else(age_num >= 65, "65+", age_band)) %>%
  group_by(geo, age_band) %>%
  summarise(pop = sum(OBS_VALUE), .groups = "drop")

target_educ <- educ_by_age %>%
  inner_join(pop_by_age, by = c("geo", "age_band")) %>%
  group_by(geo, category = educ) %>%
  summarise(pop = sum(share * pop), .groups = "drop_last") %>%
  mutate(target = pop / sum(pop)) %>%
  ungroup() %>%
  left_join(countries, by = "geo") %>%
  transmute(Country, variable = "educ", category, target)

# All targets: one row per weight set x country x variable x category
targets <- tibble(weight_set = WEIGHT_SETS) %>%
  cross_join(bind_rows(target_age_gender, target_educ)) %>%
  bind_rows(cross_join(countries %>% select(Country), target_income))

print(sprintf("Targets: Eurostat demo_pjan %s and EDAT_LFS_9901 %s", year_pop, year_educ))

# ---- 4. Raking ---------------------------------------------------------------

#' Iterative proportional fitting with trimming, for one country and weight
#' set. `t` holds the targets (variable, category, target). After each pass
#' over the margins, weights (mean 1) are capped to [lo, hi], as in anesrake.
#' Where a target needs weights beyond the caps, that margin is met as closely
#' as the caps allow (see diagnostics). Returns `d` with a column `w`.
rake_trimmed <- function(d, t, lo = WEIGHT_MIN, hi = WEIGHT_MAX,
                         max_iter = 1000, tol = 1e-8) {
  margins <- unique(t$variable)

  # Every sample category needs a target and vice versa
  for (v in margins) {
    t_v <- t %>% filter(variable == v) %>% select(category, target)
    d_v <- d %>% distinct(category = .data[[v]])
    if (nrow(anti_join(d_v, t_v, by = "category")) > 0 ||
        nrow(anti_join(t_v, d_v, by = "category")) > 0) {
      stop(first(d$Country), ": categories of ", v, " differ between sample and targets.")
    }
    d <- d %>% left_join(rename(t_v, !!v := category, !!paste0("target_", v) := target), by = v)
  }

  d <- mutate(d, w = 1)
  for (i in seq_len(max_iter)) {
    w_old <- d$w
    for (v in margins) {
      d <- d %>%
        mutate(w_total = sum(w)) %>%
        group_by(.data[[v]]) %>%
        mutate(w = w * .data[[paste0("target_", v)]] / (sum(w) / w_total)) %>%
        ungroup()
    }
    d <- mutate(d, w = pmin(pmax(w / mean(w), lo), hi))
    if (max(abs(d$w - w_old)) < tol) break
  }
  if (i == max_iter) warning("Raking did not converge after ", max_iter, " iterations.")

  select(d, -w_total, -starts_with("target_"))
}

data_rake <- data_1 %>%
  filter(!is.na(age_gender), !is.na(educ), !is.na(income_tercile))

weights_long <- expand_grid(Country = countries$Country, weight_set = WEIGHT_SETS) %>%
  pmap_dfr(function(Country, weight_set) {
    co  <- Country
    set <- weight_set
    rake_trimmed(filter(data_rake, Country == co),
                 filter(targets, Country == co, weight_set == set)) %>%
      mutate(weight_set = set)
  })

weights <- data_1 %>%
  select(Country, ID, age_band, gender, educ, income_band, income_tercile, income_dk) %>%
  left_join(weights_long %>%
              select(Country, ID, weight_set, w) %>%
              pivot_wider(names_from = weight_set, values_from = w),
            by = c("Country", "ID"))

# ---- 5. Diagnostics ----------------------------------------------------------

# Sample, target and weighted shares for every margin, including the income
# distribution implied by w_demo (which does not target it)
diagnostics <- weights_long %>%
  pivot_longer(c(age_gender, educ, income_tercile),
               names_to = "variable", values_to = "category") %>%
  group_by(Country, weight_set, variable, category) %>%
  summarise(n = n(), weighted = sum(w), .groups = "drop_last") %>%
  mutate(sample   = n / sum(n),
         weighted = weighted / sum(weighted)) %>%
  ungroup() %>%
  left_join(targets, by = c("Country", "weight_set", "variable", "category")) %>%
  select(Country, weight_set, variable, category, n, sample, target, weighted)

summary_weights <- weights %>%
  pivot_longer(all_of(WEIGHT_SETS), names_to = "weight_set", values_to = "w") %>%
  group_by(Country, weight_set) %>%
  summarise(
    n_sample       = n(),
    n_income_dk    = sum(income_dk),
    n_missing_demo = sum(is.na(age_band) | is.na(gender) | is.na(educ)),
    n_weighted     = sum(!is.na(w)),
    n_effective    = sum(w, na.rm = TRUE)^2 / sum(w^2, na.rm = TRUE),   # Kish
    design_effect  = n_weighted / n_effective,
    w_min          = min(w, na.rm = TRUE),
    w_max          = max(w, na.rm = TRUE),
    .groups        = "drop"
  )

# ---- 6. Output ---------------------------------------------------------------

write_csv(weights, PATH_WEIGHTS)
write_csv(diagnostics, "output/weights/weights_diagnostics.csv")
write_csv(summary_weights, "output/weights/weights_summary.csv")

print("Weights: effective sample size and range")
print(summary_weights %>% mutate(across(where(is.double), ~ round(.x, 2))), width = Inf)

print("Sample vs target vs weighted shares")
print(diagnostics %>%
        mutate(across(c(sample, target, weighted), ~ round(.x, 3))) %>%
        pivot_wider(id_cols = c(Country, variable, category, n, sample),
                    names_from = weight_set, values_from = c(target, weighted)) %>%
        select(Country, variable, category, n, sample,
               target_w_income, weighted_w_income,
               target_w_income_pop, weighted_w_income_pop,
               weighted_w_demo),
      n = Inf, width = Inf)

print(paste("Saved:", PATH_WEIGHTS, "and output/weights/weights_*.csv"))
