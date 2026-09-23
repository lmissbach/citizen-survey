# Helpers for conjoint_prepare.R.
# Qualtrics cleaning, inclusion, country specs, long extract, level lookup.

# ---- Qualtrics cleaning and inclusion ---------------------------------------
QUALTRICS_STATUS_IP <- c("IP-Adresse", "IP Address")
QUALTRICS_FINISHED  <- c("Wahr", "TRUE", "True")

parse_qualtrics_startdate <- function(x) {
  x <- as.character(x)
  parsed <- as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  alt <- is.na(parsed) & nzchar(x)
  if (any(alt)) {
    parsed[alt] <- as.POSIXct(x[alt], format = "%d/%m/%Y %H:%M", tz = "UTC")
  }
  parsed
}

# Q31 answers reference ${e://Field/Label1}…Label5; map to slot "1"–"5" (always character).
q31_value_to_label_slot <- function(value_raw) {
  value_raw <- as.character(value_raw)
  extracted <- stringr::str_extract(value_raw, "Label([1-5])", group = 1)
  dplyr::if_else(
    !is.na(extracted),
    extracted,
    dplyr::case_when(
      stringr::str_detect(value_raw, "1") ~ "1",
      stringr::str_detect(value_raw, "2") ~ "2",
      stringr::str_detect(value_raw, "3") ~ "3",
      stringr::str_detect(value_raw, "4") ~ "4",
      stringr::str_detect(value_raw, "5") ~ "5",
      TRUE ~ value_raw
    )
  )
}

# Standard Qualtrics conjoint column names (C1_1 … C4_1, C1_2A … C4_2C, *_DO order fields).
conjoint_first_choice_cols <- function(data) {
  intersect(names(data), paste0("C", 1:4, "_1"))
}

conjoint_second_choice_cols <- function(data) {
  intersect(
    names(data),
    paste0("C", rep(1:4, each = 3), "_2", rep(c("A", "B", "C"), 4))
  )
}

conjoint_first_order_cols <- function(data) {
  intersect(names(data), paste0("C", 1:4, "_1_DO"))
}

is_profile_display_order <- function(x) {
  grepl("^[ABC]\\|[ABC]\\|[ABC]$", x)
}

# Map Qualtrics step-1/step-2 label text (or "A"/"B"/"C") to profile letter.
# Covers FR/ES/DE "Scenario/Szenario/Escenario …" and RO "Scenariul …".
conjoint_profile_from_label <- function(x) {
  x <- as.character(x)
  dplyr::case_when(
    x %in% c("A", "B", "C") ~ x,
    grepl("(enario|enariul|escenario|szenario)\\s*A\\b", x, ignore.case = TRUE) ~ "A",
    grepl("(enario|enariul|escenario|szenario)\\s*B\\b", x, ignore.case = TRUE) ~ "B",
    grepl("(enario|enariul|escenario|szenario)\\s*C\\b", x, ignore.case = TRUE) ~ "C",
    TRUE ~ NA_character_
  )
}

# Position (1–3) of profile A/B/C in the step-1 multiple-choice list (C*_1_DO).
# That list is the rank-option order (A|B|C or C|B|A), not left-to-right in the
# conjoint table. The table shows A and B only; C (repeal) is described in the
# question text and is not displayed as a column.
profile_screen_position <- function(order_str, profile) {
  order_str <- as.character(order_str)
  profile <- as.character(profile)
  out <- rep(NA_integer_, length(order_str))
  ok <- is_profile_display_order(order_str) & !is.na(profile)
  if (!any(ok)) {
    return(out)
  }
  for (i in which(ok)) {
    slots <- strsplit(order_str[i], "|", fixed = TRUE)[[1]]
    out[i] <- match(profile[i], slots)
  }
  out
}

# Analysis-sample inclusion, after quality filters (Status, date, consent,
# Finished, Q38A, Q60A) and after filter_* flags are built.
# Drops fastest 5% / slowest 2% (filter_1a / 1b); empty attitude blocks
# (both waves NA: keep if filter_2a == 0 | filter_2b == 0); no conjoint
# first choices (filter_3a). filter_2c (all Q62–Q68 missing) is recorded
# but not applied.
apply_analysis_inclusion <- function(data) {
  data %>%
    dplyr::filter(.data$filter_1a == 0, .data$filter_1b == 0) %>%
    dplyr::filter(.data$filter_2a == 0 | .data$filter_2b == 0) %>%
    dplyr::filter(.data$filter_3a == 0) %>%
    dplyr::mutate(Inclusion = 1L)
}

# Sequential attrition on one country’s wide file (before inclusion is applied).
inclusion_attrition <- function(data, country) {
  n0 <- nrow(data)
  d1 <- dplyr::filter(data, .data$filter_1a == 0, .data$filter_1b == 0)
  d2 <- dplyr::filter(d1, .data$filter_2a == 0 | .data$filter_2b == 0)
  d3 <- dplyr::filter(d2, .data$filter_3a == 0)
  tibble::tibble(
    Country            = country,
    n_quality_sample   = n0,
    n_after_speed      = nrow(d1),
    n_after_item_nr    = nrow(d2),
    n_included         = nrow(d3)
  )
}

# ---- Country specs, extract, level lookup -----------------------------------
CONJOINT_ATTR_COLS <- c(
  "budget_and_funding",
  "budget_control",
  "household_support",
  "information",
  "infrastructure_ownership",
  "worker_support",
  "community_mobility_support"
)

COUNTRY_LEVELS_4 <- c("Spain", "France", "Germany", "Romania")

Q46_2_LEVELS <- c(
  "Strongly oppose", "Rather oppose", "Neither",
  "Rather support", "Strongly support"
)
Q61A_LEVELS <- c("Yes", "Don't know", "No")
Q61A_LABELS <- c(
  Yes = "Yes (abolish)",
  `Don't know` = "Don't know",
  No = "No (keep)"
)

CONJOINT_QUESTION_TO_ATTR <- c(
  Q62 = "budget_and_funding",
  Q63 = "budget_control",
  Q64 = "household_support",
  Q65 = "information",
  Q66 = "worker_support",
  Q67 = "infrastructure_ownership",
  Q68 = "community_mobility_support"
)

ACP_LONG_COLS <- c(
  "Country", "ID", "ResponseId", "Task", "Profile",
  CONJOINT_ATTR_COLS,
  "Preferred", "Preferred_Second", "Preferred_Least",
  "Choice", "Choice_2", "Choice_3",
  "display_order_step1", "screen_position_step1",
  "d1", "d2",
  "Q61A_response", "Q46_2_cat", "Q46_2_opposer",
  # SI subgroup covariates (respondent-level, repeated on each row)
  "sector_en", "sector_tier", "is_worker", "high_c_narrow", "high_c_broad",
  "transport_level", "transport_poor",
  "expected_cost_amt", "income_annual", "cost_share", "cost_share_above_avg",
  paste0("Q", 62:68)
)

Q62_Q68_TITLES <- c(
  Q62 = "How should the transition budget be financed?",
  Q63 = "How should the transition budget be managed?",
  Q64 = "What level of financial support for households is appropriate?",
  Q65 = "How should people access transition information?",
  Q66 = "What support should be offered to workers at risk?",
  Q67 = "Who should mainly benefit from clean energy subsidies?",
  Q68 = "What approach to low-carbon mobility is most appropriate?"
)

# English source wording of the direct-question options (plus Don't know,
# which was fielded on Q62–Q68 but is not a conjoint level).
Q62_Q68_ANSWERS_EN <- list(
  Q62 = c(
    "The revenue of the carbon pricing policy, but no additional budget",
    "A larger budget (to support people more widely) paid by borrowing",
    "A larger budget (to support people more widely) paid by a tax on wealth of the richest 10%",
    "Don't know"
  ),
  Q63 = c(
    "The government, as with any other public revenue",
    "A protected fund that guarantees the government spends on the transition over the next two decades",
    "A protected fund that guarantees the government spends on the transition over the next two decades and with citizens on the board to decide how the money is used",
    "Don't know"
  ),
  Q64 = c(
    "15% of the extra costs",
    "50% of the extra costs",
    "90% of the extra costs",
    "Don't know"
  ),
  Q65 = c(
    "From government websites",
    "A local climate center, where an advisor can guide you",
    "Don't know"
  ),
  Q66 = c(
    "Existing social welfare (unemployment insurance and training and employment services)",
    "Fully funded training course for up to 1 year",
    "Fully funded training course for up to 1 year and a salary guarantee for 3 years",
    "Don't know"
  ),
  Q67 = c(
    "Any company willing to invest",
    "Preferentially energy project co-owned by local residents",
    "Government-owned firms reinvesting profits in the transition",
    "Don't know"
  ),
  Q68 = c(
    "Maintain quality of existing public transport",
    "Increased investment in clean, frequent, affordable public transport in every community",
    "Increased investment in clean, frequent, affordable public transport in every community and fast intercity rail",
    "Don't know"
  )
)
Q62_Q68_SPLIT_ARM <- c(
  Q62 = NA_character_,
  Q63 = NA_character_,
  Q64 = NA_character_,
  Q65 = "information",
  Q66 = "worker_support",
  Q67 = "infrastructure_ownership",
  Q68 = "community_mobility_support"
)

Q62_Q68_SPLIT_NOTE <- c(
  Q65 = "Asked only of the $d_2$ = information arm (expected $N$ about half the sample).",
  Q66 = "Asked only of the $d_1$ = worker-support arm (expected $N$ about half the sample).",
  Q67 = "Asked only of the $d_2$ = infrastructure-ownership arm (expected $N$ about half the sample).",
  Q68 = "Asked only of the $d_1$ = community-mobility arm (expected $N$ about half the sample)."
)

# QT7 times the Q62–Q68 page; QT8–QT11 time conjoint tasks 1–4 (C1–C4).
CONJOINT_QT_TIMERS <- tibble::tibble(
  timer    = paste0("QT", 7:11),
  page_lab = c(
    "Direct questions (Q62-Q68)",
    "Conjoint task 1",
    "Conjoint task 2",
    "Conjoint task 3",
    "Conjoint task 4"
  )
)

PREP_COUNTRY_COLOURS <- c(
  Spain   = "#E64B35FF",
  France  = "#4DBBD5FF",
  Germany = "#00A087FF",
  Romania = "#3C5488FF"
)

COUNTRY_SURVEY <- list(
  Spain = list(
    file         = "../2_Data/0_Qualtrics_Output/20260309_Final/Spanish/Spanish_5.+Mai+2026_15.02.csv",
    consent_no   = "No",
    q38a         = "De acuerdo",
    q60a         = "Energía",
    q10_exclude  = NULL
  ),
  France = list(
    file         = "../2_Data/0_Qualtrics_Output/20260309_Final/French/French_9.+März+2026_09.31.csv",
    consent_no   = "Non",
    q38a         = "D’accord",
    q60a         = "Énergie",
    q10_exclude  = NULL
  ),
  Germany = list(
    file         = "../2_Data/0_Qualtrics_Output/20260309_Final/German/German_9.+März+2026_09.31.csv",
    consent_no   = "Nein",
    q38a         = "Stimme zu",
    q60a         = "Energie",
    q10_exclude  = "Unter 18"
  ),
  Romania = list(
    file         = "../2_Data/0_Qualtrics_Output/20260309_Final/Romanian/Romanian_5.+Mai+2026_15.02.csv",
    consent_no   = "Nu",
    q38a         = "De acord",
    q60a         = "Energie",
    q10_exclude  = "Sub 18"
  )
)

# Factor levels must match the Qualtrics JSON strings (Repeal last).
CONJOINT_FACTOR_LEVELS <- list(
  Spain = list(
    budget_and_funding = c(
      "Los ingresos de la política de tarificación del carbono, pero sin presupuesto adicional",
      "Un presupuesto mayor (para apoyar a las personas de una forma más amplia) financiado mediante endeudamiento",
      "Un presupuesto mayor (para apoyar a las personas de una forma más amplia) financiado por un impuesto sobre la riqueza al 10% de la población más rica",
      "Repeal"
    ),
    budget_control = c(
      "El gobierno, como ocurre con cualquier otro ingreso público",
      "Un fondo protegido que garantice que el gobierno invierta en la transición durante las próximas dos décadas",
      "Un fondo protegido que garantice que el gobierno invierta en la transición durante las próximas dos décadas, con ciudadanos en el consejo para decidir cómo se utiliza el dinero",
      "Repeal"
    ),
    household_support = c(
      "El 15% de los costes adicionales",
      "El 50% de los costes adicionales",
      "El 90% de los costes adicionales",
      "Repeal"
    ),
    information = c(
      "A través de las páginas web gubernamentales",
      "A través de un centro climático local, donde un asesor pueda orientarle",
      "Repeal"
    ),
    infrastructure_ownership = c(
      "Cualquier empresa dispuesta a invertir",
      "Preferentemente proyectos energéticos en los que participen residentes locales como copropietarios",
      "Empresas públicas que reinvierten sus beneficios en la transición",
      "Repeal"
    ),
    worker_support = c(
      "La asistencia social existente (seguro de desempleo y servicios de formación y empleo)",
      "Curso de formación totalmente financiado durante un máximo de 1 año",
      "Curso de formación totalmente financiado durante un máximo de 1 año y un salario garantizado durante 3 años",
      "Repeal"
    ),
    community_mobility_support = c(
      "Mantener la calidad del transporte público existente",
      "Mayor inversión en transporte público limpio, frecuente y asequible en todas las comunidades",
      "Mayor inversión en transporte público limpio, frecuente y asequible en todas las comunidades, así como en trenes rápidos interurbanos",
      "Repeal"
    )
  ),
  France = list(
    budget_and_funding = c(
      "Les recettes de la politique de tarification du carbone, mais sans budget supplémentaire",
      "Un budget plus important (pour aider un plus grand nombre de personnes) financé par l’emprunt",
      "Un budget plus important (pour aider un plus grand nombre de personnes) financé par un impôt sur la fortune des 10 % les plus riches",
      "Repeal"
    ),
    budget_control = c(
      "Le gouvernement, comme pour toute autre recette publique",
      "Un fonds protégé qui garantit que le gouvernement consacrera les dépenses à la transition au cours des deux prochaines décennies",
      "Un fonds protégé qui garantit que le gouvernement consacrera les dépenses à la transition au cours des deux prochaines décennies et dont la gestion sera supervisée par un conseil composé de citoyens",
      "Repeal"
    ),
    household_support = c(
      "15 % des coûts supplémentaires",
      "50 % des coûts supplémentaires",
      "90 % des coûts supplémentaires",
      "Repeal"
    ),
    information = c(
      "Sur les sites web du gouvernement",
      "Dans un centre d'information local, où un conseiller peut vous guider",
      "Repeal"
    ),
    infrastructure_ownership = c(
      "Toute entreprise disposée à investir",
      "Des projets à propriété partagée avec les habitants du territoire",
      "Les entreprises publiques réinvestissant leurs bénéfices dans la transition",
      "Repeal"
    ),
    worker_support = c(
      "Aide sociale existante (assurance chômage et services de formation et d’emploi)",
      "Formation entièrement financée pendant un an maximum",
      "Formation entièrement financée pendant un an maximum et salaire garanti pendant trois ans",
      "Repeal"
    ),
    community_mobility_support = c(
      "Maintien de la qualité des transports publics existants",
      "Augmentation des investissements dans des transports publics propres, fréquents et abordables dans chaque communauté",
      "Augmentation des investissements dans des transports publics propres, fréquents et abordables dans chaque communauté et dans des trains interurbains rapides",
      "Repeal"
    )
  ),
  Germany = list(
    budget_and_funding = c(
      "Die Einnahmen aus der CO2-Bepreisung, aber kein zusätzliches Budget",
      "Ein größeres Budget (um mehr Menschen zu unterstützen), das durch Kreditaufnahme finanziert wird",
      "Ein größeres Budget (um mehr Menschen zu unterstützen), das durch eine  Steuer auf das Vermögen der reichsten 10 % bezahlt wird",
      "Repeal"
    ),
    budget_control = c(
      "Die Regierung, wie bei allen anderen staatlichen Einnahmen auch",
      "Ein Fond, der garantiert, dass die Regierung die Mittel in den nächsten zwei Jahrzehnten für die Transformation ausgibt",
      "Ein Fond, der garantiert, dass die Regierung die Mittel in den nächsten zwei Jahrzehnten Geld für die Transformation ausgibt, wobei die Bürgerinnen und Bürger über die Verwendung des Geldes mitentscheiden.",
      "Repeal"
    ),
    household_support = c("15 % der Mehrkosten", "50 % der Mehrkosten", "90 % der Mehrkosten", "Repeal"),
    information = c(
      "Von Websites der Regierung",
      "Von einem lokalen Informationzentrum, in dem Sie sich beraten lassen können",
      "Repeal"
    ),
    infrastructure_ownership = c(
      "Jedes Unternehmen, das bereit ist zu investieren",
      "Vorzugsweise Energieprojekte im Miteigentum der ortsansässigen Bevölkerung",
      "Staatseigene Unternehmen, die ihre Gewinne in der Transformation reinvestieren",
      "Repeal"
    ),
    worker_support = c(
      "Bestehende Sozialleistungen (Arbeitslosenversicherung, Schulungen und Arbeitsvermittlung)",
      "Vollständig finanzierte Schulung für bis zu einem Jahr",
      "Vollständig finanzierte Schulung für bis zu einem Jahr und eine Lohngarantie für 3 Jahre",
      "Repeal"
    ),
    community_mobility_support = c(
      "Beibehaltung der Qualität der bestehenden öffentlichen Verkehrsmittel",
      "Verstärkte Investitionen in einen nachhaltigen, häufig verkehrenden und erschwinglichen ÖPNV in  jeder Gemeinde",
      "Verstärkte Investitionen in einen nachhaltigen, häufig verkehrenden und erschwinglichen ÖPNV in  jeder Gemeinde und in schnelle Intercity-Zugverbindungen",
      "Repeal"
    )
  ),
  Romania = list(
    budget_and_funding = c(
      "Veniturile generate de politica de stabilire a pretului carbonului, fara niciun buget suplimentar",
      "Un buget mai mare (pentru a sprijini mai multe persoane), finantat prin imprumuturi",
      "Un buget mai mare (pentru a sprijini mai multe persoane), finantat printr-un impozit pe averea celor mai bogati 10% din populatie",
      "Repeal"
    ),
    budget_control = c(
      "Guvernul, ca pe orice alte venituri publice",
      "Un fond protejat, care garanteaza ca guvernul va cheltui banii pentru tranzitie in urmatoarele doua decenii",
      "Un fond protejat, care garanteaza ca guvernul va cheltui banii pentru tranzitie in urmatoarele doua decenii, si care va avea in consiliul de conducere cetateni care vor decide modul in care vor fi utilizati acesti bani",
      "Repeal"
    ),
    household_support = c(
      "15% din costurile suplimentare",
      "50% din costurile suplimentare",
      "90% din costurile suplimentare",
      "Repeal"
    ),
    information = c(
      "De pe site-urile guvernamentale",
      "Un centru local de informare, unde un consilier va poate indruma",
      "Repeal"
    ),
    infrastructure_ownership = c(
      "Orice companie dispusa sa investeasca",
      "Proiect energetic preferential, detinut in comun de catre locuitorii din zona",
      "Companiile de stat care reinvestesc profiturile in tranzitie",
      "Repeal"
    ),
    worker_support = c(
      "Asistenta sociala existenta (asigurarea de somaj si servicii de formare profesionala si ocupare a fortei de munca)",
      "Curs de formare profesionala finantat integral, cu durata de pana la 1 an",
      "Curs de formare profesionala finantat integral, cu durata de pana la 1 an, si salariu garantat pe o perioada de 3 ani",
      "Repeal"
    ),
    community_mobility_support = c(
      "Mentinerea calitatii transportului public existent",
      "Cresterea investitiilor in transportul public curat, frecvent si accesibil in fiecare comunitate",
      "Cresterea investitiilor in transportul public curat, frecvent si accesibil in fiecare comunitate si in transportul feroviar interurban rapid",
      "Repeal"
    )
  )
)

resolve_survey_path <- function(path) {
  if (file.exists(path)) {
    return(path)
  }
  token <- sub("_.*", "", basename(path))
  hits <- list.files(dirname(path), pattern = paste0("^", token, "_"), full.names = TRUE)
  csv <- hits[grepl("\\.csv$", hits) & !grepl("open_ended", hits)]
  if (length(csv) == 1L) {
    return(csv)
  }
  stop("Survey file not found: ", path, call. = FALSE)
}

add_quality_filter_flags <- function(data) {
  data %>%
    dplyr::mutate(
      time = as.numeric(.data$time),
      filter_1a = as.integer(.data$time <= stats::quantile(.data$time, probs = 0.05, na.rm = TRUE)),
      filter_1b = as.integer(.data$time >= stats::quantile(.data$time, probs = 0.98, na.rm = TRUE)),
      filter_2a = as.integer(
        is.na(.data$Q41_1) & is.na(.data$Q42_1) & is.na(.data$Q43_1) &
          is.na(.data$Q44_1) & is.na(.data$Q45_1) & is.na(.data$Q46_1)
      ),
      filter_2b = as.integer(
        is.na(.data$Q41_2) & is.na(.data$Q42_2) & is.na(.data$Q43_2) &
          is.na(.data$Q44_2) & is.na(.data$Q45_2) & is.na(.data$Q46_2)
      ),
      filter_2c = as.integer(
        is.na(.data$Q62) & is.na(.data$Q63) & is.na(.data$Q64) & is.na(.data$Q65) &
          is.na(.data$Q66) & is.na(.data$Q67) & is.na(.data$Q68)
      ),
      filter_3a = as.integer(
        is.na(.data$C1_1) & is.na(.data$C2_1) & is.na(.data$C3_1) & is.na(.data$C4_1)
      )
    )
}

# ID after the date filter, before consent / attention / inclusion.
prep_country_wide <- function(raw, country, spec = COUNTRY_SURVEY[[country]]) {
  data_1 <- raw %>%
    dplyr::filter(.data$Status %in% QUALTRICS_STATUS_IP) %>%
    dplyr::mutate(
      StartDate = parse_qualtrics_startdate(.data$StartDate),
      Date      = format(.data$StartDate, "%Y-%m-%d")
    ) %>%
    dplyr::filter(as.Date(.data$Date) > as.Date("2025-12-17")) %>%
    dplyr::mutate(
      Country = country,
      ID      = dplyr::row_number()
    ) %>%
    dplyr::select("Country", "ID", dplyr::everything()) %>%
    dplyr::rename(time = "Duration (in seconds)") %>%
    dplyr::filter(.data$Q02 != spec$consent_no)

  if (!is.null(spec$q10_exclude)) {
    data_1 <- dplyr::filter(data_1, .data$Q10 != spec$q10_exclude)
  }

  if (!"ResponseId" %in% names(data_1)) {
    stop("Qualtrics export missing ResponseId for ", country, call. = FALSE)
  }

  data_1 %>%
    dplyr::filter(.data$Finished %in% QUALTRICS_FINISHED) %>%
    dplyr::filter(.data$Q38A == spec$q38a) %>%
    dplyr::filter(.data$Q60A == spec$q60a) %>%
    dplyr::select("Country", "ID", "ResponseId", "time", Q10:Date) %>%
    add_quality_filter_flags()
}

# JSON profiles → long A/B/C rows with rank dummies.
extract_conjoint <- function(data_1.4.0) {
  first_choice_cols  <- conjoint_first_choice_cols(data_1.4.0)
  second_choice_cols <- conjoint_second_choice_cols(data_1.4.0)
  first_order_cols   <- conjoint_first_order_cols(data_1.4.0)
  profile_cols       <- grep("^profile", names(data_1.4.0), value = TRUE)

  if (length(first_choice_cols) == 0) {
    stop(
      "No conjoint first-choice columns (C1_1 … C4_1) in input data.\n",
      "Pass the wide inclusion sample from prep_country_wide() / apply_analysis_inclusion().",
      call. = FALSE
    )
  }
  if (length(profile_cols) == 0) {
    stop(
      "No profile* JSON columns in input data.\n",
      "Pass the wide inclusion sample from prep_country_wide() / apply_analysis_inclusion().",
      call. = FALSE
    )
  }

  data_1.5.1 <- data_1.4.0 %>%
    dplyr::select("ID", dplyr::starts_with("profile")) %>%
    tidyr::drop_na() %>%
    tidyr::pivot_longer(dplyr::starts_with("profile"), names_to = "names", values_to = "values") %>%
    dplyr::mutate(
      Profile = dplyr::case_when(
        grepl("profileA", .data$names) ~ "A",
        grepl("profileB", .data$names) ~ "B"
      ),
      Task = dplyr::case_when(
        grepl("task0", .data$names) ~ "0",
        grepl("task1", .data$names) ~ "1",
        grepl("task2", .data$names) ~ "2",
        grepl("task3", .data$names) ~ "3"
      )
    ) %>%
    dplyr::select(-"names") %>%
    dplyr::filter(!is.na(.data$values)) %>%
    dplyr::mutate(parsed = purrr::map(.data$values, function(x) {
      x <- gsub('^"|"$', "", x)
      parts <- strsplit(x, "\\}\\s*\\{")[[1]]
      parts <- paste0("{", gsub("(^\\{|\\}$)", "", parts), "}")
      jsons <- purrr::map(parts, purrr::safely(jsonlite::fromJSON))
      dplyr::bind_rows(purrr::map(jsons, "result"))
    })) %>%
    tidyr::unnest("parsed") %>%
    dplyr::mutate(
      dplyr::across(-c(ID, Task), ~ ifelse(is.na(.x), "Missing", .x))
    ) %>%
    dplyr::arrange(.data$ID, .data$Task, .data$Profile) %>%
    dplyr::select(-"values") %>%
    dplyr::select("ID", "Task", "Profile", dplyr::everything())

  data_1.5.1_b <- tidyr::expand_grid(
    ID    = unique(data_1.5.1$ID),
    Task  = c("0", "1", "2", "3")
  ) %>%
    dplyr::mutate(Profile = "C")

  data_1.5.1 <- data_1.5.1 %>%
    dplyr::bind_rows(data_1.5.1_b) %>%
    dplyr::arrange(.data$ID, .data$Task, .data$Profile) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(is.na(.x), "Repeal", .x)))

  data_1.5.2 <- data_1.4.0 %>%
    dplyr::select("ID", dplyr::all_of(first_choice_cols)) %>%
    tidyr::pivot_longer(-"ID", names_to = "names", values_to = "Choice") %>%
    dplyr::mutate(
      Task = dplyr::case_when(
        grepl("C1_1", .data$names) ~ "0",
        grepl("C2_1", .data$names) ~ "1",
        grepl("C3_1", .data$names) ~ "2",
        grepl("C4_1", .data$names) ~ "3"
      )
    ) %>%
    dplyr::select("ID", "Task", "Choice")

  data_1.5.3 <- data_1.4.0 %>%
    dplyr::select("ID", Q62:Q68, "filter_3a")

  data_1.5.4 <- if (length(second_choice_cols) == 0) {
    tibble::tibble(
      ID = integer(), Task = character(),
      Choice_2 = character(), Choice_3 = character()
    )
  } else {
    data_1.4.0 %>%
      dplyr::select("ID", dplyr::all_of(second_choice_cols)) %>%
      tidyr::pivot_longer(-"ID", names_to = "names", values_to = "values") %>%
      dplyr::mutate(
        Task = dplyr::case_when(
          grepl("C1", .data$names) ~ "0",
          grepl("C2", .data$names) ~ "1",
          grepl("C3", .data$names) ~ "2",
          grepl("C4", .data$names) ~ "3"
        ),
        profile_label = conjoint_profile_from_label(.data$values),
        Choice_2 = .data$profile_label,
        Choice_3 = dplyr::case_when(
          .data$profile_label == "A" & grepl("_2B", .data$names) ~ "C",
          .data$profile_label == "B" & grepl("_2C", .data$names) ~ "A",
          .data$profile_label == "C" & grepl("_2A", .data$names) ~ "B",
          .data$profile_label == "A" & grepl("_2C", .data$names) ~ "B",
          .data$profile_label == "B" & grepl("_2A", .data$names) ~ "C",
          .data$profile_label == "C" & grepl("_2B", .data$names) ~ "A"
        )
      ) %>%
      dplyr::filter(!is.na(.data$Choice_2)) %>%
      dplyr::select("ID", "Task", "Choice_2", "Choice_3")
  }

  data_1.5.6 <- if (length(first_order_cols) == 0) {
    tibble::tibble(ID = integer(), Task = character(), display_order_step1 = character())
  } else {
    data_1.4.0 %>%
      dplyr::select("ID", dplyr::all_of(first_order_cols)) %>%
      tidyr::pivot_longer(-"ID", names_to = "names", values_to = "display_order_step1") %>%
      dplyr::mutate(
        Task = dplyr::case_when(
          grepl("C1_1", .data$names) ~ "0",
          grepl("C2_1", .data$names) ~ "1",
          grepl("C3_1", .data$names) ~ "2",
          grepl("C4_1", .data$names) ~ "3"
        ),
        display_order_step1 = as.character(.data$display_order_step1)
      ) %>%
      dplyr::filter(is_profile_display_order(.data$display_order_step1)) %>%
      dplyr::select("ID", "Task", "display_order_step1")
  }

  data_1.5.2 %>%
    dplyr::left_join(data_1.5.1, by = c("ID", "Task")) %>%
    dplyr::left_join(data_1.5.4, by = c("ID", "Task")) %>%
    dplyr::left_join(data_1.5.6, by = c("ID", "Task")) %>%
    dplyr::filter(!is.na(.data$Profile)) %>%
    dplyr::mutate(
      display_order_step1   = factor(.data$display_order_step1),
      screen_position_step1 = profile_screen_position(.data$display_order_step1, .data$Profile)
    ) %>%
    dplyr::select(
      "ID", "Task", "Profile",
      dplyr::all_of(CONJOINT_ATTR_COLS),
      dplyr::everything()
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(CONJOINT_ATTR_COLS),
        ~ ifelse(.x == "Missing", NA, .x)
      ),
      Preferred        = ifelse(.data$Profile == .data$Choice, 1, 0),
      Preferred_Second = ifelse(.data$Profile == .data$Choice_2, 1, 0),
      Preferred_Least  = ifelse(.data$Profile == .data$Choice_3, 1, 0)
    ) %>%
    dplyr::left_join(data_1.5.3, by = "ID") %>%
    dplyr::filter(.data$filter_3a == 0)
}

apply_conjoint_attribute_factors <- function(data, country) {
  attrs <- CONJOINT_ATTR_COLS
  if (country == "France") {
    data <- dplyr::mutate(
      data,
      household_support = stringr::str_replace_all(.data$household_support, "\u00a0", " ")
    )
  }
  if (country == "Germany") {
    data <- data %>%
      dplyr::mutate(
        budget_control = stringr::str_replace(
          .data$budget_control,
          stringr::fixed("Ein geschützter Fonds, der garantiert"),
          "Ein Fond, der garantiert"
        ),
        infrastructure_ownership = stringr::str_replace(
          .data$infrastructure_ownership,
          stringr::fixed("in den Übergang"),
          "in der Transformation"
        )
      ) %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(attrs), stringr::str_trim)
      ) %>%
      dplyr::mutate(
        dplyr::across(dplyr::all_of(attrs), ~ stringr::str_replace_all(.x, "\u2082", "2"))
      )
  } else {
    data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(attrs), stringr::str_trim))
  }
  if (country == "Romania") {
    data <- dplyr::mutate(
      data,
      dplyr::across(dplyr::all_of(attrs), ~ stringi::stri_trans_general(.x, "Latin-ASCII"))
    )
  }

  levs <- CONJOINT_FACTOR_LEVELS[[country]]
  for (attr in attrs) {
    data[[attr]] <- factor(data[[attr]], levels = levs[[attr]])
  }
  data
}

harmonise_q61a <- function(data_wide) {
  data_wide %>%
    dplyr::distinct(.data$Country, .data$ID, .data$Q61A) %>%
    dplyr::mutate(
      Q61A_response = dplyr::case_when(
        .data$Q61A %in% c("Sí", "Oui", "Ja", "Da") ~ "Yes",
        .data$Q61A %in% c("No", "Non", "Nein", "Nu") ~ "No",
        .data$Q61A %in% c("No lo sé", "Je ne sais pas", "Weiß nicht", "Nu știu") ~ "Don't know",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select("Country", "ID", "Q61A_response")
}

harmonise_q46_2 <- function(data_wide) {
  data_wide %>%
    dplyr::distinct(.data$Country, .data$ID, .data$Q46_2) %>%
    dplyr::mutate(
      .q46 = stringi::stri_trans_general(as.character(.data$Q46_2), "Latin-ASCII"),
      Q46_2_cat = dplyr::case_when(
        .q46 %in% c(
          "Me opongo firmemente", "Je suis tout a fait contre",
          "Ich lehne sie entschieden ab", "Ma opun cu tarie"
        ) ~ "Strongly oppose",
        .q46 %in% c(
          "Me opongo en parte", "Je suis plutot contre",
          "Ich bin eher dagegen", "Ma opun oarecum"
        ) ~ "Rather oppose",
        .q46 %in% c(
          "Ni la apoyo ni me opongo", "Je ne suis ni pour ni contre",
          "Ich bin weder dafur noch dagegen",
          "Nici nu o sustin, nici nu ma opun"
        ) ~ "Neither",
        .q46 %in% c(
          "La apoyo en parte", "Je suis plutot pour",
          "Ich befurworte sie in gewissem Masse", "O sustin oarecum"
        ) ~ "Rather support",
        .q46 %in% c(
          "La apoyo firmemente", "Je suis tout a fait pour",
          "Ich befurworte sie entschieden", "O sustin cu tarie"
        ) ~ "Strongly support",
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::select("Country", "ID", "Q46_2_cat")
}

build_level_short_harmonised <- function(label_map) {
  label_map %>%
    dplyr::filter(.data$level_order <= 3) %>%
    dplyr::mutate(
      attribute = unname(CONJOINT_QUESTION_TO_ATTR[.data$question]),
      level_short = .data$response_label %>%
        stringr::str_replace_all(stringr::fixed("\\n"), " ") %>%
        stringr::str_replace_all("[\r\n]+", " ") %>%
        stringr::str_squish()
    ) %>%
    dplyr::distinct(.data$attribute, .data$level_order, .data$level_short)
}

q62_q68_asked <- function(question, d1, d2) {
  arm <- unname(Q62_Q68_SPLIT_ARM[as.character(question)])
  is.na(arm) | (!is.na(d1) & d1 == arm) | (!is.na(d2) & d2 == arm)
}

latex_escape_prep <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("$", "\\$", x, fixed = TRUE)
  x <- gsub("#", "\\#", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x
}

tidy_q62_q68 <- function(data_wide, label_map) {
  data_wide %>%
    dplyr::select("Country", "ID", "d1", "d2", Q62:Q68) %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(
      Q62:Q68, names_to = "question", values_to = "response"
    ) %>%
    dplyr::left_join(
      label_map, by = c("Country", "question", "response")
    ) %>%
    dplyr::mutate(
      attribute = unname(CONJOINT_QUESTION_TO_ATTR[.data$question]),
      question_title = unname(Q62_Q68_TITLES[.data$question]),
      asked = q62_q68_asked(.data$question, .data$d1, .data$d2),
      response_label = dplyr::case_when(
        is.na(.data$response) ~ "No answer",
        is.na(.data$response_label) ~ "Unmapped",
        TRUE ~ stringr::str_replace_all(
          as.character(.data$response_label),
          stringr::fixed("\\n"),
          " "
        )
      ),
      level_order = dplyr::case_when(
        .data$response_label == "No answer" ~ 99,
        .data$response_label == "Unmapped" ~ 98,
        TRUE ~ as.numeric(.data$level_order)
      ),
      question = factor(.data$question, levels = names(Q62_Q68_TITLES)),
      Country = factor(.data$Country, levels = COUNTRY_LEVELS_4)
    )
}

summarise_q62_q68 <- function(q_long) {
  assigned <- q_long %>% dplyr::filter(.data$asked)
  answered <- assigned %>%
    dplyr::filter(.data$response_label != "No answer")
  add_shares <- function(d) {
    d %>%
      dplyr::group_by(
        .data$Country, .data$question, .data$question_title, .data$attribute,
        .data$response_label, .data$level_order
      ) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::group_by(.data$Country, .data$question) %>%
      dplyr::mutate(share = .data$n / sum(.data$n)) %>%
      dplyr::ungroup()
  }
  n_info <- function(d) {
    d %>%
      dplyr::group_by(
        .data$Country, .data$question, .data$question_title, .data$attribute
      ) %>%
      dplyr::summarise(
        n_expected = dplyr::n(),
        n_missing  = sum(.data$response_label == "No answer"),
        .groups    = "drop"
      )
  }
  pooled_assigned <- assigned %>%
    dplyr::mutate(
      Country = factor("Pooled", levels = c("Pooled", COUNTRY_LEVELS_4))
    )
  pooled_answered <- answered %>%
    dplyr::mutate(
      Country = factor("Pooled", levels = c("Pooled", COUNTRY_LEVELS_4))
    )
  dplyr::bind_rows(add_shares(pooled_answered), add_shares(answered)) %>%
    dplyr::left_join(
      dplyr::bind_rows(n_info(pooled_assigned), n_info(assigned)),
      by = c("Country", "question", "question_title", "attribute")
    ) %>%
    dplyr::arrange(.data$question, .data$Country, .data$level_order)
}

print_q62_q68_shares <- function(shares) {
  qs <- levels(shares$question)
  if (is.null(qs)) qs <- unique(as.character(shares$question))
  for (q in qs) {
    d <- dplyr::filter(shares, as.character(.data$question) == .env$q)
    title <- unique(d$question_title)[1]
    wide <- d %>%
      dplyr::mutate(pct = sprintf("%.1f", 100 * .data$share)) %>%
      dplyr::select("Country", "response_label", "pct") %>%
      tidyr::pivot_wider(names_from = "Country", values_from = "pct")
    cat("\n--- ", q, ": ", title, " ---\n", sep = "")
    print(wide, n = Inf)
    miss <- d %>%
      dplyr::distinct(.data$Country, .data$n_expected) %>%
      dplyr::arrange(.data$Country)
    cat(
      "N asked: ",
      paste(
        sprintf("%s %d", miss$Country, miss$n_expected),
        collapse = "; "
      ),
      "\n",
      sep = ""
    )
  }
  invisible(shares)
}

format_q62_q68_tabular <- function(d) {
  countries <- c("Pooled", COUNTRY_LEVELS_4)
  body <- d %>%
    dplyr::mutate(
      Country = factor(.data$Country, levels = countries)
    ) %>%
    dplyr::select(
      "Country", "response_label", "level_order", "share"
    ) %>%
    tidyr::pivot_wider(names_from = "Country", values_from = "share") %>%
    dplyr::arrange(.data$level_order)
  pct <- function(x) {
    ifelse(is.na(x), "0.0", sprintf("%.1f", 100 * x))
  }
  share_rows <- paste0(
    "    ",
    latex_escape_prep(body$response_label),
    " & ",
    pct(body$Pooled), " & ",
    pct(body$Spain), " & ",
    pct(body$France), " & ",
    pct(body$Germany), " & ",
    pct(body$Romania),
    " \\\\"
  )
  n_tab <- d %>%
    dplyr::mutate(Country = factor(.data$Country, levels = countries)) %>%
    dplyr::distinct(.data$Country, .data$n_expected) %>%
    dplyr::arrange(.data$Country)
  n_exp <- stats::setNames(n_tab$n_expected, as.character(n_tab$Country))
  n_vec <- function(v) {
    as.integer(unname(v[countries]))
  }
  n_rows <- c(
    "    \\midrule",
    sprintf(
      "    $N$ asked & %s \\\\",
      paste(n_vec(n_exp), collapse = " & ")
    )
  )
  c(
    "\\begin{tabular}{lrrrrr}",
    "  \\toprule",
    "  Response & Pooled & Spain & France & Germany & Romania \\\\",
    "  \\midrule",
    share_rows,
    n_rows,
    "  \\bottomrule",
    "\\end{tabular}"
  )
}

format_q62_q68_answers_caption <- function(q) {
  ans <- Q62_Q68_ANSWERS_EN[[q]]
  if (is.null(ans)) {
    return("")
  }
  numbered <- paste0("(", seq_along(ans), ") ", latex_escape_prep(ans))
  paste0("Possible answers: ", paste(numbered, collapse = "; "), ". ")
}

write_q62_q68_tex_tables <- function(tab, out_dir = "output/acp") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  qs <- names(Q62_Q68_TITLES)
  qs <- qs[qs %in% as.character(tab$question)]
  paths <- character()
  for (q in qs) {
    d <- dplyr::filter(tab, as.character(.data$question) == .env$q)
    title <- unique(d$question_title)[1]
    caption <- paste0(
      "Direct single-component preferences (", q, "). ",
      title, " ",
      format_q62_q68_answers_caption(q),
      "Shares (\\%) among respondents assigned to the question."
    )
    if (q %in% names(Q62_Q68_SPLIT_NOTE)) {
      caption <- paste0(caption, " ", Q62_Q68_SPLIT_NOTE[[q]])
    }
    lines <- c(
      "% Requires \\usepackage{booktabs}",
      "\\begin{table}[htbp]",
      "  \\centering",
      "  \\footnotesize",
      paste0("  \\caption{", caption, "}"),
      paste0("  \\label{tab:", tolower(q), "-shares}"),
      paste0("  ", format_q62_q68_tabular(d)),
      "\\end{table}"
    )
    path <- file.path(out_dir, paste0(tolower(q), "_shares.tex"))
    writeLines(lines, path)
    message("Saved ", path)
    paths <- c(paths, path)
  }
  invisible(paths)
}

extract_conjoint_timing <- function(data_wide, timers = CONJOINT_QT_TIMERS) {
  field_suffix <- c("First Click", "Last Click", "Page Submit", "Click Count")
  qt_cols <- intersect(
    names(data_wide),
    unlist(lapply(timers$timer, function(t) paste(t, field_suffix, sep = "_")))
  )
  if (!length(qt_cols)) {
    stop("No QT7–QT11 timing columns in wide data.", call. = FALSE)
  }
  data_wide %>%
    dplyr::select("Country", "ID", dplyr::all_of(qt_cols)) %>%
    dplyr::distinct() %>%
    tidyr::pivot_longer(
      -c("Country", "ID"),
      names_to = c("timer", "field"),
      names_pattern = "^(QT[0-9]+)_(.*)$",
      values_to = "value"
    ) %>%
    dplyr::mutate(value = as.numeric(.data$value)) %>%
    tidyr::pivot_wider(names_from = "field", values_from = "value") %>%
    dplyr::rename(
      first_click  = "First Click",
      last_click   = "Last Click",
      page_submit  = "Page Submit",
      click_count  = "Click Count"
    ) %>%
    dplyr::left_join(timers, by = "timer") %>%
    dplyr::mutate(
      Country = factor(.data$Country, levels = COUNTRY_LEVELS_4),
      timer = factor(.data$timer, levels = timers$timer),
      page_lab = factor(.data$page_lab, levels = timers$page_lab),
      click_span = pmax(.data$last_click - .data$first_click, 0)
    )
}

summarise_conjoint_timing <- function(timing) {
  timing %>%
    dplyr::group_by(.data$Country, .data$timer, .data$page_lab) %>%
    dplyr::summarise(
      n            = dplyr::n(),
      n_page       = sum(!is.na(.data$page_submit)),
      median_page  = stats::median(.data$page_submit, na.rm = TRUE),
      p10_page     = stats::quantile(.data$page_submit, 0.10, na.rm = TRUE),
      p90_page     = stats::quantile(.data$page_submit, 0.90, na.rm = TRUE),
      median_span  = stats::median(.data$click_span, na.rm = TRUE),
      .groups      = "drop"
    ) %>%
    dplyr::arrange(.data$timer, .data$Country)
}

plot_conjoint_timing_cdf <- function(timing, p_cap = 0.99, sample_note = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for timing CDFs.", call. = FALSE)
  }
  d <- timing %>%
    dplyr::filter(!is.na(.data$page_submit), .data$page_submit >= 0) %>%
    dplyr::group_by(.data$page_lab) %>%
    dplyr::filter(
      .data$page_submit <= stats::quantile(.data$page_submit, p_cap, na.rm = TRUE)
    ) %>%
    dplyr::ungroup()
  cap <- paste0(
    "Each panel x-axis truncated at the ", 100 * p_cap, "th percentile."
  )
  if (!is.null(sample_note) && nzchar(sample_note)) {
    cap <- paste(cap, sample_note)
  }
  ggplot2::ggplot(d, ggplot2::aes(x = .data$page_submit, colour = .data$Country)) +
    ggplot2::stat_ecdf(geom = "step", linewidth = 0.45) +
    ggplot2::facet_wrap(~ page_lab, ncol = 1, scales = "free_x") +
    ggplot2::scale_colour_manual(values = PREP_COUNTRY_COLOURS) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(100 * x), "%")
    ) +
    ggplot2::labs(
      x = "Time on page (seconds, Qualtrics Page Submit)",
      y = "Cumulative share of respondents",
      colour = NULL,
      caption = cap
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      strip.background   = ggplot2::element_rect(fill = "grey95"),
      strip.text         = ggplot2::element_text(size = 8, face = "bold"),
      legend.position    = "bottom",
      axis.text          = ggplot2::element_text(size = 7),
      axis.title         = ggplot2::element_text(size = 8),
      plot.caption       = ggplot2::element_text(size = 7, hjust = 0)
    )
}

# Respondents who ranked B first on every completed first-rank task.
# always_pick_b is defined only for four complete tasks (B is always the
# middle option in C*_1_DO). That is a quality flag, not a preference type.
build_always_pick_b_flags <- function(data_long) {
  data_long %>%
    dplyr::filter(.data$Preferred == 1L) %>%
    dplyr::group_by(.data$Country, .data$ID) %>%
    dplyr::summarise(
      n_first_ranks = dplyr::n(),
      n_b_first     = sum(.data$Profile == "B"),
      .groups       = "drop"
    ) %>%
    dplyr::mutate(
      always_pick_b = .data$n_first_ranks == 4L & .data$n_b_first == 4L
    )
}

summarise_always_pick_b_drop <- function(flags) {
  four <- flags %>% dplyr::filter(.data$n_first_ranks == 4L)
  dplyr::bind_rows(
    tibble::tibble(
      Country     = factor("Pooled", levels = c("Pooled", COUNTRY_LEVELS_4)),
      n_respondents = nrow(flags),
      n_four_task   = nrow(four),
      n_always_b    = sum(flags$always_pick_b),
      share_always_b = mean(four$always_pick_b)
    ),
    flags %>%
      dplyr::group_by(.data$Country) %>%
      dplyr::summarise(
        n_respondents  = dplyr::n(),
        n_four_task    = sum(.data$n_first_ranks == 4L),
        n_always_b     = sum(.data$always_pick_b),
        share_always_b = sum(.data$always_pick_b) /
          sum(.data$n_first_ranks == 4L),
        .groups = "drop"
      )
  ) %>%
    dplyr::mutate(
      Country = factor(.data$Country, levels = c("Pooled", COUNTRY_LEVELS_4))
    ) %>%
    dplyr::arrange(.data$Country)
}

filter_always_pick_b <- function(data_long, flags = NULL) {
  if (is.null(flags)) {
    flags <- build_always_pick_b_flags(data_long)
  }
  data_long %>%
    dplyr::semi_join(
      flags %>% dplyr::filter(!.data$always_pick_b),
      by = c("Country", "ID")
    )
}

# First-rank shares after the always-B quality drop. Rows are tasks, not
# countries: A and B are exchangeable reforms; B is always the middle option.
summarise_first_position_check <- function(data_long) {
  tasks <- data_long %>%
    dplyr::filter(.data$Preferred == 1L, !is.na(.data$display_order_step1)) %>%
    dplyr::mutate(
      listed_slot = as.integer(.data$screen_position_step1),
      Choice      = as.character(.data$Profile)
    ) %>%
    dplyr::select("Country", "ID", "Task", "Choice", "listed_slot")

  summarise_one <- function(x) {
    x %>%
      dplyr::summarise(
        n          = dplyr::n(),
        p_listed_1 = mean(.data$listed_slot == 1L),
        p_listed_2 = mean(.data$listed_slot == 2L),
        p_listed_3 = mean(.data$listed_slot == 3L),
        p_A        = mean(.data$Choice == "A"),
        p_B        = mean(.data$Choice == "B"),
        p_C        = mean(.data$Choice == "C"),
        .groups    = "drop"
      ) %>%
      dplyr::mutate(b_minus_a = .data$p_B - .data$p_A)
  }

  by_country <- dplyr::bind_rows(
    tasks %>%
      dplyr::mutate(
        Country = factor("Pooled", levels = c("Pooled", COUNTRY_LEVELS_4))
      ) %>%
      dplyr::group_by(.data$Country) %>%
      summarise_one(),
    tasks %>%
      dplyr::group_by(.data$Country) %>%
      summarise_one()
  ) %>%
    dplyr::mutate(
      Country = factor(.data$Country, levels = c("Pooled", COUNTRY_LEVELS_4))
    ) %>%
    dplyr::arrange(.data$Country)

  by_task <- tasks %>%
    dplyr::group_by(.data$Task) %>%
    summarise_one() %>%
    dplyr::arrange(.data$Task)

  list(by_country = by_country, by_task = by_task)
}

plot_first_position_check <- function(tab) {
  d <- tab$by_task %>%
    tidyr::pivot_longer(
      cols = c("p_listed_1", "p_listed_3"),
      names_to = "slot",
      values_to = "p"
    ) %>%
    dplyr::mutate(
      Task = factor(
        .data$Task,
        levels = c("0", "1", "2", "3"),
        labels = c("Task 1", "Task 2", "Task 3", "Task 4")
      ),
      slot = factor(
        .data$slot,
        levels = c("p_listed_1", "p_listed_3"),
        labels = c("Shown first", "Shown third")
      )
    )
  ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$Task, y = .data$p, colour = .data$slot, group = .data$slot
    )
  ) +
    ggplot2::geom_line(linewidth = 0.7) +
    ggplot2::geom_point(size = 2.2) +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(100 * x), "%"),
      limits = c(0, NA)
    ) +
    ggplot2::scale_colour_manual(
      values = c("Shown first" = "#4DBBD5FF", "Shown third" = "#3C5488FF")
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Share ranked first",
      colour = "Position in the A/B/C choice list"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      legend.position    = "bottom",
      legend.title       = ggplot2::element_text(size = 8),
      legend.text        = ggplot2::element_text(size = 8),
      axis.text          = ggplot2::element_text(size = 8),
      axis.title         = ggplot2::element_text(size = 8)
    )
}

format_first_position_tabular <- function(by_task) {
  d <- by_task %>%
    dplyr::mutate(
      Task = factor(
        .data$Task,
        levels = c("0", "1", "2", "3"),
        labels = c("1", "2", "3", "4")
      )
    ) %>%
    dplyr::arrange(.data$Task)
  pct <- function(x) sprintf("%.1f", 100 * x)
  body <- sprintf(
    "    %s & %s & %s & %s & %s & %s \\\\",
    as.character(d$Task),
    prettyNum(d$n, big.mark = ",", scientific = FALSE),
    pct(d$p_A), pct(d$p_B), pct(d$p_C),
    sprintf("$%s%.1f$", ifelse(d$b_minus_a < 0, "-", ""), abs(100 * d$b_minus_a))
  )
  c(
    "\\begin{tabular}{lrrrrr}",
    "  \\toprule",
    "  Task & $N$ tasks & $P$(A) & $P$(B) & $P$(C) & B$-$A \\\\",
    "  \\midrule",
    body,
    "  \\bottomrule",
    "\\end{tabular}"
  )
}

write_first_position_tex <- function(tab, drop_info, out_dir = "output/acp") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  pooled <- drop_info %>%
    dplyr::filter(as.character(.data$Country) == "Pooled")
  n_drop <- pooled$n_always_b[[1]]
  n_four <- pooled$n_four_task[[1]]
  share  <- sprintf("%.1f", 100 * pooled$share_always_b[[1]])
  by_task <- tab$by_task %>% dplyr::arrange(.data$Task)
  ba1 <- sprintf("%.1f", 100 * by_task$b_minus_a[by_task$Task == "0"])
  ba4 <- sprintf("%.1f", 100 * by_task$b_minus_a[by_task$Task == "3"])
  caption <- paste0(
    "First ranks by conjoint task, after dropping respondents who ranked B first ",
    "on all four tasks ($N$ dropped = ",
    prettyNum(n_drop, big.mark = ",", scientific = FALSE),
    " of ",
    prettyNum(n_four, big.mark = ",", scientific = FALSE),
    " with four complete ranks, ", share, "\\%). ",
    "Step 1 is a three-option list (A, B or C) in order A$|$B$|$C or C$|$B$|$A, ",
    "so B is always the middle option. ",
    "The conjoint grid shows reform packages A and B; C (repeal) is not in the grid. ",
    "A and B are independently drawn, so $P$(A) and $P$(B) should match if ",
    "respondents read the content. ",
    "Entries are shares of tasks (\\%). B$-$A is ", ba1, " pp on task 1 and ",
    ba4, " pp on task 4."
  )
  lines <- c(
    "% Requires \\usepackage{booktabs}",
    "\\begin{table}[htbp]",
    "  \\centering",
    "  \\small",
    paste0("  \\caption{", caption, "}"),
    "  \\label{tab:conjoint-first-position}",
    paste0("  ", format_first_position_tabular(tab$by_task)),
    "\\end{table}"
  )
  path <- file.path(out_dir, "conjoint_first_position.tex")
  writeLines(lines, path)
  message("Saved ", path)
  invisible(path)
}

# Coherence of repeal (C) ranks with stated opposition (Q46_2, Q61A).
build_repeal_opposition_respondent <- function(data_long) {
  data_long %>%
    dplyr::filter(.data$Profile == "C") %>%
    dplyr::group_by(
      .data$Country, .data$ID, .data$Q46_2_cat, .data$Q61A_response
    ) %>%
    dplyr::summarise(
      n_tasks      = dplyr::n(),
      share_first  = mean(.data$Preferred == 1L, na.rm = TRUE),
      share_last   = mean(.data$Preferred_Least == 1L, na.rm = TRUE),
      always_first = dplyr::n() == 4L &
        sum(.data$Preferred == 1L, na.rm = TRUE) == 4L,
      always_last  = dplyr::n() == 4L &
        sum(.data$Preferred_Least == 1L, na.rm = TRUE) == 4L,
      .groups = "drop"
    )
}

summarise_repeal_opposition <- function(resp) {
  one <- function(d, group_col, levels) {
    d %>%
      dplyr::filter(!is.na(.data[[group_col]])) %>%
      dplyr::mutate(
        attitude = factor(.data[[group_col]], levels = levels)
      ) %>%
      dplyr::filter(!is.na(.data$attitude)) %>%
      dplyr::group_by(.data$attitude) %>%
      dplyr::summarise(
        n              = dplyr::n(),
        p_task_first   = mean(.data$share_first),
        p_task_last    = mean(.data$share_last),
        p_always_first = mean(.data$always_first),
        p_always_last  = mean(.data$always_last),
        .groups        = "drop"
      ) %>%
      dplyr::arrange(.data$attitude)
  }
  list(
    q46 = one(resp, "Q46_2_cat", Q46_2_LEVELS),
    q61a = one(resp, "Q61A_response", Q61A_LEVELS),
    n_resp = nrow(resp),
    n_q46_missing = sum(is.na(resp$Q46_2_cat)),
    n_q61_missing = sum(is.na(resp$Q61A_response))
  )
}

plot_repeal_opposition <- function(tab) {
  lab_levels <- c(Q46_2_LEVELS, unname(Q61A_LABELS[Q61A_LEVELS]))
  q46 <- tab$q46 %>%
    dplyr::mutate(
      panel        = "Post-info attitude (Q46_2)",
      attitude_lab = as.character(.data$attitude)
    )
  q61 <- tab$q61a %>%
    dplyr::mutate(
      panel        = "Direct repeal vote (Q61A)",
      attitude_lab = unname(Q61A_LABELS[as.character(.data$attitude)])
    )
  d <- dplyr::bind_rows(q46, q61) %>%
    tidyr::pivot_longer(
      cols = c("p_task_first", "p_task_last"),
      names_to = "rank",
      values_to = "p"
    ) %>%
    dplyr::mutate(
      panel = factor(
        .data$panel,
        levels = c("Post-info attitude (Q46_2)", "Direct repeal vote (Q61A)")
      ),
      rank = factor(
        .data$rank,
        levels = c("p_task_first", "p_task_last"),
        labels = c("C ranked first", "C ranked last")
      ),
      attitude_lab = factor(.data$attitude_lab, levels = lab_levels)
    )
  ggplot2::ggplot(
    d,
    ggplot2::aes(x = .data$attitude_lab, y = .data$p, fill = .data$rank)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8), width = 0.7) +
    ggplot2::facet_wrap(~ panel, ncol = 2, scales = "free_x") +
    ggplot2::scale_y_continuous(
      labels = function(x) paste0(round(100 * x), "%"),
      limits = c(0, 1)
    ) +
    ggplot2::scale_fill_manual(
      values = c("C ranked first" = "#E64B35FF", "C ranked last" = "#3C5488FF")
    ) +
    ggplot2::labs(
      x = NULL,
      y = "Share of tasks",
      fill = "Repeal (C)"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      strip.background   = ggplot2::element_rect(fill = "grey95"),
      strip.text         = ggplot2::element_text(size = 8, face = "bold"),
      legend.position    = "bottom",
      legend.title       = ggplot2::element_text(size = 8),
      legend.text        = ggplot2::element_text(size = 8),
      axis.text.x        = ggplot2::element_text(size = 7, angle = 30, hjust = 1),
      axis.text.y        = ggplot2::element_text(size = 8),
      axis.title         = ggplot2::element_text(size = 8)
    )
}

format_repeal_opposition_tabular <- function(d, stub_header) {
  pct <- function(x) sprintf("%.1f", 100 * x)
  body <- sprintf(
    "    %s & %s & %s & %s & %s & %s \\\\",
    latex_escape_prep(as.character(d$attitude)),
    prettyNum(d$n, big.mark = ",", scientific = FALSE),
    pct(d$p_task_first), pct(d$p_task_last),
    pct(d$p_always_first), pct(d$p_always_last)
  )
  c(
    "\\begin{tabular}{lrrrrr}",
    "  \\toprule",
    paste0(
      "  ", stub_header,
      " & $N$ & C 1st & C last & Always C 1st & Always C last \\\\"
    ),
    "  \\midrule",
    body,
    "  \\bottomrule",
    "\\end{tabular}"
  )
}

write_repeal_opposition_tex <- function(tab, out_dir = "output/acp") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  q61 <- tab$q61a %>%
    dplyr::mutate(
      attitude = unname(Q61A_LABELS[as.character(.data$attitude)])
    )
  caption <- paste0(
    "Coherence of conjoint repeal ranks with stated opposition. ",
    "C 1st / C last are the share of tasks in which repeal is ranked first or last. ",
    "Always is the share of respondents who rank repeal first (last) on all four tasks. ",
    "Sample excludes respondents who ranked B first on all four tasks ",
    "($N$ = ", prettyNum(tab$n_resp, big.mark = ",", scientific = FALSE), "). ",
    "Q46\\_2 missing for ",
    prettyNum(tab$n_q46_missing, big.mark = ",", scientific = FALSE),
    " respondents (omitted from the upper panel)."
  )
  lines <- c(
    "% Requires \\usepackage{booktabs}",
    "\\begin{table}[htbp]",
    "  \\centering",
    "  \\small",
    paste0("  \\caption{", caption, "}"),
    "  \\label{tab:repeal-opposition}",
    "  \\textit{Post-info carbon-pricing attitude (Q46\\_2)}\\\\[0.4em]",
    paste0("  ", format_repeal_opposition_tabular(tab$q46, "Attitude")),
    "  \\vspace{0.9em}",
    "  \\textit{Direct vote to abolish carbon pricing (Q61A)}\\\\[0.4em]",
    paste0("  ", format_repeal_opposition_tabular(q61, "Vote")),
    "\\end{table}"
  )
  path <- file.path(out_dir, "repeal_opposition.tex")
  writeLines(lines, path)
  message("Saved ", path)
  invisible(path)
}

summarise_display_order <- function(data_long) {
  task <- data_long %>%
    dplyr::distinct(
      .data$Country, .data$ID, .data$Task,
      .data$display_order_step1
    )
  pos <- data_long %>%
    dplyr::count(.data$Country, .data$Profile, .data$screen_position_step1) %>%
    dplyr::group_by(.data$Country, .data$Profile) %>%
    dplyr::mutate(share = .data$n / sum(.data$n)) %>%
    dplyr::ungroup()
  list(
    n_missing_order = sum(is.na(task$display_order_step1)),
    n_tasks         = nrow(task),
    order_counts    = task %>% dplyr::count(.data$display_order_step1, sort = TRUE),
    position_share  = pos
  )
}

build_conjoint_level_lookup <- function(data_conjoint_country, country_name, level_short) {
  purrr::map_dfr(CONJOINT_ATTR_COLS, function(attr) {
    lev <- levels(data_conjoint_country[[attr]])
    lev <- lev[lev != "Repeal"]
    tibble::tibble(
      Country     = country_name,
      attribute   = attr,
      level       = lev,
      level_order = seq_along(lev)
    )
  }) %>%
    dplyr::left_join(level_short, by = c("attribute", "level_order"))
}
