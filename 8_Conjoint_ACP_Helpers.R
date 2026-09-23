# Helpers for conjoint ACP analysis (Ganter framework).

CONJOINT_ATTR_COLS <- c(
  "budget_and_funding",
  "budget_control",
  "household_support",
  "information",
  "infrastructure_ownership",
  "worker_support",
  "community_mobility_support"
)

CONJOINT_ATTR_LABELS <- c(
  budget_and_funding         = "Budget & funding",
  budget_control             = "Budget control",
  household_support          = "Household support",
  information                = "Information",
  infrastructure_ownership   = "Infrastructure ownership",
  worker_support             = "Worker support",
  community_mobility_support = "Community mobility"
)

#' Attribute labels for ACP figures.
PAPER_ATTR_LABELS <- CONJOINT_ATTR_LABELS
PAPER_ATTR_LABELS["budget_and_funding"] <- "Transition budget size & funding"
PAPER_ATTR_LABELS["budget_control"]     <- "Control over transition budget"

DPP_ATTR_LABELS <- c(
  budget_and_funding         = "Budget and funding",
  budget_control             = "Budget control",
  household_support          = "Household support",
  information                = "Information",
  infrastructure_ownership   = "Infrastructure ownership",
  worker_support             = "Worker support",
  community_mobility_support = "Community mobility"
)

PANEL_A_XLAB <- "Average component preference (0 = indifference)"

# ---- Respondent repeal flags -----------------------------------------------------

build_task_repeal_ranks <- function(data_conjoint) {
  data_conjoint %>%
    dplyr::filter(Profile == "C") %>%
    dplyr::transmute(
      Country,
      ID,
      Task,
      repeal_ranked_1st  = Preferred == 1L,
      repeal_ranked_last = Preferred_Least == 1L
    )
}

build_repeal_respondent_flags <- function(data_conjoint,
                                          respondents = NULL,
                                          n_tasks_complete = 4L) {
  task_repeal <- build_task_repeal_ranks(data_conjoint)
  if (!is.null(respondents)) {
    task_repeal <- task_repeal %>%
      dplyr::semi_join(respondents, by = c("Country", "ID"))
  }
  task_repeal %>%
    dplyr::group_by(Country, ID) %>%
    dplyr::summarise(
      n_repeal_tasks = dplyr::n(),
      n_repeal_first = sum(repeal_ranked_1st, na.rm = TRUE),
      n_repeal_last  = sum(repeal_ranked_last, na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    dplyr::mutate(
      repeal_always_first = n_repeal_tasks == n_tasks_complete & n_repeal_first == n_tasks_complete,
      repeal_always_last  = n_repeal_tasks == n_tasks_complete & n_repeal_last == n_tasks_complete
    )
}

enrich_conjoint_with_respondent_meta <- function(data_conjoint, respondent_meta) {
  data_conjoint %>%
    dplyr::left_join(respondent_meta, by = c("Country", "ID"))
}

# ---- Reform–repeal pairs ---------------------------------------------------------

derive_profile_rank <- function(data) {
  data %>%
    dplyr::mutate(
      rank = dplyr::case_when(
        Preferred == 1L        ~ 1L,
        Preferred_Second == 1L ~ 2L,
        Preferred_Least == 1L  ~ 3L,
        TRUE                   ~ NA_integer_
      )
    )
}

#' One reform profile vs repeal (C) per task. Profile 1 = reform, profile 2 = repeal.
build_reform_repeal_pairs_one <- function(data_ranked,
                                          reform_profile = c("A", "B"),
                                          attr_cols = CONJOINT_ATTR_COLS) {
  reform_profile <- match.arg(reform_profile)
  attr_cols <- intersect(attr_cols, names(data_ranked))

  reform <- data_ranked %>%
    dplyr::filter(Profile == reform_profile) %>%
    dplyr::select(
      Country, ID, Task,
      dplyr::any_of(c("d1", "d2")),
      rank,
      dplyr::all_of(attr_cols)
    ) %>%
    dplyr::rename_with(~ paste0(., "_1"), dplyr::all_of(c("rank", attr_cols)))

  repeal <- data_ranked %>%
    dplyr::filter(Profile == "C") %>%
    dplyr::select(Country, ID, Task, rank) %>%
    dplyr::rename(rank_2 = rank)

  reform %>%
    dplyr::inner_join(repeal, by = c("Country", "ID", "Task")) %>%
    dplyr::mutate(
      profile_1      = reform_profile,
      profile_2      = "C",
      reform_profile = reform_profile,
      repeal_profile = "C",
      pair_type      = "reform_repeal",
      pair_id        = paste(Country, ID, Task, reform_profile, "vs_C", sep = ":"),
      outcome        = as.integer(rank_1 < rank_2),
      clust          = ID
    )
}

#' All reform–repeal pairs (A–C and B–C).
build_reform_repeal_pairs <- function(data_conjoint, attr_cols = CONJOINT_ATTR_COLS) {
  ranked <- derive_profile_rank(data_conjoint) %>%
    dplyr::filter(!is.na(rank))

  dplyr::bind_rows(
    build_reform_repeal_pairs_one(ranked, "A", attr_cols),
    build_reform_repeal_pairs_one(ranked, "B", attr_cols)
  )
}

#' Subset pairs to rows where the attribute is defined (split-sample aware).
#' @param both_profiles If TRUE (reform–reform), require valid levels on profiles 1 and 2.
filter_pairs_for_attribute <- function(pairs,
                                       attribute,
                                       both_profiles = FALSE) {
  level_col_1 <- paste0(attribute, "_1")
  level_col_2 <- paste0(attribute, "_2")
  idx_col_1   <- paste0(attribute, "_idx_1")
  idx_col_2   <- paste0(attribute, "_idx_2")

  out <- pairs
  if (attribute == "worker_support") {
    out <- out %>% dplyr::filter(d1 == "worker_support")
  } else if (attribute == "community_mobility_support") {
    out <- out %>% dplyr::filter(d1 == "community_mobility_support")
  } else if (attribute == "information") {
    out <- out %>% dplyr::filter(d2 == "information")
  } else if (attribute == "infrastructure_ownership") {
    out <- out %>% dplyr::filter(d2 == "infrastructure_ownership")
  }

  out <- out %>%
    dplyr::filter(
      !is.na(.data[[level_col_1]]),
      .data[[level_col_1]] != "Missing",
      .data[[level_col_1]] != "Repeal",
      !is.na(.data[[idx_col_1]])
    )

  if (!both_profiles) {
    return(out)
  }

  out %>%
    dplyr::filter(
      !is.na(.data[[level_col_2]]),
      .data[[level_col_2]] != "Missing",
      .data[[level_col_2]] != "Repeal",
      !is.na(.data[[idx_col_2]])
    )
}

#' Columns carried through into V-tables (excluding contrast columns).
v_table_meta_cols <- function() {
  c(
    "outcome", "pair_id", "Country", "ID", "Task", "clust",
    "d1", "d2", "pair_type", "profile_1", "profile_2", "reform_profile",
    "repeal_profile",
    "n_repeal_tasks", "n_repeal_first", "n_repeal_last",
    "repeal_always_first", "repeal_always_last"
  )
}

select_v_table_meta <- function(pairs) {
  pairs %>%
    dplyr::select(
      dplyr::any_of(v_table_meta_cols()),
      dplyr::starts_with("repeal_")
    )
}

# ---- Reform–reform pairs (A vs B) ------------------------------------------------

#' A vs B per task. Profile 1 = A, profile 2 = B.
build_reform_reform_pairs <- function(data_conjoint, attr_cols = CONJOINT_ATTR_COLS) {
  ranked <- derive_profile_rank(data_conjoint) %>%
    dplyr::filter(!is.na(rank))

  attr_cols <- intersect(attr_cols, names(ranked))

  profile_a <- ranked %>%
    dplyr::filter(Profile == "A") %>%
    dplyr::select(
      Country, ID, Task,
      dplyr::any_of(c("d1", "d2")),
      rank,
      dplyr::all_of(attr_cols)
    ) %>%
    dplyr::rename_with(~ paste0(., "_1"), dplyr::all_of(c("rank", attr_cols)))

  profile_b <- ranked %>%
    dplyr::filter(Profile == "B") %>%
    dplyr::select(Country, ID, Task, rank, dplyr::all_of(attr_cols)) %>%
    dplyr::rename_with(~ paste0(., "_2"), dplyr::all_of(c("rank", attr_cols)))

  profile_a %>%
    dplyr::inner_join(profile_b, by = c("Country", "ID", "Task")) %>%
    dplyr::mutate(
      profile_1   = "A",
      profile_2   = "B",
      pair_type   = "reform_reform",
      pair_id     = paste(Country, ID, Task, "A_vs_B", sep = ":"),
      outcome     = as.integer(rank_1 < rank_2),
      clust       = ID
    )
}

#' Map level strings to indices on both profiles (reform–reform).
add_attribute_level_indices <- function(pairs, attribute, level_lookup) {
  level_col_1 <- paste0(attribute, "_1")
  level_col_2 <- paste0(attribute, "_2")
  idx_col_1   <- paste0(attribute, "_idx_1")
  idx_col_2   <- paste0(attribute, "_idx_2")

  lk <- level_lookup %>%
    dplyr::filter(.data$attribute == .env$attribute) %>%
    dplyr::distinct(Country, level, level_order)

  pairs %>%
    dplyr::left_join(
      lk %>% dplyr::rename(!!level_col_1 := level, !!idx_col_1 := level_order),
      by = c("Country", level_col_1)
    ) %>%
    dplyr::left_join(
      lk %>% dplyr::rename(!!level_col_2 := level, !!idx_col_2 := level_order),
      by = c("Country", level_col_2)
    )
}

#' Ganter V columns for reform–reform: all level–level contrasts among reform levels.
build_v_contrasts_reform_reform <- function(pairs, attribute) {
  idx1 <- pairs[[paste0(attribute, "_idx_1")]]
  idx2 <- pairs[[paste0(attribute, "_idx_2")]]

  reform_levels <- sort(unique(stats::na.omit(c(idx1, idx2))))
  if (length(reform_levels) < 2L) {
    return(NULL)
  }

  level_combinations <- utils::combn(reform_levels, 2)
  v_cols <- stats::setNames(
    lapply(seq_len(ncol(level_combinations)), function(j) {
      l1 <- level_combinations[1, j]
      l2 <- level_combinations[2, j]
      ifelse(idx1 == l1 & idx2 == l2, 1L,
             ifelse(idx1 == l2 & idx2 == l1, -1L, 0L))
    }),
    paste0(attribute, ".", level_combinations[1, ], "-", level_combinations[2, ])
  )

  dplyr::bind_cols(
    select_v_table_meta(pairs),
    v_cols
  ) %>%
    dplyr::select(dplyr::where(~ !all(. == 0)))
}

build_v_table_reform_reform <- function(pairs, attribute, level_lookup) {
  pairs %>%
    add_attribute_level_indices(attribute, level_lookup) %>%
    filter_pairs_for_attribute(attribute, both_profiles = TRUE) %>%
    build_v_contrasts_reform_reform(attribute)
}

build_all_v_tables_reform_reform <- function(pairs,
                                             attributes = CONJOINT_ATTR_COLS,
                                             level_lookup) {
  tables <- lapply(attributes, function(attr) {
    build_v_table_reform_reform(pairs, attr, level_lookup)
  })
  names(tables) <- attributes
  tables[!vapply(tables, is.null, logical(1))]
}

summarise_v_tables <- function(v_tables) {
  meta <- v_table_meta_cols()
  tibble::tibble(
    attribute   = names(v_tables),
    n_pairs     = vapply(v_tables, nrow, integer(1)),
    n_v_cols    = vapply(v_tables, function(x) sum(!names(x) %in% meta), integer(1)),
    v_col_names = vapply(v_tables, function(x) {
      paste(names(x)[!names(x) %in% meta], collapse = ", ")
    }, character(1))
  )
}

# ---- ACP from V tables -----------------------------------------------------------

#' Path to Ganter's conjacp.R (relative to project root).
find_conjacp_r_path <- function() {
  candidates <- c(
    "replication/Functions/conjacp.R",
    "../replication/Functions/conjacp.R"
  )
  hits <- candidates[file.exists(candidates)]
  if (!length(hits)) {
    stop(
      "Cannot find replication/Functions/conjacp.R. ",
      "Set working directory to the conjoint_analysis project root ",
      "(Session → Set Working Directory → To Source File may also work).",
      call. = FALSE
    )
  }
  normalizePath(hits[[1]])
}

CONJACP_ACP_PATCH_VERSION <- 2L

#' Load (or reload) conjacp.estimation() from replication/Functions/conjacp.R.
#' Always reloads when the patched version in memory is stale.
ensure_conjacp_loaded <- function() {
  loaded_ver <- if (exists(".conjacp_acp_patch_version", envir = .GlobalEnv, inherits = FALSE)) {
    get(".conjacp_acp_patch_version", envir = .GlobalEnv, inherits = FALSE)
  } else {
    0L
  }
  if (loaded_ver < CONJACP_ACP_PATCH_VERSION ||
      !exists("conjacp.estimation", mode = "function", inherits = TRUE)) {
    message("Loading conjacp.R (patch v", CONJACP_ACP_PATCH_VERSION, ")")
    source(find_conjacp_r_path(), local = FALSE)
  }
  if (!exists("conjacp.estimation", mode = "function", inherits = TRUE)) {
    stop("Loaded conjacp.R but conjacp.estimation is still missing.", call. = FALSE)
  }
  if (!exists(".conjacp_acp_patch_version", envir = .GlobalEnv, inherits = FALSE) ||
      get(".conjacp_acp_patch_version", envir = .GlobalEnv, inherits = FALSE) < CONJACP_ACP_PATCH_VERSION) {
    stop(
      "conjacp.R in replication/Functions/ is outdated (missing patch version). ",
      "Pull latest conjacp.R or restart R and re-source conjoint_acp.R.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Drop respondents who always rank repeal first and/or last (4 tasks complete).
filter_pairs_by_repeal_rankers <- function(pairs,
                                           exclude_always_first = FALSE,
                                           exclude_always_last = FALSE) {
  if (!exclude_always_first && !exclude_always_last) {
    return(pairs)
  }
  pairs %>%
    dplyr::filter(
      !exclude_always_first | !dplyr::coalesce(.data$repeal_always_first, FALSE),
      !exclude_always_last  | !dplyr::coalesce(.data$repeal_always_last, FALSE)
    )
}

summarise_repeal_ranker_exclusions <- function(pairs_before, pairs_after) {
  tibble::tibble(
    n_pairs_before    = nrow(pairs_before),
    n_pairs_after     = nrow(pairs_after),
    n_pairs_dropped   = nrow(pairs_before) - nrow(pairs_after),
    n_resp_before     = dplyr::n_distinct(pairs_before$ID),
    n_resp_after      = dplyr::n_distinct(pairs_after$ID),
    n_resp_dropped    = dplyr::n_distinct(pairs_before$ID) - dplyr::n_distinct(pairs_after$ID)
  )
}

#' Level labels for one attribute (index i = position i in attr_levels).
build_attr_levels_for_attribute <- function(attribute,
                                            level_lookup,
                                            country = NULL) {
  lk <- level_lookup %>%
    dplyr::filter(.data$attribute == .env$attribute)
  if (!is.null(country)) {
    lk <- lk %>% dplyr::filter(.data$Country == .env$country)
  }
  lk %>%
    dplyr::arrange(.data$level_order) %>%
    dplyr::distinct(.data$level_order, .data$level_short) %>%
    dplyr::pull(.data$level_short)
}

#' Minimal conjacp.prepdata object from one attribute V-table (one attribute per call).
build_prepdata_from_v_table <- function(v_table,
                                        attribute,
                                        level_lookup,
                                        country = NULL) {
  meta <- v_table_meta_cols()
  if (!is.null(country)) {
    v_table <- v_table %>% dplyr::filter(.data$Country == .env$country)
  }
  if (!nrow(v_table)) {
    stop("No pairs left after country filter.", call. = FALSE)
  }

  v_cols <- setdiff(names(v_table), meta)
  if (!length(v_cols)) {
    stop("V-table has no contrast columns for ", attribute, ".", call. = FALSE)
  }

  labels <- build_attr_levels_for_attribute(
    attribute, level_lookup, country
  )
  level_weights <- list(list(attribute, rep(1, length(labels))))

  v_mat <- as.data.frame(v_table[, v_cols, drop = FALSE], stringsAsFactors = FALSE, check.names = FALSE)
  data_prep <- data.frame(
    outcome = as.integer(v_table$outcome),
    v_mat,
    clust   = v_table$clust,
    id      = seq_len(nrow(v_table)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  data_prep <- as.data.frame(data_prep, stringsAsFactors = FALSE, check.names = FALSE)
  data_wide <- data.frame(
    clust    = v_table$clust,
    subgroup = factor(1L, levels = 1L)
  )

  list(
    data_prep         = data_prep,
    data_wide         = data_wide,
    attr_x            = attribute,
    level_weights     = level_weights,
    attr_levels       = stats::setNames(list(labels), attribute),
    subgroups         = "subgroup",
    attr_restricted   = character(0),
    attr_unrestricted = attribute,
    attr_continuous   = character(0),
    levels_restricted = NULL
  )
}

#' Run pooled ACP (adjust = FALSE) for every attribute × country.
estimate_acp_from_v_tables <- function(v_tables,
                                     level_lookup,
                                     countries,
                                     estimand = "acp",
                                     adjust = FALSE) {
  ensure_conjacp_loaded()
  out <- stats::setNames(vector("list", length(countries)), countries)
  for (country in countries) {
    out[[country]] <- stats::setNames(vector("list", length(v_tables)), names(v_tables))
    for (attr in names(v_tables)) {
      prep <- build_prepdata_from_v_table(
        v_tables[[attr]], attr, level_lookup,
        country = country
      )
      out[[country]][[attr]] <- conjacp.estimation(
        prep, estimand = estimand, adjust = adjust
      )
    }
  }
  out
}

#' Pooled ACP across countries (harmonised levels; cluster = Country:ID).
estimate_pooled_acp_from_v_tables <- function(v_tables,
                                            level_lookup,
                                            estimand = "acp",
                                            adjust = FALSE) {
  ensure_conjacp_loaded()
  out <- stats::setNames(vector("list", length(v_tables)), names(v_tables))
  for (attr in names(v_tables)) {
    vt <- v_tables[[attr]]
    if (is.null(vt) || !nrow(vt)) next
    vt <- vt %>%
      dplyr::mutate(clust = paste(.data$Country, .data$ID, sep = ":"))
    prep <- build_prepdata_from_v_table(
      vt, attr, level_lookup,
      country = NULL
    )
    out[[attr]] <- conjacp.estimation(
      prep, estimand = estimand, adjust = adjust
    )
  }
  out[!vapply(out, is.null, logical(1))]
}

#' Wrap a per-attribute ACP list as country-nested results (for summarise/plot).
wrap_pooled_acp_as_country <- function(acp_by_attr, country_label = "Pooled") {
  stats::setNames(list(acp_by_attr), country_label)
}

#' Attribute importance = max(level ACP) - min(level ACP).
rank_attributes_by_acp_spread <- function(acp_tab) {
  acp_tab %>%
    dplyr::filter(.data$level != "Repeal", !is.na(.data$estimate)) %>%
    dplyr::group_by(.data$attribute) %>%
    dplyr::summarise(
      importance = {
        v <- .data$estimate
        if (length(v) < 2L) 0 else max(v) - min(v)
      },
      .groups = "drop"
    ) %>%
    dplyr::arrange(dplyr::desc(.data$importance))
}

#' Tidy conjacp.var() for one Ganter fit.
#'
#' Works on multi-attribute joint fits and on single-attribute V-table fits.
tidy_acp_variability <- function(res, group = NULL) {
  ensure_conjacp_loaded()
  if (!exists("conjacp.var", mode = "function", inherits = TRUE)) {
    stop("conjacp.var() not found after loading conjacp.R.", call. = FALSE)
  }
  var_out <- conjacp.var(res)
  out <- data.frame(
    attribute   = names(var_out$variability_estimates),
    variability = as.numeric(var_out$variability_estimates),
    var_ci_lo   = as.numeric(var_out$variability_lower),
    var_ci_hi   = as.numeric(var_out$variability_upper),
    range       = as.numeric(var_out$range_estimates),
    range_ci_lo = as.numeric(var_out$range_lower),
    range_ci_hi = as.numeric(var_out$range_upper),
    stringsAsFactors = FALSE
  )
  if (!is.null(group)) out$group <- group
  out
}

#' Per-attribute Ganter importance (conjacp.var on each attribute fit).
#'
#' @param acp_by_attr Named list of conjacp objects (one per attribute), as from
#'   estimate_pooled_acp_from_v_tables() or acp_results[[country]].
tidy_acp_variability_by_attribute <- function(acp_by_attr, group = NULL) {
  if (!length(acp_by_attr)) {
    return(data.frame(
      attribute = character(), variability = numeric(),
      var_ci_lo = numeric(), var_ci_hi = numeric(),
      range = numeric(), range_ci_lo = numeric(), range_ci_hi = numeric(),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(names(acp_by_attr), function(attr) {
    res <- acp_by_attr[[attr]]
    if (is.null(res)) return(NULL)
    tidy_acp_variability(res, group = group)
  })
  dplyr::bind_rows(rows)
}

#' Markdown facet strip: bold attribute title + italic Ganter importance (pp).
facet_title_with_importance <- function(attribute,
                                        var_row,
                                        titles = PAPER_ATTR_LABELS) {
  title <- unname(titles[as.character(attribute)])
  if (is.na(title) || !nzchar(title)) title <- as.character(attribute)
  if (is.null(var_row) || !nrow(var_row)) {
    return(sprintf("**%s**", title))
  }
  sprintf(
    "**%s**\n*importance: %.1f pp [%.1f-%.1f pp]*",
    title,
    100 * var_row$variability[[1L]],
    100 * var_row$var_ci_lo[[1L]],
    100 * var_row$var_ci_hi[[1L]]
  )
}

# ---- Package grid: P(reform beats repeal) by cell --------------------------------

PACKAGE_GRID_ATTRS <- CONJOINT_ATTR_COLS

#' Attach harmonised level orders and 5-slot package key (bf, bc, hh, trans, d2lev).
prepare_full_package_pairs <- function(pairs, level_lookup) {
  p <- pairs
  for (a in PACKAGE_GRID_ATTRS) {
    p <- add_reform_level_order(p, a, level_lookup)
  }
  p %>%
    dplyr::mutate(
      bf    = .data$budget_and_funding_order_1,
      bc    = .data$budget_control_order_1,
      hh    = .data$household_support_order_1,
      trans = dplyr::if_else(
        .data$d1 == "worker_support",
        .data$worker_support_order_1,
        .data$community_mobility_support_order_1
      ),
      d2lev = dplyr::if_else(
        .data$d2 == "information",
        .data$information_order_1,
        .data$infrastructure_ownership_order_1
      )
    )
}

#' P(reform beats repeal) for every observed full package cell.
summarise_package_grid_vs_repeal <- function(pairs,
                                             level_lookup,
                                             countries = NULL,
                                             pool_countries = FALSE,
                                             min_n = 40L) {
  p <- prepare_full_package_pairs(pairs, level_lookup)
  if (!is.null(countries)) {
    p <- dplyr::filter(p, .data$Country %in% .env$countries)
  }
  p <- p %>%
    dplyr::filter(
      !is.na(.data$bf), !is.na(.data$bc), !is.na(.data$hh),
      !is.na(.data$trans), !is.na(.data$d2lev)
    ) %>%
    dplyr::mutate(clust = paste(.data$Country, .data$ID, sep = ":"))

  grp <- if (pool_countries) {
    c("bf", "bc", "hh", "trans", "d2lev")
  } else {
    c("Country", "bf", "bc", "hh", "trans", "d2lev")
  }

  p %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::group_modify(~ compute_p_reform_beats_repeal(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$n_pairs >= min_n)
}

#' Split-sample package grid: keeps d1 (worker vs mobility) and d2 (info vs infra).
summarise_package_grid_split_vs_repeal <- function(pairs,
                                                   level_lookup,
                                                   countries = NULL,
                                                   pool_countries = FALSE,
                                                   min_n = 40L) {
  p <- prepare_full_package_pairs(pairs, level_lookup)
  if (!is.null(countries)) {
    p <- dplyr::filter(p, .data$Country %in% .env$countries)
  }
  p <- p %>%
    dplyr::filter(
      !is.na(.data$bf), !is.na(.data$bc), !is.na(.data$hh),
      !is.na(.data$trans), !is.na(.data$d2lev),
      !is.na(.data$d1), !is.na(.data$d2)
    ) %>%
    dplyr::mutate(clust = paste(.data$Country, .data$ID, sep = ":"))

  grp <- if (pool_countries) {
    c("bf", "bc", "hh", "d1", "trans", "d2", "d2lev")
  } else {
    c("Country", "bf", "bc", "hh", "d1", "trans", "d2", "d2lev")
  }

  p %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::group_modify(~ compute_p_reform_beats_repeal(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(.data$n_pairs >= min_n)
}

level_short_for <- function(level_lookup, attribute, level_order) {
  lk <- level_lookup %>%
    dplyr::filter(.data$attribute == .env$attribute, !is.na(.data$level_short)) %>%
    dplyr::distinct(.data$level_order, .data$level_short)
  out <- lk$level_short[match(level_order, lk$level_order)]
  unname(out)
}

budget_funding_label <- function(bf) {
  dplyr::case_when(
    bf == 1L ~ "Carbon revenue only",
    bf == 2L ~ "Larger budget (public debt)",
    bf == 3L ~ "Larger budget (wealth tax)",
    TRUE     ~ NA_character_
  )
}

budget_control_label <- function(bc) {
  dplyr::case_when(
    bc == 1L ~ "Government control",
    bc == 2L ~ "Protected fund",
    bc == 3L ~ "Protected fund + citizen board",
    TRUE     ~ NA_character_
  )
}

#' Human-readable export for the harmonised (pooled split-sample) package grid.
format_harmonised_package_grid_export <- function(grid) {
  grid %>%
    dplyr::mutate(
      budget_funding        = budget_funding_label(.data$bf),
      budget_control        = budget_control_label(.data$bc),
      household_support_lvl = .data$hh,
      d1_slot               = paste(
        "Pooled over worker_support and community_mobility_support;",
        "trans = level (1 low … 3 high) on whichever d1 half the respondent saw"
      ),
      d1_level_order        = .data$trans,
      d2_slot               = paste(
        "Pooled over information (2 levels) and infrastructure_ownership (3 levels);",
        "d2lev = level on whichever d2 half the respondent saw"
      ),
      d2_level_order        = .data$d2lev
    ) %>%
    dplyr::select(
      dplyr::any_of(c("Country")),
      .data$budget_funding, .data$bf,
      .data$budget_control, .data$bc,
      .data$household_support_lvl, .data$hh,
      .data$d1_slot, .data$d1_level_order, .data$trans,
      .data$d2_slot, .data$d2_level_order, .data$d2lev,
      .data$p_reform_beats_repeal, .data$se, .data$ci_lo, .data$ci_hi,
      .data$n_pairs, .data$n_resp
    )
}

#' Human-readable export with split-sample attributes broken out.
format_split_package_grid_export <- function(grid, level_lookup) {
  grid %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      budget_funding        = budget_funding_label(.data$bf),
      budget_control        = budget_control_label(.data$bc),
      household_support     = level_short_for(
        level_lookup, "household_support", .data$hh
      ),
      d1_attribute          = .data$d1,
      d1_level_order        = .data$trans,
      d1_level              = level_short_for(
        level_lookup, .data$d1, .data$trans
      ),
      d2_attribute          = .data$d2,
      d2_level_order        = .data$d2lev,
      d2_level              = level_short_for(
        level_lookup, .data$d2, .data$d2lev
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      package_summary = paste0(
        .data$budget_funding, "; ", .data$budget_control,
        "; hh=", .data$household_support,
        "; ", .data$d1_attribute, "=", .data$d1_level,
        "; ", .data$d2_attribute, "=", .data$d2_level
      )
    ) %>%
    dplyr::select(
      dplyr::any_of(c("Country")),
      .data$budget_funding, .data$bf,
      .data$budget_control, .data$bc,
      .data$household_support, .data$hh,
      .data$d1_attribute, .data$d1_level_order, .data$d1_level,
      .data$d2_attribute, .data$d2_level_order, .data$d2_level,
      .data$package_summary,
      .data$p_reform_beats_repeal, .data$se, .data$ci_lo, .data$ci_hi,
      .data$n_pairs, .data$n_resp
    )
}

write_package_grid_codebook <- function(path) {
  codebook <- tibble::tribble(
    ~column, ~meaning,
    "bf / budget_funding", "Budget size & funding source (1=carbon only, 2=debt, 3=wealth tax)",
    "bc / budget_control", "Who controls the transition budget (1=gov, 2=protected fund, 3=+ citizen board)",
    "hh / household_support", "Household equipment support level (1 low … 3 high)",
    "trans / d1_level_order", "Level on the d1 split-sample attribute (NOT always workers): worker_support OR community_mobility_support",
    "d2lev / d2_level_order", "Level on the d2 split-sample attribute: information (2 levels) OR infrastructure_ownership (3 levels)",
    "d1_attribute", "Which transition-support half the respondent saw (worker vs community mobility)",
    "d2_attribute", "Which long-term dimension half the respondent saw (information vs infrastructure ownership)",
    "harmonised grid", "package_grid_pooled pools d1 and d2 halves; use package_grid_split_sample for breakdown",
    "corner package", "bc=3, hh=3, trans=3, bf in {2,3}; top packages for paper text in package_corner_cells_split"
  )
  readr::write_csv(codebook, path)
}

PANEL_B_XLAB <- "Prob(reform preferred over repealing EU-ETS2)"

wrap_panel_b_row_label <- function(x, width = 34L) {
  stringr::str_wrap(x, width = width)
}

# ---- Boxplots of the package distribution ----------------------------------------
# Three dimensions: budget control (all three levels, other attributes free);
# scale of support (carbon-only + minimal support vs expanded budget + full
# support, pooling debt and wealth tax); funding mode (public debt vs wealth
# tax, holding the expanded + full-support bundle).

PANEL_B_DIM_LABELS <- list(
  budget_control = "Budget control",
  budget_scale   = "Scale of support",
  funding_mode   = "Funding mode (full support)"
)

PANEL_B_GROUP_LABELS <- list(
  bc1            = "By the government (as general revenue)",
  bc2            = "Protected fund",
  bc3            = "Protected fund with citizen oversight",
  scale_tight    = "Carbon revenue only + minimal support",
  scale_expanded = "Expanded budget + full support",
  fund_debt      = "Public debt",
  fund_wealth    = "Wealth tax"
)

PANEL_B_BOXPLOT_COLOURS <- c(
  "By the government (as general revenue)" = "#636363",
  "Protected fund"                         = "#2166AC",
  "Protected fund with citizen oversight"  = "#1B7837",
  "Carbon revenue only + minimal support"  = "#7570B3",
  "Expanded budget + full support"         = "#D95F02",
  "Public debt"                            = "#E6AB02",
  "Wealth tax"                             = "#A6761D"
)

panel_b_contrast_group_order <- function(panel_row_label, contrast_values) {
  gl <- PANEL_B_GROUP_LABELS
  if (grepl("control", panel_row_label, ignore.case = TRUE)) {
    order <- c(gl$bc1, gl$bc2, gl$bc3)
  } else if (grepl("scale", panel_row_label, ignore.case = TRUE)) {
    order <- c(gl$scale_tight, gl$scale_expanded)
  } else {
    order <- c(gl$fund_debt, gl$fund_wealth)
  }
  order[order %in% contrast_values]
}

format_panel_b_means_xlabel <- function(level, label_width = 18L) {
  if (identical(level, PANEL_B_GROUP_LABELS$bc1)) {
    "By the government\n(as general revenue)"
  } else {
    stringr::str_wrap(level, width = label_width)
  }
}

boxplot_colours_for_groups <- function(groups) {
  cols <- PANEL_B_BOXPLOT_COLOURS[groups]
  cols[is.na(cols)] <- "#999999"
  cols
}

panel_b_boxplot_group_order <- function(panel_row_label, contrast_values) {
  panel_b_contrast_group_order(panel_row_label, contrast_values)
}

assign_panel_b_dimension_groups <- function(d) {
  gl <- PANEL_B_GROUP_LABELS
  rl <- PANEL_B_DIM_LABELS
  bc_data <- d %>%
    dplyr::mutate(
      panel_row = rl$budget_control,
      contrast_group = dplyr::case_when(
        .data$bc == 1L ~ gl$bc1,
        .data$bc == 2L ~ gl$bc2,
        .data$bc == 3L ~ gl$bc3,
        TRUE ~ NA_character_
      )
    )
  scale_data <- d %>%
    dplyr::mutate(
      panel_row = rl$budget_scale,
      contrast_group = dplyr::case_when(
        .data$bf == 1L & .data$hh == 1L & .data$trans == 1L ~ gl$scale_tight,
        .data$bf %in% c(2L, 3L) & .data$hh == 3L & .data$trans == 3L ~
          gl$scale_expanded,
        TRUE ~ NA_character_
      )
    )
  fund_data <- d %>%
    dplyr::mutate(
      panel_row = rl$funding_mode,
      contrast_group = dplyr::case_when(
        .data$bf == 2L & .data$hh == 3L & .data$trans == 3L ~ gl$fund_debt,
        .data$bf == 3L & .data$hh == 3L & .data$trans == 3L ~ gl$fund_wealth,
        TRUE ~ NA_character_
      )
    )
  dplyr::bind_rows(bc_data, scale_data, fund_data) %>%
    dplyr::filter(!is.na(.data$contrast_group)) %>%
    dplyr::mutate(
      panel_row = factor(.data$panel_row, levels = unname(unlist(rl))),
      contrast_group = as.character(.data$contrast_group)
    )
}

build_panel_b_boxplot_data <- function(grid) {
  keep_cols <- intersect(
    c("Country", "bf", "bc", "hh", "trans", "d2lev",
      "p_reform_beats_repeal", "se", "ci_lo", "ci_hi", "n_pairs", "n_resp"),
    names(grid)
  )
  assign_panel_b_dimension_groups(dplyr::select(grid, dplyr::all_of(keep_cols)))
}

summarise_panel_b_boxplot_means <- function(pairs,
                                            level_lookup,
                                            countries = NULL,
                                            facet_country = FALSE) {
  p <- prepare_full_package_pairs(pairs, level_lookup)
  if (!is.null(countries)) {
    p <- dplyr::filter(p, .data$Country %in% .env$countries)
  }
  p <- p %>%
    dplyr::filter(
      !is.na(.data$bf), !is.na(.data$bc), !is.na(.data$hh),
      !is.na(.data$trans), !is.na(.data$d2lev)
    ) %>%
    dplyr::mutate(clust = paste(.data$Country, .data$ID, sep = ":"))

  grouped <- assign_panel_b_dimension_groups(p)
  grp <- c("panel_row", "contrast_group", if (facet_country) "Country")
  grouped %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::group_modify(~ compute_p_reform_beats_repeal(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      mean_p = .data$p_reform_beats_repeal,
      se_mean = .data$se,
      panel_row = factor(
        as.character(.data$panel_row),
        levels = unname(unlist(PANEL_B_DIM_LABELS))
      )
    ) %>%
    dplyr::select(
      .data$panel_row, .data$contrast_group,
      dplyr::any_of("Country"),
      .data$mean_p, .data$se_mean, .data$ci_lo, .data$ci_hi,
      .data$n_pairs, .data$n_resp
    )
}

plot_panel_b_boxplot_one_row <- function(box_data,
                                         mean_pts = NULL,
                                         y_limits = c(0.38, 0.75),
                                         y_label = NULL,
                                         facet_country = FALSE,
                                         country_colours = NULL,
                                         show_y_axis = TRUE,
                                         label_width = 18L) {
  if (facet_country) {
    if (is.null(country_colours)) country_colours <- HEADLINE_COUNTRY_COLOURS
    box_data <- box_data %>%
      dplyr::mutate(Country = factor(.data$Country, levels = names(country_colours)))
  }

  strip_lab <- unique(as.character(box_data$panel_row))[1L]
  group_levels <- panel_b_boxplot_group_order(
    strip_lab, unique(as.character(box_data$contrast_group))
  )
  box_data <- box_data %>%
    dplyr::mutate(contrast_group = factor(.data$contrast_group, levels = group_levels))
  colours <- boxplot_colours_for_groups(group_levels)

  if (!is.null(mean_pts)) {
    mean_pts <- mean_pts %>%
      dplyr::filter(as.character(.data$panel_row) == strip_lab) %>%
      dplyr::mutate(
        contrast_group = factor(.data$contrast_group, levels = group_levels)
      ) %>%
      dplyr::filter(!is.na(.data$contrast_group))
    if (facet_country) {
      mean_pts <- mean_pts %>%
        dplyr::mutate(Country = factor(.data$Country, levels = names(country_colours)))
    }
  }

  p <- ggplot2::ggplot(
    box_data,
    ggplot2::aes(
      x = .data$contrast_group,
      y = .data$p_reform_beats_repeal,
      fill = .data$contrast_group
    )
  ) +
    ggplot2::geom_hline(
      yintercept = 0.5, linetype = "dashed",
      linewidth = 0.35, colour = "grey55"
    ) +
    ggplot2::geom_boxplot(
      width = 0.55, alpha = 0.22, outlier.shape = NA,
      linewidth = 0.4, colour = "grey35",
      median.colour = "grey78", median.linewidth = 0.3
    ) +
    ggplot2::geom_jitter(
      width = 0.12, height = 0, alpha = 0.35, size = 0.85,
      show.legend = FALSE, colour = "grey35"
    )

  if (!is.null(mean_pts) && nrow(mean_pts) > 0L) {
    p <- p +
      ggplot2::geom_errorbar(
        data = mean_pts,
        ggplot2::aes(
          x = .data$contrast_group,
          ymin = .data$ci_lo, ymax = .data$ci_hi
        ),
        inherit.aes = FALSE,
        width = 0.14, linewidth = 0.55, colour = "grey20"
      ) +
      ggplot2::geom_point(
        data = mean_pts,
        ggplot2::aes(x = .data$contrast_group, y = .data$mean_p),
        inherit.aes = FALSE,
        shape = 23, size = 2.2, fill = "white", colour = "grey20", stroke = 0.65
      )
  }

  p <- p +
    ggplot2::scale_fill_manual(values = colours, drop = TRUE, guide = "none") +
    ggplot2::scale_x_discrete(
      labels = function(x) vapply(x, format_panel_b_means_xlabel, character(1L),
                                  label_width = label_width)
    ) +
    ggplot2::scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = y_limits,
      breaks = seq(ceiling(y_limits[1] * 20) / 20,
                   floor(y_limits[2] * 20) / 20, by = 0.05)
    ) +
    ggplot2::labs(x = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.x        = ggplot2::element_text(size = 6.5, lineheight = 0.85),
      axis.text.y        = ggplot2::element_text(size = 8)
    )

  if (facet_country) {
    p <- p + ggplot2::facet_wrap(~ Country, ncol = 2L)
  } else {
    p <- p + ggplot2::labs(title = stringr::str_wrap(strip_lab, width = 22)) +
      ggplot2::theme(
        plot.title = ggplot2::element_text(
          size = 8, face = "bold", hjust = 0.5,
          margin = ggplot2::margin(0, 0, 2, 0, "pt")
        )
      )
  }

  if (!show_y_axis) {
    p <- p + ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )
  } else if (!is.null(y_label)) {
    p <- p + ggplot2::labs(y = y_label)
  }
  p
}

plot_panel_b_boxplot_row_plots <- function(box_data,
                                           mean_pts = NULL,
                                           y_limits = c(0.38, 0.75),
                                           y_label = PANEL_B_XLAB,
                                           facet_country = FALSE,
                                           country_colours = NULL) {
  row_levels <- levels(box_data$panel_row)
  if (is.null(row_levels)) {
    row_levels <- unique(as.character(box_data$panel_row))
  }
  purrr::imap(row_levels, function(r, i) {
    dr <- dplyr::filter(box_data, .data$panel_row == r)
    plot_panel_b_boxplot_one_row(
      dr,
      mean_pts = mean_pts,
      y_limits = y_limits,
      y_label = if (i == 1L) y_label else NULL,
      facet_country = facet_country,
      country_colours = country_colours,
      show_y_axis = i == 1L
    )
  })
}

plot_panel_b_boxplot_panel <- function(box_data,
                                       mean_pts = NULL,
                                       y_limits = c(0.38, 0.75),
                                       y_label = PANEL_B_XLAB,
                                       facet_country = FALSE,
                                       country_colours = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("Package 'cowplot' is required.", call. = FALSE)
  }
  plots <- plot_panel_b_boxplot_row_plots(
    box_data, mean_pts = mean_pts,
    y_limits = y_limits, y_label = y_label,
    facet_country = facet_country, country_colours = country_colours
  )
  if (length(plots) == 1L) {
    return(plots[[1L]])
  }
  cowplot::plot_grid(
    plotlist = plots, ncol = length(plots), align = "h", axis = "tb",
    rel_widths = if (length(plots) == 3L) c(1.25, 1, 0.85) else rep(1, length(plots))
  )
}

#' Reform-reform ACP panel with attributes ordered by importance.
#'
#' @param importance_tab Optional tidy table from tidy_acp_variability*() with
#'   columns attribute, variability, var_ci_lo, var_ci_hi. When supplied
#'   (vertical layout), facet strips show Ganter importance in pp with CIs.
plot_acp_reform_reform_panel <- function(plot_data,
                                         country = NULL,
                                         attribute_order = NULL,
                                         attribute_labels = PAPER_ATTR_LABELS,
                                         layout = c("vertical", "horizontal"),
                                         importance_tab = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  layout <- match.arg(layout)
  use_importance <- !is.null(importance_tab) && nrow(importance_tab) > 0L &&
    layout == "vertical"
  if (use_importance && !requireNamespace("ggtext", quietly = TRUE)) {
    stop("Package 'ggtext' is required for importance facet labels.", call. = FALSE)
  }

  d <- plot_data %>%
    dplyr::filter(!is.na(.data$attribute_label))
  if (!is.null(country)) {
    d <- dplyr::filter(d, .data$Country == .env$country)
  }
  if (is.null(attribute_order)) {
    attribute_order <- rank_attributes_by_acp_spread(d)$attribute
  }
  attribute_order <- as.character(attribute_order)

  d <- d %>%
    dplyr::mutate(
      attribute_label = unname(attribute_labels[as.character(.data$attribute)])
    )

  if (use_importance) {
    facet_titles <- vapply(attribute_order, function(a) {
      facet_title_with_importance(
        a,
        importance_tab[as.character(importance_tab$attribute) == a, , drop = FALSE],
        attribute_labels
      )
    }, character(1))
    names(facet_titles) <- attribute_order
    d <- d %>%
      dplyr::mutate(
        facet_lab = unname(facet_titles[as.character(.data$attribute)])
      )
    facet_levels <- unname(facet_titles[attribute_order])
  } else {
    attr_label_order <- unname(attribute_labels[attribute_order])
    if (layout == "horizontal") {
      attr_label_order <- wrap_panel_b_row_label(attr_label_order, width = 16L)
    }
    d <- d %>%
      dplyr::mutate(
        facet_lab = .data$attribute_label
      )
    facet_levels <- attr_label_order
  }

  d <- d %>%
    dplyr::group_by(.data$attribute) %>%
    dplyr::mutate(
      level = factor(
        .data$level,
        levels = unique(.data$level[order(.data$level_order)])
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      facet_lab = factor(.data$facet_lab, levels = facet_levels)
    )

  horizontal <- layout == "horizontal"
  p <- ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$level)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$ci_lo, xmax = .data$ci_hi),
      orientation = "y", width = 0.2, linewidth = 0.4, colour = "#3C5488FF"
    ) +
    ggplot2::geom_point(size = if (horizontal) 1.8 else 2.5, colour = "#3C5488FF")

  if (horizontal) {
    p <- p +
      ggplot2::facet_wrap(
        ~ facet_lab, nrow = 1L, scales = "free_y", strip.position = "top"
      ) +
      ggplot2::labs(caption = PANEL_A_XLAB, y = NULL, x = NULL) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.minor     = ggplot2::element_blank(),
        panel.grid.major.y   = ggplot2::element_blank(),
        strip.text           = ggplot2::element_text(
          size = 6.5, face = "bold", lineheight = 0.85
        ),
        strip.background     = ggplot2::element_rect(fill = "grey95"),
        axis.text.y          = ggplot2::element_text(size = 5.5),
        axis.text.x          = ggplot2::element_text(size = 6.5),
        axis.title.x         = ggplot2::element_blank(),
        panel.spacing.x      = ggplot2::unit(0.35, "lines"),
        plot.caption         = ggplot2::element_text(size = 8, hjust = 0.5),
        plot.caption.position = "plot",
        plot.margin          = ggplot2::margin(2, 4, 2, 4, "pt")
      )
  } else {
    strip_text <- if (use_importance) {
      ggtext::element_markdown(
        size = 8.5, face = "plain", hjust = 0, lineheight = 1.15
      )
    } else {
      ggplot2::element_text(size = 8.5, face = "bold")
    }
    p <- p +
      ggplot2::facet_wrap(~ facet_lab, ncol = 1, scales = "free_y") +
      ggplot2::labs(x = PANEL_A_XLAB, y = NULL) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        panel.grid.minor   = ggplot2::element_blank(),
        panel.grid.major.y = ggplot2::element_blank(),
        strip.text         = strip_text,
        strip.background   = ggplot2::element_rect(fill = "grey95"),
        axis.text.y        = ggplot2::element_text(size = 7.5),
        axis.text.x        = ggplot2::element_text(size = 8)
      )
  }
  p
}

plot_acp_reform_reform_countries_grid <- function(plot_data,
                                                   countries,
                                                   attribute_order,
                                                   ncol = 2L) {
  if (!requireNamespace("cowplot", quietly = TRUE)) {
    stop("Package 'cowplot' is required.", call. = FALSE)
  }
  plots <- purrr::map(countries, function(co) {
    plot_acp_reform_reform_panel(
      plot_data, country = co, attribute_order = attribute_order
    ) +
      ggplot2::labs(title = co) +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 9, face = "bold"))
  })
  cowplot::plot_grid(plotlist = plots, ncol = ncol, align = "hv")
}

#' Compact two-line labels for the 4-country page (narrow left strip).
A4_ATTR_LABELS <- c(
  budget_and_funding         = "Budget &\nfunding",
  budget_control             = "Budget\ncontrol",
  household_support          = "Household\nsupport",
  information                = "Information",
  infrastructure_ownership   = "Infrastructure",
  worker_support             = "Worker\nsupport",
  community_mobility_support = "Community\nmobility"
)

#' A4-portrait country ACP: one ggplot, attributes × countries.
#'
#' Shared x-scale and attribute order (from pooled ACP). No importance strips.
#' Y-ids are unique within attribute so level order is not a global fct_reorder.

plot_acp_reform_reform_countries_a4 <- function(plot_data,
                                                countries,
                                                attribute_order,
                                                attribute_labels = A4_ATTR_LABELS) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  attribute_order <- as.character(attribute_order)
  attr_lab_map <- unname(attribute_labels[attribute_order])
  names(attr_lab_map) <- attribute_order

  d <- plot_data %>%
    dplyr::filter(
      .data$Country %in% .env$countries,
      as.character(.data$attribute) %in% .env$attribute_order
    ) %>%
    dplyr::mutate(
      Country = factor(.data$Country, levels = .env$countries),
      attribute = factor(
        as.character(.data$attribute),
        levels = .env$attribute_order
      ),
      attribute_lab = factor(
        unname(attr_lab_map[as.character(.data$attribute)]),
        levels = unname(attr_lab_map)
      ),
      y_id = paste(
        as.character(.data$attribute),
        sprintf("%02d", as.integer(.data$level_order)),
        .data$level,
        sep = "|||"
      )
    ) %>%
    dplyr::arrange(.data$attribute, .data$level_order, .data$level) %>%
    dplyr::mutate(y_id = factor(.data$y_id, levels = unique(.data$y_id)))

  if (!nrow(d)) {
    stop("No country ACP rows to plot.", call. = FALSE)
  }

  x_rng <- range(c(d$ci_lo, d$ci_hi), na.rm = TRUE)
  pad <- max(0.01, 0.04 * diff(x_rng))
  y_lab <- function(x) sub("^.*\\|\\|\\|", "", x)

  ggplot2::ggplot(d, ggplot2::aes(x = .data$estimate, y = .data$y_id)) +
    ggplot2::geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey40") +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$ci_lo, xmax = .data$ci_hi),
      orientation = "y", width = 0.35, linewidth = 0.35, colour = "#3C5488FF"
    ) +
    ggplot2::geom_point(size = 1.4, colour = "#3C5488FF") +
    ggplot2::scale_y_discrete(
      labels = y_lab,
      expand = ggplot2::expansion(mult = 0.04, add = 0.12)
    ) +
    ggplot2::coord_cartesian(xlim = c(x_rng[1] - pad, x_rng[2] + pad)) +
    ggplot2::facet_grid(
      attribute_lab ~ Country,
      scales = "free_y",
      space = "free_y",
      switch = "y"
    ) +
    ggplot2::labs(x = PANEL_A_XLAB, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text.x       = ggplot2::element_text(size = 9, face = "bold"),
      strip.text.y.left  = ggplot2::element_text(
        size = 6.5, face = "bold", angle = 0, hjust = 1, vjust = 0.5,
        lineheight = 0.85, margin = ggplot2::margin(r = 3.5, l = 1.5)
      ),
      strip.background   = ggplot2::element_rect(fill = "grey95"),
      strip.placement    = "outside",
      strip.switch.pad.grid = ggplot2::unit(0.05, "cm"),
      strip.clip         = "off",
      axis.text.y        = ggplot2::element_text(size = 6),
      axis.text.x        = ggplot2::element_text(size = 7),
      axis.title.x       = ggplot2::element_text(size = 8),
      axis.ticks.y       = ggplot2::element_blank(),
      panel.spacing.x    = ggplot2::unit(0.35, "lines"),
      panel.spacing.y    = ggplot2::unit(0.12, "lines"),
      plot.margin        = ggplot2::margin(4, 6, 4, 4, "mm")
    )
}

#' Three-panel boxplot of package-level P(reform beats repeal).
save_aux_p_reform_beats_repeal_figures <- function(pairs_reform_repeal,
                                                   level_lookup,
                                                   countries = NULL,
                                                   exclude_repeal_always_first = FALSE,
                                                   exclude_repeal_always_last = FALSE,
                                                   file_suffix = "",
                                                   min_n = 40L,
                                                   panel_b_x_limits = c(0.38, 0.75),
                                                   out_dir = "figures/acp") {
  if (is.null(countries)) {
    countries <- sort(unique(pairs_reform_repeal$Country))
  }
  flag_suffix <- paste0(
    acp_figure_flag_suffix(
      exclude_repeal_always_first = exclude_repeal_always_first,
      exclude_repeal_always_last  = exclude_repeal_always_last
    ),
    file_suffix
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  grid_split_pooled <- summarise_package_grid_split_vs_repeal(
    pairs_reform_repeal, level_lookup,
    pool_countries = TRUE, min_n = min_n
  )
  bp_data <- build_panel_b_boxplot_data(grid_split_pooled)
  bp_means <- summarise_panel_b_boxplot_means(
    pairs_reform_repeal, level_lookup, countries = countries
  )
  readr::write_csv(
    bp_data,
    file.path(out_dir, paste0("aux_p_reform_beats_repeal_boxplot_data_",
                             flag_suffix, ".csv"))
  )
  readr::write_csv(
    bp_means,
    file.path(out_dir, paste0("aux_p_reform_beats_repeal_boxplot_means_",
                             flag_suffix, ".csv"))
  )
  pb_box <- plot_panel_b_boxplot_panel(
    bp_data, mean_pts = bp_means, y_limits = panel_b_x_limits
  )
  path_box <- file.path(
    out_dir, paste0("aux_p_reform_beats_repeal_boxplot_", flag_suffix, ".pdf")
  )
  ggplot2::ggsave(path_box, pb_box, width = 240 / 25.4, height = 95 / 25.4)
  message("Saved ", path_box)

  invisible(list(boxplot_data = bp_data, boxplot_means = bp_means))
}

# ---- P(reform beats repeal) from reform–repeal pairs ------------------------------

#' Join harmonised level_order / level_short onto reform profile (_1) columns.
add_reform_level_order <- function(pairs, attribute, level_lookup) {
  level_col <- paste0(attribute, "_1")
  order_col <- paste0(attribute, "_order_1")
  short_col <- paste0(attribute, "_short_1")
  if (!level_col %in% names(pairs)) {
    stop("Missing column ", level_col, " on pairs.", call. = FALSE)
  }
  if (order_col %in% names(pairs)) {
    return(pairs)
  }
  lk <- level_lookup %>%
    dplyr::filter(.data$attribute == .env$attribute) %>%
    dplyr::distinct(.data$Country, .data$level, .data$level_order, .data$level_short)
  pairs %>%
    dplyr::left_join(
      lk %>%
        dplyr::rename(
          !!level_col  := .data$level,
          !!order_col  := .data$level_order,
          !!short_col  := .data$level_short
        ),
      by = c("Country", level_col)
    )
}

#' Cluster-robust P(reform rank beats repeal) = mean(outcome) on a pair subset.
compute_p_reform_beats_repeal <- function(pairs) {
  if (!nrow(pairs)) {
    return(tibble::tibble(
      p_reform_beats_repeal = NA_real_,
      se                    = NA_real_,
      ci_lo                 = NA_real_,
      ci_hi                 = NA_real_,
      n_pairs               = 0L,
      n_resp                = 0L
    ))
  }
  m   <- stats::lm(outcome ~ 1, data = pairs)
  p   <- mean(pairs$outcome)
  se  <- sqrt(sandwich::vcovCL(m, cluster = pairs$clust)[1, 1])
  tibble::tibble(
    p_reform_beats_repeal = p,
    se                    = se,
    ci_lo                 = p - 1.96 * se,
    ci_hi                 = p + 1.96 * se,
    n_pairs               = nrow(pairs),
    n_resp                = dplyr::n_distinct(pairs$ID)
  )
}

#' P(reform beats repeal) by reform level of one attribute (other attrs vary in design).
summarise_p_reform_beats_repeal_by_level <- function(pairs,
                                                    level_lookup,
                                                    attribute,
                                                    countries = NULL) {
  if (!is.null(countries)) {
    pairs <- pairs %>% dplyr::filter(.data$Country %in% .env$countries)
  }
  order_col <- paste0(attribute, "_order_1")
  short_col <- paste0(attribute, "_short_1")
  pairs %>%
    add_reform_level_order(attribute, level_lookup) %>%
    dplyr::filter(!is.na(.data[[order_col]])) %>%
    dplyr::group_by(.data$Country, .data[[order_col]], .data[[short_col]]) %>%
    dplyr::group_modify(~ compute_p_reform_beats_repeal(.x)) %>%
    dplyr::ungroup() %>%
    dplyr::transmute(
      Country,
      scenario_type = "marginal",
      scenario      = paste0(.data[[short_col]], " (", .env$attribute, ")"),
      attribute     = .env$attribute,
      level_order   = .data[[order_col]],
      level         = .data[[short_col]],
      d1            = NA_character_,
      p_reform_beats_repeal,
      se,
      ci_lo,
      ci_hi,
      n_pairs,
      n_resp
    )
}

#' Named package scenarios for P(reform beats repeal).
summarise_p_reform_beats_repeal_packages <- function(pairs,
                                                     level_lookup,
                                                     countries = NULL) {
  if (!is.null(countries)) {
    pairs <- pairs %>% dplyr::filter(.data$Country %in% .env$countries)
  }
  pkg_attrs <- c(
    "budget_and_funding", "budget_control", "household_support",
    "worker_support", "community_mobility_support"
  )
  for (attr in pkg_attrs) {
    pairs <- add_reform_level_order(pairs, attr, level_lookup)
  }

  # Low budget (carbon revenue only) + low household + low worker/mobility (validCombos Low).
  tight <- pairs %>%
    dplyr::filter(
      .data$budget_and_funding_order_1 == 1L,
      .data$household_support_order_1 == 1L,
      (.data$d1 == "worker_support" & .data$worker_support_order_1 == 1L) |
        (.data$d1 == "community_mobility_support" &
           .data$community_mobility_support_order_1 == 1L)
    )

  # Wealth-tax expanded budget + high household + high worker/mobility (validCombos High).
  expanded <- pairs %>%
    dplyr::filter(
      .data$budget_and_funding_order_1 == 3L,
      .data$household_support_order_1 == 3L,
      (.data$d1 == "worker_support" & .data$worker_support_order_1 == 3L) |
        (.data$d1 == "community_mobility_support" &
           .data$community_mobility_support_order_1 == 3L)
    )

  dplyr::bind_rows(
    tight %>%
      dplyr::group_by(.data$Country, .data$d1) %>%
      dplyr::group_modify(~ compute_p_reform_beats_repeal(.x)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        scenario_type = "package",
        scenario      = "Tight: carbon revenue + low support (hh & worker/mobility)",
        attribute     = NA_character_,
        level_order   = NA_integer_,
        level         = NA_character_
      ),
    expanded %>%
      dplyr::group_by(.data$Country, .data$d1) %>%
      dplyr::group_modify(~ compute_p_reform_beats_repeal(.x)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        scenario_type = "package",
        scenario      = "Expanded: wealth tax + high support (hh & worker/mobility)",
        attribute     = NA_character_,
        level_order   = NA_integer_,
        level         = NA_character_
      )
  ) %>%
    dplyr::select(
      Country, scenario_type, scenario, attribute, level_order, level, d1,
      p_reform_beats_repeal, se, ci_lo, ci_hi, n_pairs, n_resp
    )
}

#' Budget-control marginals + named package scenarios.
summarise_p_reform_beats_repeal_main <- function(pairs,
                                                 level_lookup,
                                                 countries = NULL) {
  dplyr::bind_rows(
    summarise_p_reform_beats_repeal_by_level(
      pairs, level_lookup, "budget_control", countries = countries
    ),
    summarise_p_reform_beats_repeal_packages(pairs, level_lookup, countries = countries)
  )
}

#' Flat summary: country, attribute, level, estimate, se, 95% CI.
summarise_acp_results <- function(acp_results,
                                  drop_na = TRUE,
                                  drop_repeal_level = FALSE) {
  rows <- list()
  for (country in names(acp_results)) {
    for (attr in names(acp_results[[country]])) {
      res <- acp_results[[country]][[attr]]
      if (is.null(res)) next
      est <- res$estimates
      se  <- sqrt(diag(res$vcov))
      level <- sub(paste0("^", attr, "\\."), "", names(est))
      rows[[length(rows) + 1L]] <- tibble::tibble(
        Country   = country,
        attribute = attr,
        level     = level,
        estimate  = as.numeric(est),
        se        = as.numeric(se),
        ci_lo     = estimate - 1.96 * se,
        ci_hi     = estimate + 1.96 * se
      )
    }
  }
  out <- dplyr::bind_rows(rows) %>%
    dplyr::mutate(contrast = paste0(.data$attribute, ".", .data$level))
  if (drop_repeal_level) {
    out <- out %>% dplyr::filter(.data$level != "Repeal")
  }
  if (drop_na) {
    out <- out %>%
      dplyr::filter(
        !is.na(.data$estimate),
        !is.na(.data$level),
        .data$level != "NA"
      )
  }
  out
}

#' Tidy ACP table for the SI: estimate, cluster-robust SE, 95% CI.
#'
#' `level_order` is the design index of the level (1 = first listed level of
#' that attribute). It is used only to sort rows; it is not an estimate.
tidy_acp_si_table <- function(acp_results, level_lookup, pooled = NULL) {
  levels_h <- level_lookup %>%
    dplyr::group_by(.data$attribute, .data$level_short) %>%
    dplyr::summarise(
      level_order = min(.data$level_order, na.rm = TRUE),
      .groups = "drop"
    )
  bind_one <- function(res) {
    summarise_acp_results(res, drop_repeal_level = FALSE) %>%
      dplyr::left_join(
        levels_h,
        by = c("attribute", "level" = "level_short")
      ) %>%
      dplyr::mutate(
        attribute_label = unname(PAPER_ATTR_LABELS[as.character(.data$attribute)]),
        attribute = factor(.data$attribute, levels = CONJOINT_ATTR_COLS)
      )
  }
  out <- bind_one(acp_results)
  if (!is.null(pooled)) {
    out <- dplyr::bind_rows(bind_one(pooled), out)
  }
  country_levels <- unique(c("Pooled", as.character(out$Country)))
  out %>%
    dplyr::mutate(Country = factor(.data$Country, levels = country_levels)) %>%
    dplyr::arrange(.data$Country, .data$attribute, .data$level_order, .data$level) %>%
    dplyr::select(
      Country, attribute, attribute_label, level, level_order,
      estimate, se, ci_lo, ci_hi
    )
}

latex_escape <- function(x) {
  x <- as.character(x)
  x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
  x <- gsub("&", "\\&", x, fixed = TRUE)
  x <- gsub("%", "\\%", x, fixed = TRUE)
  x <- gsub("$", "\\$", x, fixed = TRUE)
  x <- gsub("#", "\\#", x, fixed = TRUE)
  x <- gsub("_", "\\_", x, fixed = TRUE)
  x
}

acp_si_tex_slug <- function(country) {
  gsub("[^a-z0-9]+", "_", tolower(as.character(country)))
}

format_acp_si_tabular <- function(d) {
  d <- d %>%
    dplyr::arrange(.data$attribute, .data$level_order, .data$level)
  attr_lab <- as.character(d$attribute_label)
  show_attr <- attr_lab != dplyr::lag(attr_lab, default = "")
  body <- sprintf(
    "    %s & %s & %.3f & %.3f & [%.3f, %.3f] \\\\",
    ifelse(show_attr, latex_escape(attr_lab), ""),
    latex_escape(d$level),
    d$estimate, d$se, d$ci_lo, d$ci_hi
  )
  attr_id <- as.integer(d$attribute)
  new_attr <- which(c(FALSE, attr_id[-1] != attr_id[-length(attr_id)]))
  if (length(new_attr)) {
    for (i in rev(new_attr)) {
      body <- append(body, "    \\midrule", after = i - 1L)
    }
  }
  c(
    "\\begin{tabular}{llrrr}",
    "  \\toprule",
    "  Attribute & Level & ACP & SE & 95\\% CI \\\\",
    "  \\midrule",
    body,
    "  \\bottomrule",
    "\\end{tabular}"
  )
}

#' One booktabs table per sample (pooled, then each country).
write_acp_si_tex_tables <- function(tab, out_dir = "output/acp") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  groups <- if (is.factor(tab$Country)) {
    intersect(levels(tab$Country), unique(as.character(tab$Country)))
  } else {
    unique(as.character(tab$Country))
  }
  paths <- character()
  for (co in groups) {
    d <- dplyr::filter(tab, as.character(.data$Country) == .env$co)
    slug <- acp_si_tex_slug(co)
    who <- if (identical(co, "Pooled")) "pooled sample" else co
    caption <- paste0(
      "Average component preference (Ganter ACP) from reform--reform comparisons, ",
      who,
      ". Cluster-robust standard errors (respondent). 95\\% confidence intervals. ",
      "The ACP of a level is the average of its direct pairwise preferences (DPPs) ",
      "against the other levels of that attribute; levels of an attribute sum to 0."
    )
    lines <- c(
      "% Requires \\usepackage{booktabs}",
      "\\begin{table}[htbp]",
      "  \\centering",
      paste0("  \\caption{", caption, "}"),
      paste0("  \\label{tab:acp-", slug, "}"),
      paste0("  ", format_acp_si_tabular(d)),
      "\\end{table}"
    )
    path <- file.path(out_dir, paste0("acp_reform_reform_", slug, ".tex"))
    writeLines(lines, path)
    message("Saved ", path)
    paths <- c(paths, path)
  }
  invisible(paths)
}

parse_v_contrast_indices <- function(nm, attribute) {
  nm <- gsub("`", "", nm, fixed = TRUE)
  rest <- sub(paste0("^", attribute, "\\."), "", nm)
  parts <- strsplit(rest, "-", fixed = TRUE)[[1]]
  as.integer(parts[seq_len(2L)])
}

#' DPP for one attribute: lm of pair outcome on Ganter V columns (same as
#' conjacp estimand = "p"). Cluster-robust SEs.
estimate_dpp_from_one_v_table <- function(vt, attribute, level_lookup,
                                         cluster_ids) {
  meta <- v_table_meta_cols()
  v_cols <- setdiff(names(vt), meta)
  if (!length(v_cols) || !nrow(vt)) {
    return(NULL)
  }
  dat <- data.frame(
    outcome = vt$outcome,
    vt[, v_cols, drop = FALSE],
    check.names = FALSE
  )
  m <- stats::lm(outcome ~ ., data = dat)
  V <- sandwich::vcovCL(m, cluster = cluster_ids)
  keep <- setdiff(names(stats::coef(m)), "(Intercept)")
  est <- stats::coef(m)[keep]
  se <- sqrt(diag(V[keep, keep, drop = FALSE]))
  labels <- build_attr_levels_for_attribute(attribute, level_lookup)
  idx <- lapply(keep, parse_v_contrast_indices, attribute = attribute)
  tibble::tibble(
    attribute = attribute,
    level_a   = labels[vapply(idx, `[[`, integer(1), 1L)],
    level_b   = labels[vapply(idx, `[[`, integer(1), 2L)],
    estimate  = as.numeric(est),
    se        = as.numeric(se),
    ci_lo     = as.numeric(est) - 1.96 * as.numeric(se),
    ci_hi     = as.numeric(est) + 1.96 * as.numeric(se)
  )
}

#' Pooled + by-country DPP table from reform–reform V tables.
build_dpp_results_table <- function(v_tables, level_lookup, countries) {
  one <- function(country_label, vt, cluster_ids) {
    purrr::imap_dfr(v_tables, function(tbl, attr) {
      tbl <- vt(tbl)
      if (!nrow(tbl)) {
        return(NULL)
      }
      estimate_dpp_from_one_v_table(
        tbl, attr, level_lookup, cluster_ids(tbl)
      ) %>%
        dplyr::mutate(Country = country_label)
    })
  }
  pooled <- one(
    "Pooled",
    function(tbl) tbl,
    function(tbl) paste(tbl$Country, tbl$ID, sep = ":")
  )
  by_co <- purrr::map_dfr(countries, function(co) {
    one(
      co,
      function(tbl) dplyr::filter(tbl, .data$Country == .env$co),
      function(tbl) tbl$clust
    )
  })
  dplyr::bind_rows(pooled, by_co)
}

# ---- Subgroup-vs-rest DPP (interaction regressions) --------------------------
#' DPP contrasts for a subgroup vs the rest, for ONE attribute.
#'
#' Fits lm(outcome ~ V * group) where V are the Ganter reform–reform contrast
#' columns and `group` is a 0/1 indicator (1 = subgroup). This is the DPP
#' (estimand = "p") estimated jointly for both groups:
#'   V coefficient        = contrast among the rest (group 0)
#'   V:group coefficient  = difference in the contrast (subgroup − rest)  [the test]
#'   V + V:group          = contrast within the subgroup
#' SEs are respondent-cluster-robust (same as estimate_dpp_from_one_v_table).
estimate_dpp_subgroup_diff_from_v_table <- function(vt, attribute, level_lookup,
                                                    cluster_ids) {
  meta   <- v_table_meta_cols()
  v_cols <- setdiff(names(vt), c(meta, "group"))
  if (!length(v_cols) || !nrow(vt) ||
      length(unique(stats::na.omit(vt$group))) < 2L) {
    return(NULL)
  }
  Xv <- as.matrix(vt[, v_cols, drop = FALSE])
  g  <- as.numeric(vt$group)
  Xi <- Xv * g
  k  <- ncol(Xv)
  colnames(Xv) <- paste0("v", seq_len(k))
  colnames(Xi) <- paste0("d", seq_len(k))
  dat <- data.frame(outcome = vt$outcome, Xv, g = g, Xi, check.names = FALSE)
  m <- stats::lm(outcome ~ ., data = dat)
  V <- sandwich::vcovCL(m, cluster = cluster_ids)
  cf <- stats::coef(m)

  labels <- build_attr_levels_for_attribute(attribute, level_lookup)
  idx <- lapply(v_cols, parse_v_contrast_indices, attribute = attribute)

  lc <- function(a, b = NULL) {
    # estimate/se for coef `a`, or the linear combo a + b
    if (is.na(cf[a]) || !(a %in% rownames(V))) return(c(NA_real_, NA_real_))
    if (is.null(b)) return(c(cf[a], sqrt(V[a, a])))
    if (is.na(cf[b]) || !(b %in% rownames(V))) return(c(NA_real_, NA_real_))
    c(cf[a] + cf[b], sqrt(V[a, a] + V[b, b] + 2 * V[a, b]))
  }

  purrr::imap_dfr(v_cols, function(nm, j) {
    va <- paste0("v", j); da <- paste0("d", j)
    rest <- lc(va)
    diff <- lc(da)
    sub  <- lc(va, da)
    p_diff <- if (is.na(diff[2]) || diff[2] == 0) NA_real_ else
      2 * stats::pnorm(-abs(diff[1] / diff[2]))
    tibble::tibble(
      attribute = attribute,
      level_a   = labels[idx[[j]][1]],
      level_b   = labels[idx[[j]][2]],
      est_rest  = rest[1], se_rest = rest[2],
      est_sub   = sub[1],  se_sub  = sub[2],
      est_diff  = diff[1], se_diff = diff[2],
      p_diff    = p_diff,
      ci_diff_lo = diff[1] - 1.96 * diff[2],
      ci_diff_hi = diff[1] + 1.96 * diff[2]
    )
  })
}

#' Pooled + by-country subgroup-vs-rest DPP for one attribute.
#'
#' @param subgroup_tbl respondent-level tibble with Country, ID, group (logical).
#' @param na_as_rest    if TRUE, group = NA is folded into the rest (0);
#'                      if FALSE (default) NA respondents are dropped (so the
#'                      comparison is subgroup vs the complementary known group).
build_dpp_subgroup_diff_table <- function(pairs_reform_reform, attribute,
                                          subgroup_tbl, level_lookup, countries,
                                          na_as_rest = FALSE) {
  vt <- build_v_table_reform_reform(pairs_reform_reform, attribute, level_lookup) %>%
    dplyr::left_join(subgroup_tbl, by = c("Country", "ID")) %>%
    dplyr::mutate(group = dplyr::case_when(
      .data$group %in% TRUE  ~ 1,
      .data$group %in% FALSE ~ 0,
      na_as_rest             ~ 0,
      TRUE                   ~ NA_real_
    ))
  if (!na_as_rest) vt <- dplyr::filter(vt, !is.na(.data$group))
  if (!nrow(vt)) return(NULL)

  n_tab <- vt %>% dplyr::distinct(.data$Country, .data$ID, .data$group)
  attr_n <- function(co) {
    d <- if (identical(co, "Pooled")) n_tab else dplyr::filter(n_tab, .data$Country == co)
    list(n_sub = sum(d$group == 1, na.rm = TRUE),
         n_rest = sum(d$group == 0, na.rm = TRUE))
  }

  pooled <- estimate_dpp_subgroup_diff_from_v_table(
    vt, attribute, level_lookup,
    cluster_ids = paste(vt$Country, vt$ID, sep = ":")
  )
  if (!is.null(pooled)) {
    nn <- attr_n("Pooled")
    pooled <- dplyr::mutate(pooled, Country = "Pooled",
                            n_sub = nn$n_sub, n_rest = nn$n_rest)
  }
  by_co <- purrr::map_dfr(countries, function(co) {
    s <- dplyr::filter(vt, .data$Country == co)
    r <- estimate_dpp_subgroup_diff_from_v_table(s, attribute, level_lookup,
                                                 cluster_ids = s$clust)
    if (is.null(r)) return(NULL)
    nn <- attr_n(co)
    dplyr::mutate(r, Country = co, n_sub = nn$n_sub, n_rest = nn$n_rest)
  })
  dplyr::bind_rows(pooled, by_co) %>%
    dplyr::relocate("Country")
}

#' Run subgroup-vs-rest DPP for several (attribute, covariate) specs and stack.
#' @param specs list of lists, each: attribute, group_col, na_as_rest, label.
build_dpp_subgroup_diff_all <- function(pairs_reform_reform, respondent_covs,
                                        specs, level_lookup, countries) {
  purrr::map_dfr(specs, function(sp) {
    sg <- respondent_covs %>%
      dplyr::transmute(.data$Country, .data$ID, group = .data[[sp$group_col]])
    build_dpp_subgroup_diff_table(
      pairs_reform_reform, sp$attribute, sg, level_lookup, countries,
      na_as_rest = isTRUE(sp$na_as_rest)
    ) %>%
      dplyr::mutate(subgroup = sp$label, group_col = sp$group_col)
  }) %>%
    dplyr::relocate("subgroup", "group_col")
}

# ---- Subgroup-vs-rest difference in P(reform > repeal) ------------------------
#' Difference in overall reform support between a subgroup and the rest.
#' lm(outcome ~ group) on reform–repeal pairs; respondent-cluster-robust SEs.
#'   (Intercept)      = P(reform > repeal) among the rest
#'   group            = difference (subgroup - rest)  [the test]
#'   Intercept+group  = P(reform > repeal) within the subgroup
estimate_p_reform_diff_one <- function(pairs, cluster_ids) {
  if (!nrow(pairs) || length(unique(stats::na.omit(pairs$group))) < 2L) {
    return(NULL)
  }
  m <- stats::lm(outcome ~ group, data = pairs)
  V <- sandwich::vcovCL(m, cluster = cluster_ids)
  b <- stats::coef(m)
  se <- sqrt(diag(V))
  diff <- unname(b["group"]); se_d <- unname(se["group"])
  tibble::tibble(
    p_rest  = unname(b["(Intercept)"]),
    p_sub   = unname(b["(Intercept)"] + b["group"]),
    diff    = diff, se_diff = se_d,
    p_value = 2 * stats::pnorm(-abs(diff / se_d)),
    ci_lo   = diff - 1.96 * se_d, ci_hi = diff + 1.96 * se_d
  )
}

#' Pooled + by-country subgroup-vs-rest difference in P(reform > repeal).
build_p_reform_diff_table <- function(pairs_reform_repeal, subgroup_tbl,
                                      countries, na_as_rest = FALSE) {
  p <- pairs_reform_repeal %>%
    dplyr::left_join(subgroup_tbl, by = c("Country", "ID")) %>%
    dplyr::mutate(group = dplyr::case_when(
      .data$group %in% TRUE  ~ 1,
      .data$group %in% FALSE ~ 0,
      na_as_rest             ~ 0,
      TRUE                   ~ NA_real_
    ))
  if (!na_as_rest) p <- dplyr::filter(p, !is.na(.data$group))
  if (!nrow(p)) return(NULL)

  n_tab <- p %>% dplyr::distinct(.data$Country, .data$ID, .data$group)
  n_for <- function(co, g) {
    d <- if (identical(co, "Pooled")) n_tab else dplyr::filter(n_tab, .data$Country == co)
    sum(d$group == g, na.rm = TRUE)
  }
  pooled <- estimate_p_reform_diff_one(
    p, cluster_ids = paste(p$Country, p$ID, sep = ":")
  )
  if (!is.null(pooled)) {
    pooled <- dplyr::mutate(pooled, Country = "Pooled",
                            n_sub = n_for("Pooled", 1), n_rest = n_for("Pooled", 0))
  }
  by_co <- purrr::map_dfr(countries, function(co) {
    s <- dplyr::filter(p, .data$Country == co)
    r <- estimate_p_reform_diff_one(s, cluster_ids = s$clust)
    if (is.null(r)) return(NULL)
    dplyr::mutate(r, Country = co, n_sub = n_for(co, 1), n_rest = n_for(co, 0))
  })
  dplyr::bind_rows(pooled, by_co) %>%
    dplyr::relocate("Country")
}

#' Run the P(reform > repeal) subgroup difference for several specs and stack.
build_p_reform_diff_all <- function(pairs_reform_repeal, respondent_covs,
                                    specs, countries) {
  purrr::map_dfr(specs, function(sp) {
    sg <- respondent_covs %>%
      dplyr::transmute(.data$Country, .data$ID, group = .data[[sp$group_col]])
    build_p_reform_diff_table(
      pairs_reform_repeal, sg, countries, na_as_rest = isTRUE(sp$na_as_rest)
    ) %>%
      dplyr::mutate(subgroup = sp$label, group_col = sp$group_col)
  }) %>%
    dplyr::relocate("subgroup", "group_col")
}

#' Tidy DPP table for the SI (full sample, matching the main-text ACP).
tidy_dpp_si_table <- function(dpp_tab, level_lookup) {
  levels_h <- level_lookup %>%
    dplyr::group_by(.data$attribute, .data$level_short) %>%
    dplyr::summarise(
      level_order = min(.data$level_order, na.rm = TRUE),
      .groups = "drop"
    )
  dpp_tab %>%
    dplyr::left_join(
      levels_h %>% dplyr::rename(
        level_a = .data$level_short,
        level_a_order = .data$level_order
      ),
      by = c("attribute", "level_a")
    ) %>%
    dplyr::left_join(
      levels_h %>% dplyr::rename(
        level_b = .data$level_short,
        level_b_order = .data$level_order
      ),
      by = c("attribute", "level_b")
    ) %>%
    dplyr::mutate(
      attribute_label = unname(DPP_ATTR_LABELS[as.character(.data$attribute)]),
      attribute = factor(.data$attribute, levels = CONJOINT_ATTR_COLS),
      Country = factor(
        .data$Country,
        levels = unique(c("Pooled", as.character(.data$Country)))
      )
    ) %>%
    dplyr::arrange(
      .data$Country, .data$attribute,
      .data$level_a_order, .data$level_b_order
    ) %>%
    dplyr::select(
      Country, attribute, attribute_label,
      level_a, level_b, level_a_order, level_b_order,
      estimate, se, ci_lo, ci_hi
    )
}

format_dpp_si_tabular <- function(d) {
  d <- d %>%
    dplyr::arrange(.data$attribute, .data$level_a_order, .data$level_b_order) %>%
    dplyr::mutate(
      comparison = paste(.data$level_a, "vs", .data$level_b)
    )
  attr_id <- as.integer(d$attribute)
  new_attr <- which(c(TRUE, attr_id[-1] != attr_id[-length(attr_id)]))
  pair_row <- sprintf(
    "    %s & $%.3f$ & $%.3f$ & $[%.3f,\\,%.3f]$ \\\\",
    latex_escape(d$comparison),
    d$estimate, d$se, d$ci_lo, d$ci_hi
  )
  header_row <- sprintf(
    "    \\multicolumn{4}{@{}l}{\\textit{%s}} \\\\",
    latex_escape(as.character(d$attribute_label))
  )
  body <- pair_row
  for (i in rev(new_attr)) {
    extra <- header_row[[i]]
    if (i > 1L) {
      extra <- c("    \\addlinespace", extra)
    }
    body <- append(body, extra, after = i - 1L)
  }
  c(
    "\\setlength{\\tabcolsep}{4pt}",
    "\\begin{tabular}{@{}p{0.52\\textwidth}rrr@{}}",
    "  \\toprule",
    "  Comparison & DPP & SE & 95\\% CI \\\\",
    "  \\midrule",
    body,
    "  \\bottomrule",
    "\\end{tabular}"
  )
}

#' One booktabs DPP table per sample (pooled, then each country).
write_dpp_si_tex_tables <- function(tab, out_dir = "output/acp") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  groups <- if (is.factor(tab$Country)) {
    intersect(levels(tab$Country), unique(as.character(tab$Country)))
  } else {
    unique(as.character(tab$Country))
  }
  paths <- character()
  for (co in groups) {
    d <- dplyr::filter(tab, as.character(.data$Country) == .env$co)
    slug <- acp_si_tex_slug(co)
    who <- if (identical(co, "Pooled")) "pooled sample" else co
    caption <- paste0(
      "Direct pairwise preferences (Ganter DPP) from reform--reform comparisons, ",
      who,
      " (full sample; no repeal-ranker exclusions, matching the main-text ACP). ",
      "Each entry is P(prefer Level A over Level B) minus $1/2$. ",
      "Cluster-robust standard errors (respondent). ",
      "A positive DPP means Level A is preferred to Level B."
    )
    lines <- c(
      "% Requires \\usepackage{booktabs}",
      "\\begin{table}[htbp]",
      "  \\centering",
      "  \\small",
      paste0("  \\caption{", caption, "}"),
      paste0("  \\label{tab:dpp-", slug, "}"),
      paste0("  ", format_dpp_si_tabular(d)),
      "\\end{table}"
    )
    path <- file.path(out_dir, paste0("dpp_reform_reform_", slug, ".tex"))
    writeLines(lines, path)
    message("Saved ", path)
    paths <- c(paths, path)
  }
  invisible(paths)
}

# ---- SI tex: subgroup DPP diffs and reform-margin diffs ----------------------

#' Single booktabs table (pooled sample) grouping all subgroups: DPP contrasts
#' for the rest, the subgroup, and the difference (subgroup - rest). One section
#' per subgroup; country-level results are omitted (see the CSV for those).
write_dpp_subgroup_diff_tex <- function(tab, out_dir = "output/acp") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  d <- dplyr::filter(tab, as.character(.data$Country) == "Pooled")
  body <- character()
  for (sg in unique(d$subgroup)) {
    dc <- dplyr::filter(d, .data$subgroup == sg)
    attr_lab <- unname(DPP_ATTR_LABELS[as.character(dc$attribute[1])])
    if (is.na(attr_lab)) attr_lab <- as.character(dc$attribute[1])
    header <- sprintf("%s -- %s (n=%d vs %d)", sg, attr_lab, dc$n_sub[1], dc$n_rest[1])
    if (length(body)) body <- c(body, "    \\addlinespace")
    body <- c(body, sprintf("    \\multicolumn{6}{@{}l}{\\textit{%s}} \\\\",
                            latex_escape(header)))
    body <- c(body, sprintf(
      "    %s & $%.3f$ & $%.3f$ & $%.3f$ & $%.3f$ & $[%.3f,\\,%.3f]$ \\\\",
      latex_escape(paste(dc$level_a, "vs", dc$level_b)),
      dc$est_rest, dc$est_sub, dc$est_diff, dc$se_diff, dc$ci_diff_lo, dc$ci_diff_hi
    ))
  }
  caption <- paste0(
    "Subgroup heterogeneity in direct pairwise preferences (Ganter DPP), ",
    "pooled sample. For each subgroup vs the rest, `Rest' and `Subgroup' are the ",
    "within-group DPP contrasts (P(prefer A over B) minus $1/2$) and `Diff' is ",
    "subgroup minus rest, the interaction term of ",
    "lm(outcome $\\sim$ V $\\times$ subgroup) on reform--reform pairs. ",
    "Full sample; cluster-robust SEs (respondent). ",
    "$n$ = respondents (subgroup vs rest)."
  )
  lines <- c(
    "% Requires \\usepackage{booktabs}",
    "\\begin{table}[htbp]", "  \\centering", "  \\small",
    paste0("  \\caption{", caption, "}"),
    "  \\label{tab:dpp-subgroup-pooled}",
    "  \\setlength{\\tabcolsep}{4pt}",
    "  \\begin{tabular}{@{}p{0.40\\textwidth}rrrrr@{}}",
    "    \\toprule",
    "    Comparison & Rest & Subgroup & Diff & SE & 95\\% CI \\\\",
    "    \\midrule",
    body,
    "    \\bottomrule",
    "  \\end{tabular}",
    "\\end{table}"
  )
  path <- file.path(out_dir, "dpp_subgroup_pooled.tex")
  writeLines(lines, path)
  message("Saved ", path)
  invisible(path)
}

#' Single booktabs table (pooled sample): subgroup-vs-rest difference in
#' P(reform > repeal), one row per subgroup. Country results are omitted.
write_p_reform_diff_tex <- function(tab, out_dir = "output/acp") {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  d <- dplyr::filter(tab, as.character(.data$Country) == "Pooled")
  body <- sprintf(
    "    %s & $%.3f$ & $%.3f$ & $%.3f$ & $%.3f$ & $[%.3f,\\,%.3f]$ \\\\",
    latex_escape(sprintf("%s (n=%d vs %d)", d$subgroup, d$n_sub, d$n_rest)),
    d$p_rest, d$p_sub, d$diff, d$se_diff, d$ci_lo, d$ci_hi
  )
  caption <- paste0(
    "Subgroup heterogeneity in overall reform support, ",
    "P(reform $\\succ$ repeal), pooled sample. For each subgroup vs the rest, ",
    "`P(rest)' and `P(sub)' are group means and `Diff' is subgroup minus rest ",
    "from lm(outcome $\\sim$ subgroup) on all reform--repeal pairs (full sample; ",
    "not attribute-restricted). Cluster-robust SEs (respondent). ",
    "$n$ = respondents (subgroup vs rest)."
  )
  lines <- c(
    "% Requires \\usepackage{booktabs}",
    "\\begin{table}[htbp]", "  \\centering", "  \\small",
    paste0("  \\caption{", caption, "}"),
    "  \\label{tab:p-reform-subgroup}",
    "  \\setlength{\\tabcolsep}{4pt}",
    "  \\begin{tabular}{@{}p{0.40\\textwidth}rrrrr@{}}",
    "    \\toprule",
    "    Subgroup & P(rest) & P(sub) & Diff & SE & 95\\% CI \\\\",
    "    \\midrule",
    body,
    "    \\bottomrule",
    "  \\end{tabular}",
    "\\end{table}"
  )
  path <- file.path(out_dir, "p_reform_diff_subgroup.tex")
  writeLines(lines, path)
  message("Saved ", path)
  invisible(path)
}

# ---- ACP plots -------------------------------------------------------------------

#' Tidy ACP table with level order and attribute labels for plotting.
prepare_acp_reform_reform_plot_data <- function(acp_results, level_lookup) {
  summarise_acp_results(acp_results, drop_repeal_level = FALSE) %>%
    dplyr::left_join(
      level_lookup %>%
        dplyr::distinct(.data$Country, .data$attribute, .data$level_short, .data$level_order),
      by = c("Country", "attribute", "level" = "level_short")
    ) %>%
    dplyr::mutate(
      attribute       = factor(.data$attribute, levels = CONJOINT_ATTR_COLS),
      attribute_label = unname(CONJOINT_ATTR_LABELS[as.character(.data$attribute)])
    )
}

#' Filename suffix encoding repeal-ranker exclusion flags.
acp_figure_flag_suffix <- function(exclude_repeal_always_first = FALSE,
                                   exclude_repeal_always_last = FALSE) {
  paste0(
    if (exclude_repeal_always_first) "exclrepealfst" else "inclrepealfst",
    "_",
    if (exclude_repeal_always_last) "exclrepeallst" else "inclrepeallst"
  )
}

#' Pairs + reform-reform ACP for one repeal-ranker exclusion spec.
build_acp_sample_bundle <- function(pairs_reform_repeal_raw,
                                    pairs_reform_reform_raw,
                                    level_lookup,
                                    countries,
                                    exclude_repeal_always_first = FALSE,
                                    exclude_repeal_always_last = FALSE) {
  pairs_reform_repeal <- filter_pairs_by_repeal_rankers(
    pairs_reform_repeal_raw,
    exclude_always_first = exclude_repeal_always_first,
    exclude_always_last  = exclude_repeal_always_last
  )
  pairs_reform_reform <- filter_pairs_by_repeal_rankers(
    pairs_reform_reform_raw,
    exclude_always_first = exclude_repeal_always_first,
    exclude_always_last  = exclude_repeal_always_last
  )
  v_tables_reform_reform <- build_all_v_tables_reform_reform(
    pairs_reform_reform, level_lookup = level_lookup
  )
  acp_results_reform_reform <- estimate_acp_from_v_tables(
    v_tables_reform_reform,
    level_lookup   = level_lookup,
    countries      = countries,
    estimand       = "acp",
    adjust         = FALSE
  )
  list(
    pairs_reform_repeal       = pairs_reform_repeal,
    pairs_reform_reform       = pairs_reform_reform,
    acp_results_reform_reform = acp_results_reform_reform
  )
}

#' Restrict raw pair tables to a respondent subgroup, then build ACP bundle.
build_acp_subgroup_bundle <- function(pairs_reform_repeal_raw,
                                      pairs_reform_reform_raw,
                                      respondent_ids,
                                      level_lookup,
                                      countries,
                                      exclude_repeal_always_first = FALSE,
                                      exclude_repeal_always_last = FALSE) {
  pairs_reform_repeal_raw <- pairs_reform_repeal_raw %>%
    dplyr::semi_join(respondent_ids, by = c("Country", "ID"))
  pairs_reform_reform_raw <- pairs_reform_reform_raw %>%
    dplyr::semi_join(respondent_ids, by = c("Country", "ID"))
  build_acp_sample_bundle(
    pairs_reform_repeal_raw,
    pairs_reform_reform_raw,
    level_lookup                = level_lookup,
    countries                   = countries,
    exclude_repeal_always_first = exclude_repeal_always_first,
    exclude_repeal_always_last  = exclude_repeal_always_last
  )
}

# ---- Bundled min vs max packages -------------------------------------------------
# Dumbbell: the two poles by colour/shape; remaining dimension (sample or
# country) by colour/marker. d2 (information vs infrastructure) is pooled.

BUNDLED_PACKAGE_DEFS <- list(
  minimal = list(
    id          = "minimal",
    short_label = "Minimal package",
    plot_label  = paste(
      "Carbon revenue only - government-managed -",
      "lower investment"
    ),
    colour      = "grey40",
    fill        = "white",
    shape       = 21L,
    filter = function(d) {
      dplyr::filter(d, .data$bf == 1L, .data$bc == 1L,
                    .data$hh == 1L, .data$trans == 1L)
    }
  ),
  maximal = list(
    id          = "maximal",
    short_label = "Maximal package",
    plot_label  = paste(
      "Expanded budget with wealth tax -",
      "protected fund with citizen oversight -",
      "higher investment"
    ),
    colour      = "#0072B2",
    fill        = "#0072B2",
    shape       = 22L,
    filter = function(d) {
      dplyr::filter(d, .data$bf == 3L, .data$bc == 3L,
                    .data$hh == 3L, .data$trans == 3L)
    }
  )
)

BUNDLED_PACKAGE_SAMPLE_SPECS <- function(pairs_non_committed,
                                           pairs_opposers) {
  list(
    list(
      id = "non_committed", order = 1L,
      label = "Non-committed sample",
      pairs = pairs_non_committed
    ),
    list(
      id = "opposers", order = 2L,
      label = "Opposers to carbon pricing",
      pairs = pairs_opposers
    )
  )
}

# Sample colours when countries are on the y-axis.
BUNDLED_SAMPLE_COLOURS <- c(
  "Non-committed sample"       = "grey20",
  "Opposers to carbon pricing" = "#C44E52"
)
BUNDLED_SAMPLE_FILLS <- c(
  "Non-committed sample"       = "white",
  "Opposers to carbon pricing" = "#C44E52"
)
BUNDLED_SAMPLE_SHAPES <- c(
  "Non-committed sample"       = 21L,
  "Opposers to carbon pricing" = 22L
)

BUNDLED_COUNTRY_SHAPES <- c(
  Spain   = 21L,
  France  = 22L,
  Germany = 24L,
  Romania = 23L
)

bundled_package_styles <- function() {
  defs <- BUNDLED_PACKAGE_DEFS
  list(
    colours = stats::setNames(
      vapply(defs, `[[`, character(1), "colour"),
      vapply(defs, `[[`, character(1), "short_label")
    ),
    fills = stats::setNames(
      vapply(defs, `[[`, character(1), "fill"),
      vapply(defs, `[[`, character(1), "short_label")
    ),
    shapes = stats::setNames(
      vapply(defs, `[[`, integer(1), "shape"),
      vapply(defs, `[[`, character(1), "short_label")
    ),
    plot_labels = stats::setNames(
      vapply(defs, `[[`, character(1), "plot_label"),
      vapply(defs, `[[`, character(1), "short_label")
    )
  )
}

#' P(reform beats repeal) for the two bundled poles, pooling over d1/d2 halves.
summarise_bundled_package_contrast <- function(pairs,
                                             level_lookup,
                                             sample_label,
                                             countries = NULL) {
  p <- prepare_full_package_pairs(pairs, level_lookup)
  if (!is.null(countries)) {
    p <- dplyr::filter(p, .data$Country %in% .env$countries)
  }
  p <- p %>%
    dplyr::filter(
      !is.na(.data$bf), !is.na(.data$bc), !is.na(.data$hh),
      !is.na(.data$trans)
    ) %>%
    dplyr::mutate(clust = paste(.data$Country, .data$ID, sep = ":"))

  purrr::imap_dfr(BUNDLED_PACKAGE_DEFS, function(def, pkg_id) {
    p %>%
      def$filter() %>%
      dplyr::group_by(.data$Country) %>%
      dplyr::group_modify(~ compute_p_reform_beats_repeal(.x)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        package_id    = pkg_id,
        package_label = def$short_label,
        sample_label  = sample_label
      )
  })
}

build_bundled_package_contrast_data <- function(sample_specs,
                                                level_lookup,
                                                countries = NULL) {
  if (is.null(countries)) {
    countries <- sort(unique(sample_specs[[1]]$pairs$Country))
  }
  purrr::map_dfr(sample_specs, function(spec) {
    summarise_bundled_package_contrast(
      spec$pairs, level_lookup,
      sample_label = spec$label,
      countries = countries
    ) %>%
      dplyr::mutate(
        sample_id    = spec$id,
        sample_order = spec$order
      )
  }) %>%
    dplyr::mutate(
      Country = factor(.data$Country, levels = names(HEADLINE_COUNTRY_COLOURS)),
      sample_label = factor(
        .data$sample_label,
        levels = vapply(sample_specs, `[[`, character(1), "label")
      ),
      package_label = factor(
        .data$package_label,
        levels = vapply(BUNDLED_PACKAGE_DEFS, `[[`, character(1), "short_label")
      )
    )
}

prepare_bundled_package_segment_data <- function(contrast_data,
                                                  group_vars) {
  contrast_data %>%
    dplyr::select(
      dplyr::all_of(c(group_vars, "package_label", "p_reform_beats_repeal"))
    ) %>%
    tidyr::pivot_wider(
      names_from  = "package_label",
      values_from = "p_reform_beats_repeal"
    ) %>%
    dplyr::filter(
      !is.na(.data$`Minimal package`),
      !is.na(.data$`Maximal package`)
    )
}

plot_bundled_package_contrast <- function(contrast_data,
                                          layout = c("by_country", "by_sample"),
                                          x_limits = c(0.20, 0.78),
                                          x_label = PANEL_B_XLAB,
                                          draw_connectors = FALSE) {
  layout <- match.arg(layout)
  if (!nrow(contrast_data)) {
    stop("No bundled package contrast rows to plot.", call. = FALSE)
  }
  styles <- bundled_package_styles()
  dodge_w <- 0.55

  if (layout == "by_country") {
    row_var <- "Country"
    within_var <- "sample_label"
    shape_values <- BUNDLED_SAMPLE_SHAPES
  } else {
    row_var <- "sample_label"
    within_var <- "Country"
    shape_values <- BUNDLED_COUNTRY_SHAPES
  }

  # Manual dodge shared by points and error bars. First row level is placed at
  # the top (Spain, France, Germany, Romania when Country is the row axis).
  row_levels <- levels(contrast_data[[row_var]])
  within_levels <- levels(contrast_data[[within_var]])
  n_within <- length(within_levels)
  n_rows <- length(row_levels)
  offset_for <- function(w) {
    i <- as.integer(factor(as.character(w), levels = within_levels))
    dodge_w * ((i - 0.5) / n_within - 0.5)
  }

  d <- contrast_data %>%
    dplyr::mutate(
      y_row = n_rows + 1L -
        as.integer(factor(as.character(.data[[row_var]]), levels = row_levels)),
      # Reverse offset so the first within-group appears on top within a row.
      y_pos = .data$y_row - offset_for(.data[[within_var]])
    )

  plot_labels_wrapped <- stats::setNames(
    stringr::str_wrap(unname(styles$plot_labels), width = 42),
    names(styles$plot_labels)
  )

  p <- ggplot2::ggplot(
    d,
    ggplot2::aes(
      x = .data$p_reform_beats_repeal,
      y = .data$y_pos,
      colour = .data$package_label,
      fill = .data$package_label,
      shape = .data[[within_var]]
    )
  ) +
    ggplot2::geom_vline(
      xintercept = 0.5, linewidth = 0.35,
      linetype = "dashed", colour = "grey55"
    )

  if (isTRUE(draw_connectors)) {
    seg <- prepare_bundled_package_segment_data(
      contrast_data, c(row_var, within_var)
    ) %>%
      dplyr::mutate(
        y_row = n_rows + 1L -
          as.integer(factor(as.character(.data[[row_var]]), levels = row_levels)),
        y_pos = .data$y_row - offset_for(.data[[within_var]])
      )
    p <- p +
      ggplot2::geom_segment(
        data = seg,
        ggplot2::aes(
          x = .data$`Minimal package`,
          xend = .data$`Maximal package`,
          y = .data$y_pos,
          yend = .data$y_pos
        ),
        inherit.aes = FALSE,
        linewidth = 0.4, colour = "grey55", alpha = 0.7
      )
  }

  p +
    ggplot2::geom_errorbar(
      ggplot2::aes(xmin = .data$ci_lo, xmax = .data$ci_hi),
      orientation = "y",
      width = 0.1, linewidth = 0.45
    ) +
    ggplot2::geom_point(size = 2.0, stroke = 0.55) +
    ggplot2::scale_colour_manual(
      values = styles$colours,
      labels = plot_labels_wrapped,
      name = NULL
    ) +
    ggplot2::scale_fill_manual(
      values = styles$fills,
      labels = plot_labels_wrapped,
      name = NULL
    ) +
    ggplot2::scale_shape_manual(
      values = shape_values, name = NULL
    ) +
    ggplot2::guides(
      colour = ggplot2::guide_legend(order = 1, override.aes = list(
        shape = 21, size = 2.2, stroke = 0.55
      )),
      fill = ggplot2::guide_legend(order = 1),
      shape = ggplot2::guide_legend(order = 2, override.aes = list(
        colour = "grey20", fill = "white", size = 2.2
      ))
    ) +
    ggplot2::scale_x_continuous(
      labels = scales::percent_format(accuracy = 1),
      limits = x_limits,
      breaks = seq(ceiling(x_limits[1] * 10) / 10,
                   floor(x_limits[2] * 10) / 10, by = 0.1),
      expand = ggplot2::expansion(mult = c(0.02, 0.02))
    ) +
    ggplot2::scale_y_continuous(
      breaks = seq_len(n_rows),
      labels = stringr::str_wrap(rev(row_levels), width = 18),
      expand = ggplot2::expansion(add = 0.45)
    ) +
    ggplot2::labs(x = x_label, y = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y        = ggplot2::element_text(size = 8),
      axis.text.x        = ggplot2::element_text(size = 8),
      axis.title.x       = ggplot2::element_text(size = 7),
      legend.position    = "bottom",
      legend.box         = "vertical",
      legend.text        = ggplot2::element_text(size = 6.5, lineheight = 0.95),
      legend.key.height  = ggplot2::unit(0.55, "cm"),
      legend.key.width   = ggplot2::unit(0.45, "cm"),
      plot.margin        = ggplot2::margin(6, 10, 4, 6, "pt")
    )
}

#' Bundled minimal vs maximal packages (non-committed sample vs Q46_2 opposers).
save_main_p_reform_beats_repeal_figures <- function(pairs_non_committed,
                                                  pairs_opposers,
                                                  level_lookup,
                                                  countries = NULL,
                                                  x_limits = c(0.20, 0.78),
                                                  out_dir = "figures/acp",
                                                  width_mm = 170,
                                                  height_mm = 110) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  sample_specs <- BUNDLED_PACKAGE_SAMPLE_SPECS(
    pairs_non_committed, pairs_opposers
  )
  contrast_data <- build_bundled_package_contrast_data(
    sample_specs, level_lookup, countries = countries
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  readr::write_csv(
    contrast_data,
    file.path(out_dir, "main_p_reform_beats_repeal_all_samples.csv")
  )

  p <- plot_bundled_package_contrast(
    contrast_data,
    layout = "by_country",
    x_limits = x_limits,
    draw_connectors = FALSE
  )
  path <- file.path(out_dir, "main_p_reform_beats_repeal_by_country.pdf")
  ggplot2::ggsave(path, p, width = width_mm / 25.4, height = height_mm / 25.4)
  message("Saved ", path)

  invisible(contrast_data)
}

# ---- Shared country colours / funding pair prep ----------------------------------

#' Country fill/colour palette, shared across country overlays.
HEADLINE_COUNTRY_COLOURS <- c(
  Spain   = "#E64B35FF",
  France  = "#4DBBD5FF",
  Germany = "#00A087FF",
  Romania = "#3C5488FF"
)

#' Attach budget/household/transition reform-level orders; transition pools the
#' worker_support / community_mobility_support half each respondent actually saw.
prepare_funding_package_pairs <- function(pairs, level_lookup) {
  for (a in c("budget_and_funding", "household_support",
              "worker_support", "community_mobility_support")) {
    pairs <- add_reform_level_order(pairs, a, level_lookup)
  }
  pairs %>%
    dplyr::mutate(
      bf    = .data$budget_and_funding_order_1,
      hh    = .data$household_support_order_1,
      trans = dplyr::if_else(
        .data$d1 == "worker_support",
        .data$worker_support_order_1,
        .data$community_mobility_support_order_1
      )
    )
}

#' Save reform–reform ACP figures: pooled PDF, plus A4 country page or per-country PDFs.
#'
#' @param countries_as_a4_page If TRUE (default), write one A4-portrait PDF of
#'   the four countries instead of four separate tall PDFs.
save_acp_reform_reform_figures <- function(acp_results,
                                          pairs_reform_reform,
                                          level_lookup,
                                          exclude_repeal_always_first = FALSE,
                                          exclude_repeal_always_last = FALSE,
                                          countries = NULL,
                                          file_suffix = "",
                                          countries_as_a4_page = TRUE,
                                          out_dir = "figures/acp",
                                          width_mm = 180,
                                          height_mm = 290,
                                          a4_width_mm = 190,
                                          a4_height_mm = 175) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for ACP plots.", call. = FALSE)
  }
  if (is.null(countries)) {
    countries <- sort(unique(pairs_reform_reform$Country))
  }
  flag_suffix <- paste0(
    acp_figure_flag_suffix(
      exclude_repeal_always_first = exclude_repeal_always_first,
      exclude_repeal_always_last  = exclude_repeal_always_last
    ),
    file_suffix
  )
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  v_tables_pooled <- build_all_v_tables_reform_reform(
    pairs_reform_reform, level_lookup = level_lookup
  )
  acp_pooled <- estimate_pooled_acp_from_v_tables(v_tables_pooled, level_lookup)
  acp_pooled_wrapped <- wrap_pooled_acp_as_country(acp_pooled, "Pooled")
  plot_data_pooled <- prepare_acp_reform_reform_plot_data(
    acp_pooled_wrapped, level_lookup
  ) %>%
    dplyr::filter(.data$Country == "Pooled")
  plot_data_countries <- prepare_acp_reform_reform_plot_data(
    acp_results, level_lookup
  )
  attr_order <- rank_attributes_by_acp_spread(plot_data_pooled)$attribute

  importance_pooled <- tidy_acp_variability_by_attribute(acp_pooled, group = "Pooled")
  readr::write_csv(
    importance_pooled %>%
      dplyr::mutate(
        attribute_lab = unname(PAPER_ATTR_LABELS[as.character(.data$attribute)])
      ),
    file.path(out_dir, paste0("panel_a_acp_importance_pooled_", flag_suffix, ".csv"))
  )

  pa_pooled <- plot_acp_reform_reform_panel(
    plot_data_pooled,
    attribute_order = attr_order,
    importance_tab = importance_pooled
  )
  path_pa_pooled <- file.path(
    out_dir, paste0("panel_a_acp_pooled_", flag_suffix, ".pdf")
  )
  ggplot2::ggsave(path_pa_pooled, pa_pooled,
                  width = width_mm / 25.4, height = height_mm / 25.4)
  message("Saved ", path_pa_pooled)

  if (isTRUE(countries_as_a4_page)) {
    pa_countries <- plot_acp_reform_reform_countries_a4(
      plot_data_countries,
      countries = countries,
      attribute_order = attr_order
    )
    path_countries <- file.path(
      out_dir, paste0("panel_a_acp_countries_", flag_suffix, ".pdf")
    )
    ggplot2::ggsave(
      path_countries, pa_countries,
      width = a4_width_mm, height = a4_height_mm, units = "mm"
    )
    message("Saved ", path_countries)
  } else {
    purrr::walk(countries, function(co) {
      importance_co <- tidy_acp_variability_by_attribute(
        acp_results[[co]], group = co
      )
      pa_co <- plot_acp_reform_reform_panel(
        plot_data_countries,
        country = co,
        attribute_order = attr_order,
        importance_tab = importance_co
      )
      slug <- gsub("[^a-z0-9]+", "_", tolower(co))
      path_co <- file.path(
        out_dir, paste0("panel_a_acp_", slug, "_", flag_suffix, ".pdf")
      )
      ggplot2::ggsave(path_co, pa_co,
                      width = width_mm / 25.4, height = height_mm / 25.4)
      message("Saved ", path_co)
    })
  }

  invisible(list(attribute_order = attr_order, acp_pooled = acp_pooled))
}
