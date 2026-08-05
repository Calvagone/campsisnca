#'
#' Replicated NCA table class.
#'
#' @export
setClass(
  "replicated_nca_table",
  representation(
    title = "character",
    subtitle = "character",
    selected_statistics = "character", # vector
    summary_stat_display = "character", # vector
    summary_stat_signif_digits = "integer", # integer value
    strata = "character", # Named vector
    data = "data.frame", # Transient field
    effective_strat_vars = "character", # effective stratification variables in data, transient field: updated when calculate is called
    tab_options = "list"
  ),
  prototype = prototype(
    title = NA_character_,
    subtitle = NA_character_,
    selected_statistics = character(), # empty means no filter (all stats used)
    summary_stat_display = get_stat_display_default(),
    summary_stat_signif_digits = 3L,
    strata = get_default_strata(),
    tab_options = list()
  )
)

#'
#' Replicated NCA table.
#'
#' @param title table title, optional character value
#' @param subtitle table subtitle, optional character value
#' @param selected_statistics NCA metrics statistics to keep (e.g. mean, etc) when summary statistics are computed on replicated output.
#'  Default is the empty character vector (all statistics are computed).
#' @param summary_stat_display display format for replicate statistics, character vector. Default is \verb{'{median} ({p5}–{p95})'}.
#' @param summary_stat_signif_digits number of significant digits to display for replicate statistics, default is 3.
#' @param strata strata levels this analysis refers to, named vector, e.g. c(ARM='1g QD').
#'  Note, the default strata are c(SCENARIO='all', ARM='all').
#'  Use 'all' if this analysis refers to all levels for the specified stratification variable.
#'  By default, a stratification variable that has only 1 level is ignored.
#' @param tab_options list of options to pass to gt::tab_options
#' @param json path to JSON table file or JSON content in string form
#' @export
ReplicatedNCATable <- function(
  title = NULL,
  subtitle = NULL,
  selected_statistics = character(),
  summary_stat_display = get_stat_display_default(),
  summary_stat_signif_digits = 3L,
  strata = get_default_strata(),
  tab_options = list(),
  json = NULL
) {
  if (is.null(json)) {
    if (is.null(title)) {
      title <- NA_character_
    }
    if (is.null(subtitle)) {
      subtitle <- NA_character_
    }
    if (is.null(strata)) {
      strata <- get_default_strata()
    }
    table <- new(
      "replicated_nca_table",
      title = title,
      subtitle = subtitle,
      selected_statistics = selected_statistics,
      summary_stat_display = summary_stat_display,
      summary_stat_signif_digits = summary_stat_signif_digits,
      strata = strata,
      tab_options = tab_options
    )
  } else {
    table <- load_from_json(object = new("replicated_nca_table"), json = json)
  }
  return(table)
}

#_______________________________________________________________________________
#----                              calculate                                ----
#_______________________________________________________________________________

#' Does the data contain more than one replicate?
#'
#' @return \code{TRUE} if the data contains a \code{replicate} column with more than one distinct value, \code{FALSE} otherwise
#' @param x a data frame (typically \code{std_campsis_tbl})
#' @importFrom dplyr n_distinct
#' @keywords internal
.is_replicated <- function(x) {
  return("replicate" %in% colnames(x) && dplyr::n_distinct(x$replicate) > 1)
}

#' @importFrom dplyr across filter if_any if_else matches mutate select transmute
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_wider
#' @importFrom rlang .data
#' @rdname calculate
setMethod(
  "calculate",
  signature = c("replicated_nca_table", "campsis_output", "ANY"),
  definition = function(object, x, options, ...) {
    # Check data frame class
    if (!is(x, "summary_campsisnca_tbl")) {
      stop("x must be of class 'summary_campsisnca_tbl' for now")
    }

    # Check if the data contains replicates
    if (!.is_replicated(x)) {
      stop(
        "x must contain a 'replicate' column with more than one distinct value"
      )
    }

    # Add category if not existing
    if (!"category" %in% colnames(x)) {
      x$category <- NA_character_
    }

    categories <- x %>%
      tibble::add_column(!!!c(category = NA)[!"category" %in% names(x)]) %>%
      dplyr::filter(!is.na(.data$category)) %>%
      dplyr::transmute(.data$metric, .data$stat, .data$category) %>%
      dplyr::distinct()

    # Filter NCA metric statistics
    if (length(object@selected_statistics) > 0) {
      x_filtered <- x %>%
        dplyr::filter(.data$stat %in% object@selected_statistics)
    } else {
      x_filtered <- x
    }

    # Detect specific strata
    specific_strata <- object@strata[object@strata != all_strata_levels()]
    specific_strata_names <- names(specific_strata)

    # Filter input data frame to specific strata
    x_reduced <- purrr::reduce(
      specific_strata_names,
      ~ dplyr::filter(.x, .data[[.y]] == specific_strata[[.y]]),
      .init = x_filtered
    )

    # remove specific stratification variables
    x_reduced <- x_reduced %>%
      dplyr::select(-dplyr::all_of(specific_strata_names))

    # Detect effective stratification variables
    all_cols <- colnames(x_reduced)
    object@effective_strat_vars <- all_cols[
      !all_cols %in% c("replicate", "metric", "stat", "value", "category")
    ]

    x_wider <- x_reduced %>%
      dplyr::filter(dplyr::if_any(
        dplyr::matches("category"),
        ~ is.na(.x) | .x != "FALSE"
      )) %>%
      discard_category_column() %>%
      tidyr::pivot_wider(
        names_from = c("metric", "stat"),
        values_from = c("value"),
        names_glue = "{metric} ({stat})"
      ) %>%
      dplyr::mutate(dplyr::across(
        dplyr::any_of(sprintf(
          "%s (%s_%s)",
          categories$metric,
          categories$stat,
          categories$category
        )),
        .fns = ~ dplyr::if_else(is.na(.x), 0, .x)
      )) %>%
      dplyr::select(-replicate)

    object@data <- x_wider
    return(object)
  }
)

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

#' Export replicated NCA table.
#'
#' @param object replicated_nca_table object
#' @param dest destination for the summarised table, either "dataframe", "gtsummary" or "gt"
#' @param ... extra arguments
#' @importFrom gtsummary all_categorical all_continuous all_stat_cols modify_footnote modify_header tbl_summary
#' @importFrom rlang as_function
setMethod(
  "export",
  signature = c("replicated_nca_table", "character"),
  definition = function(object, dest, ...) {
    x_wider <- object@data
    if (dest == "dataframe") {
      return(x_wider)
    }

    stat_type <- if (length(object@summary_stat_display) > 1) {
      "continuous2"
    } else {
      "continuous"
    }

    if (dest %in% c("gt", "gtsummary")) {
      gtsummary_table <- gtsummary::tbl_summary(
        data = x_wider,
        by = object@effective_strat_vars,
        statistic = list(
          gtsummary::all_continuous() ~ object@summary_stat_display,
          gtsummary::all_categorical() ~ object@summary_stat_display
        ),
        type = list(
          gtsummary::all_continuous() ~ stat_type,
          gtsummary::all_categorical() ~ stat_type
        ),
        label = list(),
        digits = list(
          gtsummary::all_continuous() ~ list(rlang::as_function(
            ~ gtsummary::style_sigfig(.x, object@summary_stat_signif_digits)
          )),
          gtsummary::all_categorical() ~ list(rlang::as_function(
            ~ gtsummary::style_sigfig(.x, object@summary_stat_signif_digits)
          ))
        )
      ) %>%
        gtsummary::modify_header(
          gtsummary::all_stat_cols() ~ "**{level}**",
          label = "**Metric**"
        )

      # Instead of 'Overall', we clearly show the specific strata
      specific_strata <- object@strata[object@strata != all_strata_levels()]
      if (length(specific_strata) > 0) {
        gtsummary_table <- gtsummary_table %>%
          gtsummary::modify_header(
            stat_0 = paste0(
              "**",
              paste0(specific_strata, collapse = " / "),
              "**"
            )
          )
      }

      if (length(object@summary_stat_display) == 1) {
        gtsummary_table <- gtsummary_table %>%
          gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~ sprintf(
              "%s, N<sub>rep</sub> = {n}",
              translate_stat_string(object@summary_stat_display)
            )
          )
      } else {
        gtsummary_table <- gtsummary_table %>%
          gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~ "N<sub>rep</sub> = {n}"
          )
      }

      if (dest == "gtsummary") {
        return(gtsummary_table)
      }
      if (dest == "gt") {
        gt_table <- gtsummary_table %>%
          toGt(
            subscripts = TRUE,
            title = object@title,
            subtitle = object@subtitle,
            opts = object@tab_options
          )
        return(gt_table)
      }
    } else {
      stop("Unsupported destination for summarise_replicates")
    }
  }
)

#_______________________________________________________________________________
#----                               utilities                               ----
#_______________________________________________________________________________

translate_stat_string <- function(stat_string) {
  # Standard map for common base statistics
  stat_map <- c(
    "median" = "Median",
    "mean" = "Mean",
    "sd" = "SD",
    "se" = "SE",
    "var" = "Variance",
    "min" = "Minimum",
    "max" = "Maximum",
    "sum" = "Sum",
    "n" = "n",
    "N" = "N"
  )

  # Replace base statistics (e.g., {median} -> Median)
  for (stat in names(stat_map)) {
    stat_string <- gsub(paste0("{", stat, "}"), stat_map[stat], stat_string, fixed = TRUE)
  }

  # Dynamic map for percentiles (e.g., {p5} -> 5th percentile)
  while (grepl("\\{p[0-9]+\\}", stat_string)) {
    p_match <- regmatches(stat_string, regexpr("\\{p([0-9]+)\\}", stat_string))
    p_num <- gsub("[^0-9]", "", p_match)

    # Determine ordinal suffix (st, nd, rd, th)
    suffix <- switch(
      ifelse(p_num %in% c("11", "12", "13"), "th", substr(p_num, nchar(p_num), nchar(p_num))),
      "1" = "st",
      "2" = "nd",
      "3" = "rd",
      "th"
    )

    # CRITICAL FIX: Add fixed = TRUE here so R treats "{p5}" as a literal string
    stat_string <- sub(p_match, paste0(p_num, suffix, " percentile"), stat_string, fixed = TRUE)
  }

  # Clean up duplicate trailing " percentile" strings if they appear in ranges
  stat_string <- gsub(" percentile([\U2013-])([0-9]+[a-z]{2} percentile)", "\\1\\2", stat_string)

  return(stat_string)
}

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("replicated_nca_table", "json_element"), definition = function(object, json) {
  json <- json@data

  # Extract possible tab options
  if (!is.null(json$tab_options)) {
    object@tab_options <- json$tab_options
    json$tab_options <- NULL
  }

  object <- map_json_properties_to_s4_slots(object = object, json = JSONElement(json))
  return(object)
})

setMethod("load_from_json", signature = c("replicated_nca_table", "character"), definition = function(object, json) {
  schema <- system.file("extdata", "campsisnca_replicated_table.schema.json", package = "campsisnca")
  return(load_from_json(object = object, json = open_json(json = json, schema = schema)))
})

setMethod("load_from_json", signature = c("replicated_nca_table", "list"), definition = function(object, json) {
  schema <- system.file("extdata", "campsisnca_replicated_table.schema.json", package = "campsisnca")
  return(load_from_json(object = object, json = open_json(json = json, schema = schema)))
})
