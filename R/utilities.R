#'
#' Get the Campsisnca options (R options).
#'
#' @return global options for Campsisnca
#' @export
#' @keywords internal
getCampsisncaOptions <- function() {
  return(getOption("campsisnca.options"))
}

#'
#' Get Campsisnca option logic.
#'
#' @param name option to search
#' @param default default value if option not found
#' @return option value
#' @export
getCampsisncaOption <- function(name, default) {
  option <- getCampsisncaOptions()
  if (is.null(option)) {
    return(default)
  } else {
    value <- option[[name]]
    if (is.null(value)) {
      return(default)
    } else {
      return(value)
    }
  }
}

all_strata_levels <- function() {
  return("all")
}

get_default_strata <- function() {
  return(c(SCENARIO = all_strata_levels(), ARM = all_strata_levels()))
}

#' Filter Column Names to Character (and Optionally Factor) Columns
#'
#' Filters a vector of column names to include only those present in a data frame 
#' that are of class character (and optionally factor).
#'
#' @param x A data frame or tibble.
#' @param cols A character vector of candidate column names.
#' @param include_factor Logical. If \code{TRUE}, factor columns are also included 
#'   along with character columns. Defaults to \code{FALSE}.
#'
#' @return A character vector of column names matching the specified class criteria.
#' @export
#' @importFrom purrr map_lgl
get_character_cols_only <- function(x, cols, include_factor = FALSE) {
  matched <- intersect(cols, colnames(x))
  
  if (length(matched) == 0) {
    return(character(0))
  }

  is_valid_col <- purrr::map_lgl(matched, ~ {
    is.character(x[[.x]]) || (include_factor && is.factor(x[[.x]]))
  })

  return(matched[is_valid_col])
}

#' Preserve Existing Column Value Order as Factor Levels
#'
#' Converts target columns into factors using their current unique row appearance
#' order as the factor levels.
#'
#' @param x a data frame or tibble.
#' @param cols a character vector of column names to convert.
#'
#' @return a data frame with updated factor columns.
#' @export
#' @importFrom dplyr mutate across all_of
preserve_column_levels <- function(x, cols) {
  if (length(cols) > 0) {
    x <- x %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(cols),
          ~ factor(.x, levels = unique(.x))
        )
      )
  }
  return(x)
}

#' Strip Factor Class from Columns
#'
#' Converts target columns from factors into standard character vectors.
#'
#' @param x a data frame or tibble.
#' @param cols a character vector of column names to convert.
#'
#' @return a data frame with character columns.
#' @export
#' @importFrom dplyr mutate across all_of
remove_column_levels <- function(x, cols) {
  if (length(cols) > 0) {
    x <- x %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(cols),
          as.character
        )
      )
  }
  return(x)
}

#' Extract and Save Column Factor Levels
#'
#' Extracts factor levels (or unique value order for non-factors) for target columns
#' and stores them in a named list for later restoration.
#'
#' @param x a data frame or tibble.
#' @param cols a character vector of column names whose levels should be saved.
#'
#' @return a named list where names correspond to column names and values
#'   contain character vectors of factor levels.
#' @export
#' @importFrom purrr map
#' @importFrom stats setNames
save_column_levels <- function(x, cols) {
  target_cols <- get_character_cols_only(x = x, cols = cols, include_factor = TRUE)

  if (length(target_cols) == 0) {
    return(list())
  }

  saved_levels <- purrr::map(
    target_cols,
    ~ {
      if (is.factor(x[[.x]])) {
        levels(x[[.x]])
      } else {
        unique(x[[.x]])
      }
    }
  )

  return(stats::setNames(saved_levels, target_cols))
}

#' Restore Factor Levels and Optionally Reorder Rows
#'
#' Re-applies saved factor levels back onto target columns, optionally
#' physically sorts the rows by the restored factor order, and optionally
#' converts the columns back to character vectors.
#'
#' @param x a data frame or tibble.
#' @param saved_levels a named list of character vectors representing factor levels
#'   (typically created by \code{\link{save_column_levels}}).
#' @param arrange logical. If \code{TRUE} (default), physically reorders rows in
#'   \code{x} to match the restored factor level sequence.
#' @param to_character logical. If \code{TRUE}, converts the target columns back to
#'   character vectors after restoring levels and sorting. Defaults to \code{FALSE}.
#'
#' @return a data frame with restored factor (or character) columns and
#'   optionally reordered rows.
#' @export
#' @importFrom dplyr mutate across all_of arrange cur_column
restore_column_levels <- function(x, saved_levels, arrange = TRUE, to_character = FALSE) {
  target_cols <- intersect(names(saved_levels), colnames(x))

  if (length(target_cols) > 0) {
    # 1. Restore factor metadata
    x <- x %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(target_cols),
          ~ factor(.x, levels = saved_levels[[dplyr::cur_column()]])
        )
      )

    # 2. Optionally reorder physical rows to match the restored levels
    if (arrange) {
      x <- x %>%
        dplyr::arrange(dplyr::across(dplyr::all_of(target_cols)))
    }

    # 3. Optionally convert columns back to character vector class
    if (to_character) {
      x <- x %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(target_cols),
            as.character
          )
        )
    }
  }
  return(x)
}
