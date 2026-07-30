no_default_function_provided_debug <- function(args_list, fun_name) {
  # Get the class of each argument as a string (fixed CHARACTER to character)
  arg_classes <- vapply(
    args_list,
    function(x) {
      if (is.null(x)) "NULL" else paste(class(x), collapse = "/")
    },
    character(1)
  )

  # Format into a readable string: "arg1 (class), arg2 (class), ..."
  formatted_args <- sprintf("%s (%s)", names(arg_classes), arg_classes)
  error_details <- paste(formatted_args, collapse = "\n  ")

  stop(
    paste0(
      "Generic '",
      fun_name,
      "' function cannot be called directly.\n",
      "Received arguments:\n  ",
      error_details
    ),
    call. = FALSE
  )
}

#_______________________________________________________________________________
#----                          apply_time_window                            ----
#_______________________________________________________________________________

#' Apply time window.
#'
#' @param x input data for the calculation, data frame
#' @param window time window
#' @param data_time_unit time unit of TIME column in data (x argument)
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname apply_time_window
apply_time_window <- function(x, window, data_time_unit, ...) {
  stop("No default function is provided")
}

setGeneric("apply_time_window", function(x, window, data_time_unit, ...) {
  standardGeneric("apply_time_window")
})

#_______________________________________________________________________________
#----                             calculate                                 ----
#_______________________________________________________________________________

#' Calculate.
#'
#' @param object object (NCA table, NCA analyses, NCA analysis, PK metric) where calculation is applied
#' @param x input data for the calculation, data frame
#' @param options NCA options
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname calculate
calculate <- function(object, x, options, ...) {
  no_default_function_provided_debug(mget(names(formals()), envir = environment()), "calculate")
}

setGeneric("calculate", function(object, x, options = NULL, ...) {
  if (is.null(options)) {
    options <- UndefinedNCAOptions()
  }
  standardGeneric("calculate")
})


#_______________________________________________________________________________
#----                              i_value                                  ----
#_______________________________________________________________________________

#' Compute the individual value of an individual.
#'
#' @param object PK metric
#' @param time time vector, numeric
#' @param value value vector, numeric
#' @return individual value
#' @export
#' @rdname i_value
i_value <- function(object, time, value) {
  stop("No default function is provided")
}

setGeneric("i_value", function(object, time, value = NULL) {
  assertthat::assert_that(length(time) == length(value), msg = "time and value must be the same length")
  assertthat::assert_that(length(value) > 0, msg = "value should contain at least 1 value")
  standardGeneric("i_value")
})

#_______________________________________________________________________________
#----                           i_value_tbl                                 ----
#_______________________________________________________________________________

#' Compute the individual value of an individual.
#'
#' @param object PK metric
#' @param data individual data, tibble
#' @param ... extra arguments
#' @return individual value
#' @export
#' @rdname i_value_tbl
i_value_tbl <- function(object, data, ...) {
  stop("No default function is provided")
}

setGeneric("i_value_tbl", function(object, data, ...) {
  standardGeneric("i_value_tbl")
})

#_______________________________________________________________________________
#----                             i_values                                  ----
#_______________________________________________________________________________

#' Compute the individual values on a simulation output.
#'
#' @param object PK metric
#' @param x input data for the calculation, data frame
#' @param options NCA options
#' @param strat_vars stratification variable names
#' @param ... extra arguments
#' @return individual values
#' @export
#' @rdname i_values
i_values <- function(object, x, options, strat_vars, ...) {
  stop("No default function is provided")
}

setGeneric("i_values", function(object, x, options, strat_vars, ...) {
  standardGeneric("i_values")
})

#_______________________________________________________________________________
#----                      generate_table_code                              ----
#_______________________________________________________________________________

#' Generate table code.
#'
#' @param object table object
#' @param init generate initialization code to generate the individuals, default is TRUE
#' @param ... extra arguments
#' @export
#' @rdname generate_table_code
generate_table_code <- function(object, init, ...) {
  stop("No default function is provided")
}

setGeneric("generate_table_code", function(object, init = NULL, ...) {
  if (is.null(init)) {
    init <- TRUE
  }
  standardGeneric("generate_table_code")
})

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' Get the name of the metric in LaTeX notation (with subscript coded with an underscore and brackets).
#'
#' @param x metric
#' @param ... extra arguments, not used
#' @export
#' @rdname get_latex_name
get_latex_name <- function(x, ...) {
  stop("No default function is provided")
}

setGeneric("get_latex_name", function(x, ...) {
  standardGeneric("get_latex_name")
})

#_______________________________________________________________________________
#----                            get_strata                                 ----
#_______________________________________________________________________________

#' Get all stratas.
#'
#' @param object table object
#' @param keep_single keep single stratification values, logical (default TRUE)
#' @param ... extra arguments
#' @return list of stratification variable names
#' @export
#' @rdname get_strata
get_strata <- function(object, keep_single, ...) {
  stop("No default function is provided")
}

setGeneric("get_strata", function(object, keep_single = NULL, ...) {
  if (is.null(keep_single)) {
    keep_single <- TRUE
  }
  standardGeneric("get_strata")
})

#_______________________________________________________________________________
#----                              get_unit                                 ----
#_______________________________________________________________________________

#' Get the unit corresponding to the given metric.
#'
#' @param object any object that contains units
#' @param metric given metric name
#' @param ... extra arguments, not used
#' @export
#' @rdname get_unit
get_unit <- function(object, metric, ...) {
  stop("No default function is provided")
}

setGeneric("get_unit", function(object, metric, ...) {
  standardGeneric("get_unit")
})

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' Get default name.
#'
#' @param object get default name of this object
#' @param ... optional extra arguments
#' @export
#' @rdname get_default_name
get_default_name <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("get_default_name", function(object, ...) {
  standardGeneric("get_default_name")
})

#_______________________________________________________________________________
#----                        stat_display_string                            ----
#_______________________________________________________________________________

#' Return the evaluated statistics display string. This method was kept for
#' backward compatibility in the tests. It is recommended to call `export(dest="dataframe", type="summary_pretty")`
#' on the NCA table instead.
#'
#' @param object PK metric
#' @param ... extra arguments
#' @return a string, e.g. 100 [45-143]
#' @export
#' @rdname stat_display_string
stat_display_string <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("stat_display_string", function(object, ...) {
  standardGeneric("stat_display_string")
})
