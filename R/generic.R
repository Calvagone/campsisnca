no_default_function_provided_debug <- function(args_list, fun_name) {
  # Get the class of each argument as a string (fixed CHARACTER to character)
  arg_classes <- vapply(args_list, function(x) {
    if (is.null(x)) "NULL" else paste(class(x), collapse = "/")
  }, character(1))
  
  # Format into a readable string: "arg1 (class), arg2 (class), ..."
  formatted_args <- sprintf("%s (%s)", names(arg_classes), arg_classes)
  error_details <- paste(formatted_args, collapse = "\n  ")
  
  stop(paste0(
    "Generic '", fun_name, "' function cannot be called directly.\n",
    "Received arguments:\n  ", error_details
  ), call. = FALSE)
}

#_______________________________________________________________________________
#----                           applyTimeWindow                             ----
#_______________________________________________________________________________

#' Apply time window.
#' 
#' @param x input data for the calculation, data frame
#' @param window time window
#' @param data_time_unit time unit of TIME column in data (x argument)
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname applyTimeWindow
applyTimeWindow <- function(x, window, data_time_unit, ...) {
  stop("No default function is provided")
}

setGeneric("applyTimeWindow", function(x, window, data_time_unit, ...) {
  standardGeneric("applyTimeWindow")
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

setGeneric("calculate", function(object, x, options=NULL, ...) {
  if (is.null(options)) {
    options <- UndefinedNCAOptions()
  }
  standardGeneric("calculate")
})


#_______________________________________________________________________________
#----                              iValue                                   ----
#_______________________________________________________________________________

#' Compute the individual value of an individual.
#' 
#' @param object PK metric
#' @param time time vector, numeric
#' @param value value vector, numeric
#' @return individual value
#' @export
#' @rdname iValue
iValue <- function(object, time, value) {
  stop("No default function is provided")
}

setGeneric("iValue", function(object, time, value=NULL) {
  assertthat::assert_that(length(time)==length(value), msg="time and value must be the same length")
  assertthat::assert_that(length(value) > 0, msg="value should contain at least 1 value")
  standardGeneric("iValue")
})

#_______________________________________________________________________________
#----                            iValueTbl                                  ----
#_______________________________________________________________________________

#' Compute the individual value of an individual.
#' 
#' @param object PK metric
#' @param data individual data, tibble
#' @param ... extra arguments
#' @return individual value
#' @export
#' @rdname iValueTbl
iValueTbl <- function(object, data, ...) {
  stop("No default function is provided")
}

setGeneric("iValueTbl", function(object, data, ...) {
  standardGeneric("iValueTbl")
})

#_______________________________________________________________________________
#----                             iValues                                   ----
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
#' @rdname iValues
iValues <- function(object, x, options, strat_vars, ...) {
  stop("No default function is provided")
}

setGeneric("iValues", function(object, x, options, strat_vars, ...) {
  standardGeneric("iValues")
})

#_______________________________________________________________________________
#----                       generateTableCode                               ----
#_______________________________________________________________________________

#' Generate table code.
#' 
#' @param object table object
#' @param init generate initialization code to generate the individuals, default is TRUE
#' @param ... extra arguments
#' @export
#' @rdname generateTableCode
generateTableCode <- function(object, init, ...) {
  stop("No default function is provided")
}

setGeneric("generateTableCode", function(object, init=NULL, ...) {
  if (is.null(init)) {
    init <- TRUE
  }
  standardGeneric("generateTableCode")
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' Get the name of the metric in LaTeX notation (with subscript coded with an underscore and brackets).
#' 
#' @param x metric
#' @param ... extra arguments, not used
#' @export
#' @rdname getLaTeXName
getLaTeXName <- function(x, ...) {
  stop("No default function is provided")
}

setGeneric("getLaTeXName", function(x, ...) {
  standardGeneric("getLaTeXName")
})

#_______________________________________________________________________________
#----                           getScenarios                                ----
#_______________________________________________________________________________

#' Get all stratas.
#' 
#' @param object table object
#' @param keep_single keep single stratification values, logical (default TRUE)
#' @param ... extra arguments
#' @return list of stratification variable names
#' @export
#' @rdname getStrata
getStrata <- function(object, keep_single, ...) {
  stop("No default function is provided")
}

setGeneric("getStrata", function(object, keep_single=NULL, ...) {
  if (is.null(keep_single)) {
    keep_single <- TRUE
  }
  standardGeneric("getStrata")
})

#_______________________________________________________________________________
#----                              getUnit                                  ----
#_______________________________________________________________________________

#' Get the unit corresponding to the given metric.
#' 
#' @param object any object that contains units
#' @param metric given metric name
#' @param ... extra arguments, not used
#' @export
#' @rdname getUnit
getUnit <- function(object, metric, ...) {
  stop("No default function is provided")
}

setGeneric("getUnit", function(object, metric, ...) {
  standardGeneric("getUnit")
})

#_______________________________________________________________________________
#----                           getDefaultName                              ----
#_______________________________________________________________________________

#' Get default name.
#' 
#' @param object get default name of this object
#' @param ... optional extra arguments
#' @export
#' @rdname getDefaultName
getDefaultName <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("getDefaultName", function(object, ...) {
  standardGeneric("getDefaultName")
})

#_______________________________________________________________________________
#----                         statDisplayString                             ----
#_______________________________________________________________________________

#' Return the evaluated statistics display string. This method was kept for
#' backward compatibility in the tests. It is recommended to call `export(dest="dataframe", type="summary_pretty")`
#' on the NCA table instead.
#'
#' @param object PK metric
#' @param ... extra arguments
#' @return a string, e.g. 100 [45-143]
#' @export
#' @rdname statDisplayString
statDisplayString <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("statDisplayString", function(object, ...) {
  standardGeneric("statDisplayString")
})

#_______________________________________________________________________________
#----                        summarise_replicates                           ----
#_______________________________________________________________________________

#' Summarise NCA results obtained across simulation replicates.
#' 
#' @details Summarises a table of NCA metrics computed on several simulation
#' replicates (e.g. as produced by \code{\link{calculate}} on a
#' \code{summary_campsisnca_tbl}) into a single summary table. Stratification
#' variables are automatically detected as the columns of \code{x} other than
#' \code{replicate}, \code{metric}, \code{stat}, \code{value} and \code{category}.
#' @param object object (NCA table) which contains the table structure
#' @param x input data for the calculation, data frame of class 'summary_campsisnca_tbl' containing a 'replicate' column with more than one distinct value
#' @param options NCA options
#' @param ... extra arguments like 'dest' which can take the following values: "dataframe", "gtsummary" or "gt"
#' @return a summarised table, whose class depends on the 'dest' argument: a data frame ('dataframe'), a gtsummary table ('gtsummary') or a gt table ('gt')
#' @export
#' @rdname summarise_replicates
summarise_replicates <- function(object, x, options, ...) {
  no_default_function_provided_debug(mget(names(formals()), envir = environment()), "summarise_replicates")
}

setGeneric("summarise_replicates", function(object, x, options=NULL, ...) {
  if (is.null(options)) {
    options <- UndefinedNCAOptions()
  }
  standardGeneric("summarise_replicates")
})
