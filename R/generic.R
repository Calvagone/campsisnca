#_______________________________________________________________________________
#----                           applyTimeWindow                             ----
#_______________________________________________________________________________

#' Apply time window.
#' 
#' @param x input data for the calculation, data frame
#' @param window time window
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname applyTimeWindow
applyTimeWindow <- function(x, window, ...) {
  stop("No default function is provided")
}

setGeneric("applyTimeWindow", function(x, window, ...) {
  standardGeneric("applyTimeWindow")
})

#_______________________________________________________________________________
#----                             calculate                                 ----
#_______________________________________________________________________________

#' Calculate.
#' 
#' @param object object (PK metric) that needs to be calculated
#' @param x input data for the calculation, data frame
#' @param strat_vars stratification variable names
#' @param quantile_type type of quantile to use (see ?quantile), default value in campsisnca is 2 (aligned with gtsummary)
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname calculate
calculate <- function(object, x, strat_vars, quantile_type, ...) {
  stop("No default function is provided")
}

setGeneric("calculate", function(object, x, strat_vars=NULL, quantile_type=NULL, ...) {
  if (is.null(quantile_type)) {
    quantile_type <- 2
  }
  if (is.null(strat_vars)) {
    strat_vars <- character(0)
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
#' @param strat_vars stratification variable names
#' @param ... extra arguments
#' @return individual values
#' @export
#' @rdname iValues
iValues <- function(object, x, strat_vars, ...) {
  stop("No default function is provided")
}

setGeneric("iValues", function(object, x, strat_vars, ...) {
  standardGeneric("iValues")
})

#_______________________________________________________________________________
#----                       generateTableCode                               ----
#_______________________________________________________________________________

#' Generate table code.
#' 
#' @param object table object
#' @param init generate initialization code to generate the individuals, default is TRUE
#' @param subscripts use LaTeX subcripts/superscripts notation when writing labels
#' @param all_dichotomous_levels show all dichotomous levels (0 and 1) when data is dichotomous, default is FALSE
#' @param combine_with either 'tbl_stack' or 'tbl_merge'
#' @param header_label header label name
#' @param ... extra arguments
#' @export
#' @rdname generateTableCode
generateTableCode <- function(object, init, subscripts, all_dichotomous_levels, combine_with, header_label,  ...) {
  stop("No default function is provided")
}

setGeneric("generateTableCode", function(object, init=NULL, subscripts=NULL, all_dichotomous_levels=NULL, combine_with=NULL, header_label=NULL, ...) {
  if (is.null(init)) {
    init <- TRUE
  }
  if (is.null(subscripts)) {
    subscripts <- FALSE
  }
  if (is.null(all_dichotomous_levels)) {
    all_dichotomous_levels <- FALSE
  }
  if (is.null(combine_with)) {
    combine_with <- "tbl_stack"
  }
  if (is.null(header_label)) {
    header_label <- "Metric"
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

#' Return the evaluated statistics display string.
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
