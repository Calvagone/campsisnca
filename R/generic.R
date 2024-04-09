
#_______________________________________________________________________________
#----                             calculate                                 ----
#_______________________________________________________________________________

#' Calculate.
#' 
#' @param object object (PK metric) that needs to be calculated
#' @param level prediction interval level, default is 0.9 (90\% prediction interval)
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname calculate
calculate <- function(object, level=NULL, ...) {
  stop("No default function is provided")
}

setGeneric("calculate", function(object, level=NULL, ...) {
  if (is.null(level)) {
    level <- 0.9
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
#' @param ... extra arguments
#' @return individual value
#' @export
#' @rdname iValue
iValue <- function(object, time, value, ...) {
  stop("No default function is provided")
}

setGeneric("iValue", function(object, time, value, ...) {
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

#' Compute the individual values of a population.
#' 
#' @param object PK metric
#' @param ... extra arguments
#' @return individual values
#' @export
#' @rdname iValues
iValues <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("iValues", function(object, ...) {
  standardGeneric("iValues")
})

#_______________________________________________________________________________
#----                       generateTableCode                               ----
#_______________________________________________________________________________

#' Generate table code.
#' 
#' @param object table object
#' @param subscripts use LaTeX subcripts/superscripts notation when writing labels
#' @param max_2dim if TRUE, table will be reduced to 2 dimensions
#' @param ... extra arguments
#' @export
#' @rdname generateTableCode
generateTableCode <- function(object, subscripts, max_2dim, ...) {
  stop("No default function is provided")
}

setGeneric("generateTableCode", function(object, subscripts=NULL, max_2dim=NULL, ...) {
  if (is.null(subscripts)) {
    subscripts <- FALSE
  }
  if (is.null(max_2dim)) {
    max_2dim <- FALSE
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

#' Get all scenarios that were added to the table object.
#' 
#' @param object table object
#' @param ... extra arguments
#' @return a dataframe with 2 columns name (stratification variable) and value (all level values)
#' @export
#' @rdname getScenarios
getScenarios <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("getScenarios", function(object, ...) {
  standardGeneric("getScenarios")
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

#_______________________________________________________________________________
#----                       reduceTo2Dimensions                             ----
#_______________________________________________________________________________

#' Reduce table to 2 dimensions.
#' 
#' @param object generic object
#' @param ... extra arguments
#' @return same object/table with max 2 dimensions (useful before export)
#' @export
#' @rdname reduceTo2Dimensions
reduceTo2Dimensions <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("reduceTo2Dimensions", function(object, ...) {
  standardGeneric("reduceTo2Dimensions")
})
