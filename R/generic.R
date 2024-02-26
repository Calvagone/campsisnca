
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
#----                               core                                    ----
#_______________________________________________________________________________

#' Core function for the evaluation of NCA metrics.
#' 
#' @param object object (PK metric) that needs to be calculated
#' @param time time vector, numeric
#' @param value value vector, numeric
#' @param ... extra arguments
#' @return results vector, numeric
#' @export
#' @rdname core
core <- function(object, time, value, ...) {
  stop("No default function is provided")
}

setGeneric("core", function(object, time, value, ...) {
  assertthat::assert_that(length(time)==length(value), msg="time and value must be the same length")
  assertthat::assert_that(length(value) > 0, msg="value should contain at least 1 value")
  standardGeneric("core")
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
