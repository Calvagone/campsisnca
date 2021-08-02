
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
#----                              getUnit                                  ----
#_______________________________________________________________________________

#' Get the unit corresponding to the given metric.
#' 
#' @param object any object that contains units
#' @param metric given metric name
#' @export
#' @rdname getUnit
getUnit <- function(object, metric, ...) {
  stop("No default function is provided")
}

setGeneric("getUnit", function(object, metric, ...) {
  standardGeneric("getUnit")
})
