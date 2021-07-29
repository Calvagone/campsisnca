
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
calculate <- function(object, level=0.9, ...) {
  stop("No default function is provided")
}

setGeneric("calculate", function(object, level=0.9, ...) {
  standardGeneric("calculate")
})
