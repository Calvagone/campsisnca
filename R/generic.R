
#_______________________________________________________________________________
#----                             calculate                                 ----
#_______________________________________________________________________________

#' Calculate.
#' 
#' @param object object (PK metric) that needs to be calculated
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname calculate
calculate <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("calculate", function(object, ...) {
  standardGeneric("calculate")
})
