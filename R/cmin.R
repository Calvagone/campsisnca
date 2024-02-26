#_______________________________________________________________________________
#----                          cmin_metric class                            ----
#_______________________________________________________________________________

validateCminMetric <- function(object) {
  return(TRUE)
}

#' 
#' Cmin metric class.
#' 
#' @export
setClass(
  "cmin_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateCminMetric
)

#' 
#' Cmin.
#' 
#' @inheritParams metricsParams
#' @export
Cmin <- function(x=NULL, variable=NULL, name=NULL, unit=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "Cmin" else name
  unit <- processUnit(unit)
  return(new("cmin_metric", x=x, variable=variable, name=name, unit=unit))
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("cmin_metric", "numeric"), definition=function(object, level, ...) {
  object@individual <- computeIndividualValues(metric=object)
  return(object %>% summariseIndividualData(level=level))    
})

#_______________________________________________________________________________
#----                             core                                      ----
#_______________________________________________________________________________

#' @rdname core
setMethod("core", signature=c("cmin_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(min(value))    
})
