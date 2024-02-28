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
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("cmin_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(min(value))    
})
