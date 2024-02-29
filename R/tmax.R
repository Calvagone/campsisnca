#_______________________________________________________________________________
#----                          tmax_metric class                            ----
#_______________________________________________________________________________

validateTmaxMetric <- function(object) {
  return(TRUE)
}

#' 
#' Tmax metric class.
#' 
#' @export
setClass(
  "tmax_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateTmaxMetric
)

#' 
#' Tmax.
#' 
#' @inheritParams metricsParams
#' @export
Tmax <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=getStatDisplayDefault()) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "tmax" else name
  unit <- processUnit(unit)
  return(new("tmax_metric", x=x, variable=variable, name=name, unit=unit,
             stat_display=stat_display))
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("tmax_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(time[which.max(value)])    
})
