#_______________________________________________________________________________
#----                          cmax_metric class                             ----
#_______________________________________________________________________________

validateCmaxMetric <- function(object) {
  return(TRUE)
}

#' 
#' Cmax metric class.
#' 
#' @export
setClass(
  "cmax_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateCmaxMetric
)

#' 
#' Cmax.
#' 
#' @inheritParams metricsParams
#' @export
Cmax <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=getStatDisplayDefault(), digits=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "Cmax" else name
  unit <- processUnit(unit)
  digits <- deparseDigits(digits)
  return(new("cmax_metric", x=x, variable=variable, name=name, unit=unit,
             stat_display=stat_display, digits=digits))
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("cmax_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(max(value))    
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("nca_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "max"))
})
