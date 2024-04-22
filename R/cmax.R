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
Cmax <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cmax_metric", def_name="Cmax")
  metric@concentration <- TRUE
  return(metric)
}

#' 
#' Max.
#' 
#' @inheritParams metricsParams
#' @export
Max <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cmax_metric", def_name="Max")
  metric@concentration <- FALSE
  return(metric)
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
