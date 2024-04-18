#_______________________________________________________________________________
#----                          thalf_metric class                             ----
#_______________________________________________________________________________

validateThalfMetric <- function(object) {
  return(TRUE)
}

#' 
#' Thalf metric class.
#' 
#' @export
setClass(
  "thalf_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateThalfMetric
)

#' 
#' Terminal half life computed by making a linear regression in the log domain 
#' on the given data x.
#' 
#' @inheritParams metricsParams
#' @export
Thalf <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=getStatDisplayDefault(), digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="thalf_metric", def_name="thalf")
  return(metric)
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("thalf_metric", "numeric", "numeric"), definition=function(object, time, value) {
  linearMod <- lm(log(value) ~ time)
  k <- -linearMod$coefficients[["time"]]
  return(log(2)/k)    
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("thalf_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "half", "\U00BD"))
})

