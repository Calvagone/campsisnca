#_______________________________________________________________________________
#----                         cavg_metric class                             ----
#_______________________________________________________________________________

validateCavgMetric <- function(object) {
  return(TRUE)
}

#' 
#' Cavg metric class.
#' 
#' @export
setClass(
  "cavg_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateCavgMetric
)

#' 
#' Cavg.
#' 
#' @inheritParams metricsParams
#' @export
Cavg <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=getStatDisplayDefault(), digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cavg_metric", def_name="Cavg")
  metric@concentration <- TRUE
  return(metric)
}

#' 
#' Avg.
#' 
#' @inheritParams metricsParams
#' @export
Avg <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=getStatDisplayDefault(), digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cavg_metric", def_name="Avg")
  metric@concentration <- FALSE
  return(metric)
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("cavg_metric", "numeric", "numeric"), definition=function(object, time, value) {
  start <- time[1]
  end <- time[length(time)]
  auc <- trap(x=time, y=value, method=1L)
  return(auc/(end - start))    
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("cavg_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "avg"))
})

