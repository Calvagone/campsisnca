#_______________________________________________________________________________
#----                          tmax_metric class                            ----
#_______________________________________________________________________________

validateTmaxMetric <- function(object) {
  return(expectOne(object, "rebase"))
}

#' 
#' Tmax metric class.
#' 
#' @export
setClass(
  "tmax_metric",
  representation(
    rebase="logical"
  ),
  contains="nca_metric",
  prototype=prototype(rebase=TRUE),
  validity=validateTmaxMetric
)

#' 
#' Tmax.
#' 
#' @inheritParams metricsParams
#' @param rebase rebase time according to start time of window
#' @export
Tmax <- function(variable=NULL, window=NULL, rebase=TRUE, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="tmax_metric")
  metric@rebase <- rebase
  return(setDefaultNameIfNA(metric))
}

#_______________________________________________________________________________
#----                           getDefaultName                              ----
#_______________________________________________________________________________

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("tmax_metric"), definition=function(object, ...) {
  return("tmax") 
})

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("tmax_metric", "numeric", "numeric"), definition=function(object, time, value) {
  retValue <- time[which.max(value)]
  if (object@rebase) {
    retValue <- retValue - object@window@start
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("tmax_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "max"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("tmax_metric", "json_element"), definition=function(object, json) {
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(setDefaultNameIfNA(object))
})

