#_______________________________________________________________________________
#----                          tmin_metric class                            ----
#_______________________________________________________________________________

validateTminMetric <- function(object) {
  return(expectOne(object, "rebase"))
}

#' 
#' Tmin metric class.
#' 
#' @export
setClass(
  "tmin_metric",
  representation(
    rebase="logical"
  ),
  contains="nca_metric",
  prototype=prototype(rebase=TRUE),
  validity=validateTminMetric
)

#' 
#' Tmin.
#' 
#' @inheritParams metricsParams
#' @param rebase rebase time according to start time of window
#' @export
Tmin <- function(variable=NULL, window=NULL, rebase=TRUE, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="tmin_metric")
  metric@rebase <- rebase
  return(setDefaultNameIfNA(metric))
}

#_______________________________________________________________________________
#----                           getDefaultName                              ----
#_______________________________________________________________________________

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("tmin_metric"), definition=function(object, ...) {
  return("tmin") 
})

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("tmin_metric", "numeric", "numeric"), definition=function(object, time, value) {
  retValue <- time[which.min(value)]
  if (object@rebase) {
    retValue <- retValue - object@window@start
  }
  return(retValue)    
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("tmin_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "min"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("tmin_metric", "json_element"), definition=function(object, json) {
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(setDefaultNameIfNA(object))
})

