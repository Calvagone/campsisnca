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
Tmax <- function(variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="tmax_metric")
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
  return(time[which.max(value)])    
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

