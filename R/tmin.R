#_______________________________________________________________________________
#----                          tmin_metric class                            ----
#_______________________________________________________________________________

validateTminMetric <- function(object) {
  return(TRUE)
}

#' 
#' Tmin metric class.
#' 
#' @export
setClass(
  "tmin_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateTminMetric
)

#' 
#' Tmin.
#' 
#' @inheritParams metricsParams
#' @export
Tmin <- function(variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="tmin_metric")
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
  return(time[which.min(value)])    
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

