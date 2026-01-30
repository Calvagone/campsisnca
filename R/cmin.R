#_______________________________________________________________________________
#----                         (c)min_metric classes                         ----
#_______________________________________________________________________________

validateMinMetric <- function(object) {
  return(TRUE)
}

#' 
#' Abstract min metric class.
#' 
#' @export
setClass(
  "abstract_min_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateMinMetric
)

#' 
#' Min metric class.
#' 
#' @export
setClass(
  "min_metric",
  representation(
  ),
  contains="abstract_min_metric"
)

#' 
#' Cmin metric class.
#' 
#' @export
setClass(
  "cmin_metric",
  representation(
  ),
  contains="abstract_min_metric"
)

#' 
#' Min.
#' 
#' @inheritParams metricsParams
#' @export
Min <- function(variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="min_metric")
  return(setDefaultNameIfNA(metric))
}

#' 
#' Cmin
#' 
#' @inheritParams metricsParams
#' @export
Cmin <- function(variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cmin_metric")
  return(setDefaultNameIfNA(metric))
}

#_______________________________________________________________________________
#----                           getDefaultName                              ----
#_______________________________________________________________________________

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("min_metric"), definition=function(object, ...) {
  return("Min")
})

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("cmin_metric"), definition=function(object, ...) {
  return("Cmin")
})

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("abstract_min_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(min(value))    
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("abstract_min_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "min"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("abstract_min_metric", "json_element"), definition=function(object, json) {
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(setDefaultNameIfNA(object))
})
