#_______________________________________________________________________________
#----                         (c)max_metric class                           ----
#_______________________________________________________________________________

validateMaxMetric <- function(object) {
  return(TRUE)
}

#' 
#' Abstract max metric class.
#' 
#' @export
setClass(
  "abstract_max_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateMaxMetric
)

#' 
#' Max metric class.
#' 
#' @export
setClass(
  "max_metric",
  representation(
  ),
  contains="abstract_max_metric"
)

#' 
#' Cmax metric class.
#' 
#' @export
setClass(
  "cmax_metric",
  representation(
  ),
  contains="abstract_max_metric"
)

#' 
#' Max.
#' 
#' @inheritParams metricsParams
#' @export
Max <- function(variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="max_metric")
  return(setDefaultNameIfNA(metric))
}

#' 
#' Cmax.
#' 
#' @inheritParams metricsParams
#' @export
Cmax <- function(variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cmax_metric")
  return(setDefaultNameIfNA(metric))
}

#_______________________________________________________________________________
#----                           getDefaultName                              ----
#_______________________________________________________________________________

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("max_metric"), definition=function(object, ...) {
  return("Max")
})

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("cmax_metric"), definition=function(object, ...) {
  return("Cmax")
})

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("abstract_max_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(max(value))    
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("abstract_max_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "max"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("abstract_max_metric", "json_element"), definition=function(object, json) {
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(setDefaultNameIfNA(object))
})

