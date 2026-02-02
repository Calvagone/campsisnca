#_______________________________________________________________________________
#----                       ctrough / last metric classes                   ----
#_______________________________________________________________________________

#' 
#' Abstract last metric class.
#' 
#' @export
setClass(
  "abstract_last_metric",
  representation(
  ),
  contains="nca_metric"
)

#' 
#' Last metric class.
#' 
#' @export
setClass(
  "last_metric",
  representation(
  ),
  contains="abstract_last_metric"
)

#' 
#' Ctrough metric class.
#' 
#' @export
setClass(
  "ctrough_metric",
  representation(
  ),
  contains="abstract_last_metric"
)

#' 
#' Last value.
#' 
#' @inheritParams metricsParams
#' @export
Last <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="last_metric")
  return(setDefaultNameIfNA(metric))
}

#' 
#' Ctrough. Last time in x shall be considered as the trough time. Similar to Last, but for concentrations.
#' 
#' @inheritParams metricsParams
#' @export
Ctrough <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="ctrough_metric")
  return(setDefaultNameIfNA(metric))
}

#_______________________________________________________________________________
#----                           getDefaultName                              ----
#_______________________________________________________________________________

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("last_metric"), definition=function(object, ...) {
  return("Last value")
})

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("ctrough_metric"), definition=function(object, ...) {
  return("Ctrough")
})

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("abstract_last_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(value[length(value)])
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("last_metric"), definition = function(x) {
    retValue <- x %>% getName()
    return(retValue)
})

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("ctrough_metric"), definition = function(x) {
  retValue <- x %>% getName()
  return(subscriptOccurrence(retValue, "trough"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("abstract_last_metric", "json_element"), definition=function(object, json) {
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(setDefaultNameIfNA(object))
})


