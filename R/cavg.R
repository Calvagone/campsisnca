#_______________________________________________________________________________
#----                       (c)avg_metric class                             ----
#_______________________________________________________________________________

validateAvgMetric <- function(object) {
  return(TRUE)
}

#' 
#' Abstract avg metric class.
#' 
#' @export
setClass(
  "abstract_avg_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateAvgMetric
)

#' 
#' Avg metric class.
#' 
#' @export
setClass(
  "avg_metric",
  representation(
  ),
  contains="abstract_avg_metric"
)

#' 
#' Cavg metric class.
#' 
#' @export
setClass(
  "cavg_metric",
  representation(
  ),
  contains="abstract_avg_metric"
)

#' 
#' Avg.
#' 
#' @inheritParams metricsParams
#' @export
Avg <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="avg_metric")
  return(setDefaultNameIfNA(metric))
}

#' 
#' Cavg.
#' 
#' @inheritParams metricsParams
#' @export
Cavg <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="cavg_metric")
  return(setDefaultNameIfNA(metric))
}

#_______________________________________________________________________________
#----                           getDefaultName                              ----
#_______________________________________________________________________________

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("avg_metric"), definition=function(object, ...) {
  return("Avg")
})

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("cavg_metric"), definition=function(object, ...) {
  return("Cavg")
})

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("abstract_avg_metric", "numeric", "numeric", "nca_time_window"), definition=function(object, time, value, window) {
  start <- time[1]
  end <- time[length(time)]
  auc <- trap(x=time, y=value, method=1L)
  return(auc/(end - start))    
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("abstract_avg_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "avg"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("abstract_avg_metric", "json_element"), definition=function(object, json) {
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(setDefaultNameIfNA(object))
})
