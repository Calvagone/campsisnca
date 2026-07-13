#_______________________________________________________________________________
#----                          tmin_metric class                            ----
#_______________________________________________________________________________

validate_tmin_metric <- function(object) {
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
  validity=validate_tmin_metric
)

#' 
#' Tmin.
#' 
#' @inheritParams metrics_params
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
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature=c("tmin_metric"), definition=function(object, ...) {
  return("tmin") 
})

#_______________________________________________________________________________
#----                            i_value                                    ----
#_______________________________________________________________________________

#' @rdname i_value
setMethod("i_value", signature=c("tmin_metric", "numeric", "numeric"), definition=function(object, time, value) {
  retValue <- time[which.min(value)]
  if (object@rebase) {
    retValue <- retValue - object@window@start
  }
  return(retValue)    
})

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature=c("tmin_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "min"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("tmin_metric", "json_element"), definition=function(object, json) {
  return(loadMetricFromJSON(object=object, json=json))
})

