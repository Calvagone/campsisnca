#_______________________________________________________________________________
#----                          tmax_metric class                            ----
#_______________________________________________________________________________

validate_tmax_metric <- function(object) {
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
  validity=validate_tmax_metric
)

#' 
#' Tmax.
#' 
#' @inheritParams metrics_params
#' @param rebase rebase time according to start time of window
#' @export
Tmax <- function(variable=NULL, window=NULL, rebase=TRUE, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- nca_constructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="tmax_metric")
  metric@rebase <- rebase
  return(set_default_name_if_na(metric))
}

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature=c("tmax_metric"), definition=function(object, ...) {
  return("tmax") 
})

#_______________________________________________________________________________
#----                            i_value                                    ----
#_______________________________________________________________________________

#' @rdname i_value
setMethod("i_value", signature=c("tmax_metric", "numeric", "numeric"), definition=function(object, time, value) {
  retValue <- time[which.max(value)]
  if (object@rebase) {
    retValue <- retValue - object@window@start
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature=c("tmax_metric"), definition = function(x) {
  return(subscript_occurrence(x %>% getName(), "max"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("tmax_metric", "json_element"), definition=function(object, json) {
  return(loadMetricFromJSON(object=object, json=json))
})

