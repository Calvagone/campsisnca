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
#' @inheritParams metrics_params
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
#' @inheritParams metrics_params
#' @export
Ctrough <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="ctrough_metric")
  return(setDefaultNameIfNA(metric))
}

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature=c("last_metric"), definition=function(object, ...) {
  return("Last value")
})

#' @rdname get_default_name
setMethod("get_default_name", signature=c("ctrough_metric"), definition=function(object, ...) {
  return("Ctrough")
})

#_______________________________________________________________________________
#----                            i_value                                    ----
#_______________________________________________________________________________

#' @rdname i_value
setMethod("i_value", signature=c("abstract_last_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(value[length(value)])
})

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature=c("last_metric"), definition = function(x) {
    retValue <- x %>% getName()
    return(retValue)
})

#' @rdname get_latex_name
setMethod("get_latex_name", signature=c("ctrough_metric"), definition = function(x) {
  retValue <- x %>% getName()
  return(subscriptOccurrence(retValue, "trough"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("abstract_last_metric", "json_element"), definition=function(object, json) {
  return(loadMetricFromJSON(object=object, json=json))
})


