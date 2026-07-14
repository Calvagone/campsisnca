#_______________________________________________________________________________
#----                       (c)max_metric classes                           ----
#_______________________________________________________________________________

validate_max_metric <- function(object) {
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
  validity=validate_max_metric
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
#' @inheritParams metrics_params
#' @export
Max <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- nca_constructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="max_metric")
  return(set_default_name_if_na(metric))
}

#' 
#' Cmax.
#' 
#' @inheritParams metrics_params
#' @export
Cmax <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- nca_constructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="cmax_metric")
  return(set_default_name_if_na(metric))
}

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature=c("max_metric"), definition=function(object, ...) {
  return("Max")
})

#' @rdname get_default_name
setMethod("get_default_name", signature=c("cmax_metric"), definition=function(object, ...) {
  return("Cmax")
})

#_______________________________________________________________________________
#----                            i_value                                    ----
#_______________________________________________________________________________

#' @rdname i_value
setMethod("i_value", signature=c("abstract_max_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(max(value))    
})

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature=c("abstract_max_metric"), definition = function(x) {
  return(subscript_occurrence(x %>% getName(), "max"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("abstract_max_metric", "json_element"), definition=function(object, json) {
  return(loadMetricFromJSON(object=object, json=json))
})

