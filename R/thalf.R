#_______________________________________________________________________________
#----                          thalf_metric class                             ----
#_______________________________________________________________________________

validate_thalf_metric <- function(object) {
  return(TRUE)
}

#' 
#' Thalf metric class.
#' 
#' @export
setClass(
  "thalf_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validate_thalf_metric
)

#' 
#' Terminal half life computed by making a linear regression in the log domain 
#' on the given data x.
#' 
#' @inheritParams metrics_params
#' @export
Thalf <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- nca_constructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="thalf_metric")
  return(set_default_name_if_na(metric))
}

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature=c("thalf_metric"), definition=function(object, ...) {
  return("thalf") 
})


#_______________________________________________________________________________
#----                            i_value                                    ----
#_______________________________________________________________________________

#' @rdname i_value
setMethod("i_value", signature=c("thalf_metric", "numeric", "numeric"), definition=function(object, time, value) {
  linearMod <- lm(log(value) ~ time)
  k <- -linearMod$coefficients[["time"]]
  return(log(2)/k)    
})

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature=c("thalf_metric"), definition = function(x) {
  return(subscript_occurrence(x %>% get_name(), "half", "\U00BD"))
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature=c("thalf_metric", "json_element"), definition=function(object, json) {
  return(load_metric_from_json(object=object, json=json))
})

