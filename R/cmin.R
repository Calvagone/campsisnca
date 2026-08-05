#_______________________________________________________________________________
#----                         (c)min_metric classes                         ----
#_______________________________________________________________________________

validate_min_metric <- function(object) {
  return(TRUE)
}

#'
#' Abstract min metric class.
#'
#' @export
setClass(
  "abstract_min_metric",
  representation(),
  contains = "nca_metric",
  validity = validate_min_metric
)

#'
#' Min metric class.
#'
#' @export
setClass(
  "min_metric",
  representation(),
  contains = "abstract_min_metric"
)

#'
#' Cmin metric class.
#'
#' @export
setClass(
  "cmin_metric",
  representation(),
  contains = "abstract_min_metric"
)

#'
#' Min.
#'
#' @inheritParams metrics_params
#' @export
Min <- function(variable = NULL, window = NULL, name = NULL, unit = NULL, stat_display = NULL, digits = NULL) {
  metric <- nca_constructor(
    variable = variable,
    window = window,
    name = name,
    unit = unit,
    stat_display = stat_display,
    digits = digits,
    metric_name = "min_metric"
  )
  return(set_default_name_if_na(metric))
}

#'
#' Cmin
#'
#' @inheritParams metrics_params
#' @export
Cmin <- function(variable = NULL, window = NULL, name = NULL, unit = NULL, stat_display = NULL, digits = NULL) {
  metric <- nca_constructor(
    variable = variable,
    window = window,
    name = name,
    unit = unit,
    stat_display = stat_display,
    digits = digits,
    metric_name = "cmin_metric"
  )
  return(set_default_name_if_na(metric))
}

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature = c("min_metric"), definition = function(object, ...) {
  return("Min")
})

#' @rdname get_default_name
setMethod("get_default_name", signature = c("cmin_metric"), definition = function(object, ...) {
  return("Cmin")
})

#_______________________________________________________________________________
#----                            i_value                                    ----
#_______________________________________________________________________________

#' @rdname i_value
setMethod(
  "i_value",
  signature = c("abstract_min_metric", "numeric", "numeric"),
  definition = function(object, time, value) {
    return(min(value))
  }
)

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature = c("abstract_min_metric"), definition = function(x) {
  return(subscript_occurrence(x %>% get_name(), "min"))
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("abstract_min_metric", "json_element"), definition = function(object, json) {
  return(load_metric_from_json(object = object, json = json))
})
