#_______________________________________________________________________________
#----                       (c)avg_metric class                             ----
#_______________________________________________________________________________

validate_avg_metric <- function(object) {
  return(TRUE)
}

#'
#' Abstract avg metric class.
#'
#' @export
setClass(
  "abstract_avg_metric",
  representation(),
  contains = "nca_metric",
  validity = validate_avg_metric
)

#'
#' Avg metric class.
#'
#' @export
setClass(
  "avg_metric",
  representation(),
  contains = "abstract_avg_metric"
)

#'
#' Cavg metric class.
#'
#' @export
setClass(
  "cavg_metric",
  representation(),
  contains = "abstract_avg_metric"
)

#'
#' Avg.
#'
#' @inheritParams metrics_params
#' @export
Avg <- function(variable = NULL, window = NULL, name = NULL, unit = NULL, stat_display = NULL, digits = NULL) {
  metric <- nca_constructor(
    variable = variable,
    window = window,
    name = name,
    unit = unit,
    stat_display = stat_display,
    digits = digits,
    metric_name = "avg_metric"
  )
  return(set_default_name_if_na(metric))
}

#'
#' Cavg.
#'
#' @inheritParams metrics_params
#' @export
Cavg <- function(variable = NULL, window = NULL, name = NULL, unit = NULL, stat_display = NULL, digits = NULL) {
  metric <- nca_constructor(
    variable = variable,
    window = window,
    name = name,
    unit = unit,
    stat_display = stat_display,
    digits = digits,
    metric_name = "cavg_metric"
  )
  return(set_default_name_if_na(metric))
}

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature = c("avg_metric"), definition = function(object, ...) {
  return("Avg")
})

#' @rdname get_default_name
setMethod("get_default_name", signature = c("cavg_metric"), definition = function(object, ...) {
  return("Cavg")
})

#_______________________________________________________________________________
#----                            i_value                                    ----
#_______________________________________________________________________________

#' @rdname i_value
setMethod(
  "i_value",
  signature = c("abstract_avg_metric", "numeric", "numeric"),
  definition = function(object, time, value) {
    start <- time[1]
    end <- time[length(time)]
    auc <- trap(x = time, y = value, method = 1L)
    return(auc / (end - start))
  }
)

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature = c("abstract_avg_metric"), definition = function(x) {
  return(subscript_occurrence(x %>% get_name(), "avg"))
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature = c("abstract_avg_metric", "json_element"), definition = function(object, json) {
  return(load_metric_from_json(object = object, json = json))
})
