#_______________________________________________________________________________
#----                   conc at/value at metric classes                     ----
#_______________________________________________________________________________

validate_value_at_metric <- function(object) {
  return(expect_one(object, "observed_time"))
}

#'
#' Abstract value at metric class.
#'
#' @export
setClass(
  "abstract_value_at_metric",
  representation(
    observed_time = "numeric"
  ),
  contains = "nca_metric",
  prototype = prototype(observed_time = as.numeric(NA)),
  validity = validate_value_at_metric
)

#'
#' Value at metric class.
#'
#' @export
setClass(
  "value_at_metric",
  representation(),
  contains = "abstract_value_at_metric"
)

#'
#' Concentration at metric class.
#'
#' @export
setClass(
  "conc_at_metric",
  representation(),
  contains = "abstract_value_at_metric"
)

#'
#' Value at (value at specific time).
#'
#' @inheritParams metrics_params
#' @param time what time to read the values. If not provided, last values from x will be returned.
#' @export
ValueAt <- function(
  variable = NULL,
  window = NULL,
  time = NULL,
  name = NULL,
  unit = NULL,
  stat_display = NULL,
  digits = NULL
) {
  metric <- nca_constructor(
    variable = variable,
    window = window,
    name = name,
    unit = unit,
    stat_display = stat_display,
    digits = digits,
    metric_name = "value_at_metric"
  )
  metric@observed_time <- ifelse(is.null(time), as.numeric(NA), time)
  return(set_default_name_if_na(metric))
}


#'
#' CAt (concentration at specific time).
#'
#' @inheritParams metrics_params
#' @param time what time to read the concentrations. If not provided, last concentrations from x will be returned.
#' @export
CAt <- function(
  variable = NULL,
  window = NULL,
  time = NULL,
  name = NULL,
  unit = NULL,
  stat_display = NULL,
  digits = NULL
) {
  metric <- nca_constructor(
    variable = variable,
    window = window,
    name = name,
    unit = unit,
    stat_display = stat_display,
    digits = digits,
    metric_name = "conc_at_metric"
  )
  metric@observed_time <- ifelse(is.null(time), as.numeric(NA), time)
  return(set_default_name_if_na(metric))
}

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature = c("value_at_metric"), definition = function(object, ...) {
  return("Value")
})

#' @rdname get_default_name
setMethod("get_default_name", signature = c("conc_at_metric"), definition = function(object, ...) {
  return("Conc")
})

#_______________________________________________________________________________
#----                            i_value                                    ----
#_______________________________________________________________________________

#' @rdname i_value
setMethod(
  "i_value",
  signature = c("abstract_value_at_metric", "numeric", "numeric"),
  definition = function(object, time, value) {
    observed_time <- object@observed_time
    if (is.na(observed_time)) {
      return(value[length(value)])
    } else {
      index <- which(time == observed_time)
      if (length(index) == 0) {
        stop(paste0("Could not find any sample at t=", observed_time))
      } else {
        return(value[index[1]])
      }
    }
  }
)

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature = c("abstract_value_at_metric"), definition = function(x) {
  retValue <- x %>% get_name()
  return(retValue)
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod(
  "load_from_json",
  signature = c("abstract_value_at_metric", "json_element"),
  definition = function(object, json) {
    return(load_metric_from_json(object = object, json = json))
  }
)
