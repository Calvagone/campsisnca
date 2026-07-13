#_______________________________________________________________________________
#----                      baseline_metric classes                          ----
#_______________________________________________________________________________

validate_baseline_metric <- function(object) {
  return(TRUE)
}

#' 
#' Abstract baseline metric class.
#' 
#' @export
setClass(
  "abstract_baseline_metric",
  representation(),
  contains="nca_metric",
  validity=validate_baseline_metric
)

#' 
#' Absolute Change from Baseline metric class.
#' 
#' @export
setClass(
  "cfb_metric",
  representation(),
  contains="abstract_baseline_metric"
)

#' 
#' Percent Change from Baseline metric class.
#' 
#' @export
setClass(
  "pcfb_metric",
  representation(),
  contains="abstract_baseline_metric"
)

#' 
#' Ratio to Baseline metric class.
#' 
#' @export
setClass(
  "ratio_baseline_metric",
  representation(),
  contains="abstract_baseline_metric"
)

#' 
#' Log-transformed Change from Baseline metric class.
#' 
#' @export
setClass(
  "log_cfb_metric",
  representation(),
  contains="abstract_baseline_metric"
)

#_______________________________________________________________________________
#----                            Constructors                               ----
#_______________________________________________________________________________

#' 
#' Absolute Change from Baseline (CFB).
#' 
#' @inheritParams metrics_params
#' @export
CFB <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="cfb_metric")
  return(setDefaultNameIfNA(metric))
}

#' 
#' Percent Change from Baseline (PCFB).
#' 
#' @inheritParams metrics_params
#' @export
PCFB <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="pcfb_metric")
  return(setDefaultNameIfNA(metric))
}

#' 
#' Ratio to Baseline.
#' 
#' @inheritParams metrics_params
#' @export
RatioBaseline <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="ratio_baseline_metric")
  return(setDefaultNameIfNA(metric))
}

#' 
#' Log-transformed Change from Baseline.
#' 
#' @inheritParams metrics_params
#' @export
LogCFB <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="log_cfb_metric")
  return(setDefaultNameIfNA(metric))
}

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature=c("cfb_metric"), definition=function(object, ...) {
  return("CFB")
})

#' @rdname get_default_name
setMethod("get_default_name", signature=c("pcfb_metric"), definition=function(object, ...) {
  return("PCFB")
})

#' @rdname get_default_name
setMethod("get_default_name", signature=c("ratio_baseline_metric"), definition=function(object, ...) {
  return("Ratio_Base")
})

#' @rdname get_default_name
setMethod("get_default_name", signature=c("log_cfb_metric"), definition=function(object, ...) {
  return("Log_CFB")
})

#_______________________________________________________________________________
#----                               i_value                                 ----
#_______________________________________________________________________________

# Helper function to extract the first value chronologically (assuming time is sorted)
# If time is not guaranteed to be sorted in your data passing mechanism, use value[which.min(time)]
get_baseline_value <- function(time, value) {
  if (length(value) == 0) return(NA_real_)
  return(value[1]) 
}

#' @rdname i_value
setMethod("i_value", signature=c("cfb_metric", "numeric", "numeric"), definition=function(object, time, value) {
  y0 <- get_baseline_value(time, value)
  return(value - y0)
})

#' @rdname i_value
setMethod("i_value", signature=c("pcfb_metric", "numeric", "numeric"), definition=function(object, time, value) {
  y0 <- get_baseline_value(time, value)
  if (is.na(y0) || y0 == 0) return(rep(NA_real_, length(value)))
  return(((value - y0) / y0) * 100)
})

#' @rdname i_value
setMethod("i_value", signature=c("ratio_baseline_metric", "numeric", "numeric"), definition=function(object, time, value) {
  y0 <- get_baseline_value(time, value)
  if (is.na(y0) || y0 == 0) return(rep(NA_real_, length(value)))
  return(value / y0)
})

#' @rdname i_value
setMethod("i_value", signature=c("log_cfb_metric", "numeric", "numeric"), definition=function(object, time, value) {
  y0 <- get_baseline_value(time, value)
  if (is.na(y0) || y0 <= 0 || any(value <= 0, na.rm = TRUE)) return(rep(NA_real_, length(value)))
  return(log(value) - log(y0))
})

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature=c("abstract_baseline_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "base"))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("abstract_baseline_metric", "json_element"), definition=function(object, json) {
  return(loadMetricFromJSON(object=object, json=json))
})
