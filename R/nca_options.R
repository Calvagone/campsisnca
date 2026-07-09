#' 
#' NCA options class.
#' 
#' @export
setClass(
  "nca_options",
  representation(
    quantile_type = "integer",
    data_time_unit = "character",
    table_time_unit = "character",
    rep_stat_display = "character", # Vector
    rep_stat_digits = "integer"
  ),
  prototype=prototype(quantile_type=2L, data_time_unit="hour", table_time_unit="hour",
   rep_stat_display=getStatDisplayDefault(), rep_stat_digits=3L),
)

#' 
#' Undefined NCA options class.
#' 
#' @export
setClass(
  "undefined_nca_options",
  representation(
  ),
  contains="nca_options"
)

#' 
#' NCA options used for calculation of metrics.
#' 
#' @param quantile_type type of quantile to use (see ?quantile), default value in campsisnca is 2 (aligned with gtsummary)
#' @param data_time_unit time unit of the data given to 'calculate'
#' @param table_time_unit time unit in table (for time-dependent metrics like AUC, Time above and below, etc.)
#' @param rep_stat_display display format for replicate statistics, character vector. Default is '{median} ({p5}–{p95})'.
#' @param rep_stat_digits number of significant digits to display for replicate statistics, default is 3.
#' @export
NCAOptions <- function(quantile_type=2L, data_time_unit="hour", table_time_unit="hour",
 rep_stat_display=getStatDisplayDefault(), rep_stat_digits=3L) {
  return(new("nca_options", quantile_type=as.integer(quantile_type),
             data_time_unit=data_time_unit, table_time_unit=table_time_unit, rep_stat_display=rep_stat_display,
              rep_stat_digits=rep_stat_digits))
}

#' 
#' Undefined NCA options.
#' 
#' @export
UndefinedNCAOptions <- function() {
  return(new("undefined_nca_options"))
}

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("nca_options", "json_element"), definition=function(object, json) {
  return(mapJSONPropertiesToS4Slots(object=object, json=json))
})
