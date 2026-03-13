#' 
#' NCA options class.
#' 
#' @export
setClass(
  "nca_options",
  representation(
    quantile_type = "integer",
    data_time_unit = "character"
  ),
  prototype=prototype(quantile_type=2L, data_time_unit="hour"),
)

#' 
#' NCA options used for calculation of metrics.
#' 
#' @param quantile_type type of quantile to use (see ?quantile), default value in campsisnca is 2 (aligned with gtsummary)
#' @param data_time_unit time unit of the data given to 'calculate'
#' @export
NCAOptions <- function(quantile_type=2L, data_time_unit="hour") {
  return(new("nca_options", quantile_type=as.integer(quantile_type), data_time_unit=data_time_unit))
}
