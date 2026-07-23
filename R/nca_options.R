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
    replicated_nca_options = "replicated_nca_options"
  ),
  prototype = prototype(
    quantile_type = 2L,
    data_time_unit = "hour",
    table_time_unit = "hour",
    replicated_nca_options = ReplicatedNCAOptions()
  )
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
#' @export
NCAOptions <- function(
  quantile_type = 2L,
  data_time_unit = "hour",
  table_time_unit = "hour"
) {
  return(new(
    "nca_options",
    quantile_type = as.integer(quantile_type),
    data_time_unit = data_time_unit,
    table_time_unit = table_time_unit
  ))
}

#' 
#' Undefined NCA options.
#' 
#' @export
UndefinedNCAOptions <- function() {
  return(new("undefined_nca_options"))
}

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature=c("nca_options", "json_element"), definition=function(object, json) {
  rep_options_json <- json@data$replicated_nca_options
  rep_options <- ReplicatedNCAOptions()
  if (!is.null(rep_options_json)) {
    rep_options <- load_from_json(rep_options, JSONElement(rep_options_json))
    json@data$replicated_nca_options <- NULL
  }
  print(json)
  nca_options <- map_json_properties_to_s4_slots(object=object, json=json)
  nca_options@replicated_nca_options <- rep_options
  return(nca_options)
})
