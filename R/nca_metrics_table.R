#_______________________________________________________________________________
#----                       nca_metrics_table class                         ----
#_______________________________________________________________________________

validateNCAMetricsTable <- function(object) {
  return(TRUE)
}

#' 
#' NCA metrics table class.
#' 
#' @export
setClass(
  "nca_metrics_table",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="nca_metrics"),
  validity=validateNCAMetricsTable
)

#' 
#' NCA metrics table.
#' 
#' @export
NCAMetricsTable <- function() {
  return(new("nca_metrics_table"))
}
