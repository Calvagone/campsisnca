#_______________________________________________________________________________
#----                          nca_metrics class                            ----
#_______________________________________________________________________________

#' 
#' NCA metrics class. See this class as a list of NCA metrics.
#' 
#' @export
setClass(
  "nca_metrics",
  representation(
  ),
  contains=c("pmx_list"),
  prototype = prototype(type="nca_metric")
)

#' 
#' NCA metrics
#' 
#' @export
NCAMetrics <- function() {
  return(new("nca_metrics"))
}

#_______________________________________________________________________________
#----                             get_name                                   ----
#_______________________________________________________________________________

setMethod("get_name", signature=c("nca_metrics"), definition=function(x) {
  return(paste0("NCA metrics: ", paste0(x@list %>% purrr::map(~get_name(.x)), collapse=" / ")))
})

#_______________________________________________________________________________
#----                              get_unit                                 ----
#_______________________________________________________________________________

#' @rdname get_unit
setMethod("get_unit", signature=c("nca_metrics", "character"), definition=function(object, metric, ...) {
  metrics <- object@list %>% purrr::keep(.p=~.x@name==metric)
  if (metrics %>% length() == 0) {
    stop(paste0("Metric ", metric, " not found"))
  }
  metric <- metrics[[1]]
  return(metric@unit)
})
