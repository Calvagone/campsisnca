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
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature=c("nca_metrics"), definition=function(x) {
  return(paste0("NCA metrics: ", paste0(x@list %>% purrr::map(~getName(.x)), collapse=" / ")))
})

#_______________________________________________________________________________
#----                              getUnit                                  ----
#_______________________________________________________________________________

#' @rdname getUnit
setMethod("getUnit", signature=c("nca_metrics", "character"), definition=function(object, metric, ...) {
  metrics <- object@list %>% purrr::keep(.p=~.x@name==metric)
  if (metrics %>% length() == 0) {
    stop(paste0("Metric ", metric, " not found"))
  }
  metric <- metrics[[1]]
  return(metric@unit)
})
