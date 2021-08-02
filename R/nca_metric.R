#_______________________________________________________________________________
#----                          nca_metric class                             ----
#_______________________________________________________________________________

validateMetric <- function(object) {
  return(expectOneForAll(object, c("variable", "name")))
}

#' 
#' NCA metric class. See this class as an interface.
#' 
#' @export
setClass(
  "nca_metric",
  representation(
    x = "data.frame",               # specific dataframe
    variable = "character",         # specific variable
    individual = "data.frame",      # individual results
    summary = "data.frame",         # summary results
    name = "character"              # metric name (this name is exported)
  ),
  contains="pmx_element",
  validity=validateMetric
)

summariseIndividualData <- function(x, level) {
  assertthat::assert_that(all(colnames(x)==c("id", "value")))
  level.low <- (1 - level)/2
  level.up <- 1 - level.low
  x@summary <-
    x@individual %>% dplyr::summarise(
      low = quantile(value, level.low),
      med = median(value),
      up = quantile(value, level.up)
    )
  return(x)
}

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature=c("nca_metric"), definition = function(x) {
  return(x@name)
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

setMethod("export", signature=c("nca_metric", "character"), definition=function(object, dest, ...) {
  if (dest=="dataframe") {
    return(object %>% export(new("dataframe_type")))
  } else {
    stop("Only dataframe is supported for now")
  }
})

setMethod("export", signature=c("nca_metric", "dataframe_type"), definition=function(object, dest, ...) {
  if (nrow(object@summary) == 0) {
    stop(paste0("Metric ", object %>% getName(), " has no summary"))
  }
  return(dplyr::bind_cols(tibble::tibble(metric=object %>% getName(), object@summary)))
})