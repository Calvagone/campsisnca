#_______________________________________________________________________________
#----                           nca_analysis class                          ----
#_______________________________________________________________________________

#' 
#' NCA analysis class.
#' 
#' @export
setClass(
  "nca_analysis",
  representation(
    name = "character",              # analysis name
    window = "nca_time_window",   # default time range
    variable = "character",          # default variable name
    metrics = "nca_metrics"          # metrics contained in this analysis
  ),
  contains="pmx_element",
  prototype=prototype(name="Default", window=TimeWindow(), variable=character(0), metrics=NCAMetrics())
)

#' 
#' Create an NCA analysis.
#' 
#' @param name name of this analysis, e.g. 'Day 1'
#' @param time_range default time range for this analysis
#' @param variable default variable which is analysed
#' @param metrics list of metrics
#' @export
NCAAnalysis <- function(name="Default", window=TimeWindow(), variable=NULL, metrics=NCAMetrics()) {
  if (is.null(variable)) {
    variable = character(0)
  }
  return(new("nca_analysis", name=name, window=window, variable=variable, metrics=metrics))
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("nca_analysis", "nca_metric"), definition = function(object, x) {
  object@metrics <- object@metrics %>% add(x)
  return(object)
})

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature=c("nca_analysis"), definition = function(x) {
  return(x@name)
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("nca_analysis", "json_element"), definition=function(object, json) {
  # Assign type to type range
  json@data$window$type <- "nca_time_window"
  
  # Retrieve metrics
  jsonMetrics <- json@data$metrics
  json@data$metrics <- NULL
  object@metrics@list <- jsonMetrics %>%
    purrr::map(~loadFromJSON(new(.x$type), JSONElement(.x)))
  
  # Auto mapping
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(object)
})
