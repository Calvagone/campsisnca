#_______________________________________________________________________________
#----                          nca_metrics class                            ----
#_______________________________________________________________________________

validateNCAMetrics <- function(object) {
  return(c(expectOne(object, "variable")))
}

#' 
#' NCA metric class. See this class as an interface.
#' 
#' @export
setClass(
  "nca_metrics",
  representation(
    x = "data.frame",         # default dataframe
    variable = "character",   # default variable
    scenario = "character"    # named character vector, e.g. c(day='Day 1', fasted='Fasted')
  ),
  contains=c("pmx_element", "pmx_list"), # PMX element in nca_metrics_table
  prototype = prototype(type="nca_metric"),
  validity=validateNCAMetrics
)

#' 
#' NCA metrics
#' 
#' @inheritParams metricsParams
#' @param scenario character vector used to describe the current scenario.
#' E.g. c(day="Day 1", condition="Fasted)
#' @export
NCAMetrics <- function(x=NULL, variable=NULL, scenario=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  if (is.null(scenario)) {
    scenario <- character()
  }
  return(new("nca_metrics", x=x, variable=variable, scenario=scenario))
}

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature=c("nca_metrics"), definition=function(x) {
  return(paste0("NCA metrics: ", paste0(x@scenario, collapse=" / ")))
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

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("nca_metrics", "numeric"), definition=function(object, quantile_type, ...) {
  object@list <- object@list %>% purrr::map(.f=function(.x) {
    # Use default dataframe if specific dataframe is empty
    if (nrow(.x@x) == 0) {
      .x@x <- object@x
    }
    # Use default dependent variable if specific variable is empty
    if (is.na(.x@variable)) {
      .x@variable <- object@variable
    }
    return(.x %>% calculate(quantile_type=quantile_type, ...))
  })
  return(object)    
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

setMethod("export", signature=c("nca_metrics", "character"), definition=function(object, dest, ...) {
  if (dest=="dataframe") {
    return(object %>% export(new("dataframe_type"), ...))
  } else {
    stop("Only dataframe is supported for now")
  }
})

setMethod("export", signature=c("nca_metrics", "dataframe_type"), definition=function(object, dest, type="summary", ...) {
  retValue <- object@list %>% purrr::map_df(~.x %>% export(dest=dest, type=type, ...))
  names <- names(object@scenario)
  values <- as.character(object@scenario)
  for (name in names) {
    retValue <- retValue %>% dplyr::mutate(!!name := values[[which(name==names)]])
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                       reduceTo2Dimensions                             ----
#_______________________________________________________________________________

#' @rdname reduceTo2Dimensions
setMethod("reduceTo2Dimensions", signature=c("nca_metrics"), definition=function(object, ...) {
  scenario <- object@scenario
  size <- length(scenario)
  if (size > 2) {
    names <- names(scenario)
    values <- as.character(scenario)
    hNames <- names[1:(size-1)]
    hValues <- values[1:(size-1)]
    hVec <- paste0(hValues, collapse=" / ")
    names(hVec) <- paste0(hNames, collapse="_") # Variables concatenated with _
    object@scenario <- c(hVec, scenario[size])
  }
  return(object)
})
