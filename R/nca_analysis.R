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
    window = "nca_time_window",      # default time range
    variable = "character",          # default variable name
    metrics = "nca_metrics",         # metrics contained in this analysis
    strat_vars = "character"         # expected stratification variables in data
  ),
  contains="pmx_element",
  prototype=prototype(name="Default", window=TimeWindow(), variable=as.character(NA),
                      metrics=NCAMetrics(), strat_vars=character(0))
)

#' 
#' Create an NCA analysis.
#' 
#' @param name name of this analysis, e.g. 'Day 1'
#' @param time_range default time range for this analysis
#' @param variable default variable which is analysed
#' @param metrics list of metrics
#' @export
NCAAnalysis <- function(name="Default", window=TimeWindow(), variable=NULL, metrics=NCAMetrics(), strat_vars=NULL) {
  if (is.null(variable)) {
    variable = as.character(NA)
  }
  if (is.null(strat_vars)) {
    strat_vars = character(0)
  }
  return(new("nca_analysis", name=name, window=window, variable=variable, metrics=metrics, strat_vars=strat_vars))
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("nca_analysis", "nca_metric"), definition = function(object, x) {
  object@metrics <- object@metrics %>% add(x)
  return(object)
})

setMethod("add", signature = c("nca_analysis", "list"), definition = function(object, x) {
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
#----                              getUnit                                  ----
#_______________________________________________________________________________

#' @rdname getUnit
setMethod("getUnit", signature=c("nca_analysis", "character"), definition=function(object, metric, ...) {
  return(object@metrics %>% getUnit(metric=metric, ...))
})

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("nca_analysis", "data.frame", "character", "numeric"), definition=function(object, x, strat_vars, quantile_type, ...) {
  object@metrics@list <- object@metrics@list %>% purrr::map(.f=function(.x) {
    
    # Use default analysis variable
    if (is.na(.x@variable) && !is.na(object@variable)) {
      .x@variable <- object@variable
    }
    
    # Use default analysis window
    if (is(.x@window, "undefined_nca_time_window")) {
      .x@window <- object@window
    }
    
    # Use default 'expected' stratification variables of this analysis
    if (length(strat_vars) == 0 && length(object@strat_vars) > 0) {
      strat_vars <- object@strat_vars
    }
    
    return(.x %>% calculate(x=x, strat_vars=strat_vars, quantile_type=quantile_type, ...))
  })
  return(object)    
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

setMethod("export", signature=c("nca_analysis", "character"), definition=function(object, dest, analysis_strat=TRUE, ...) {
  if (dest=="dataframe") {
    return(object %>% export(new("dataframe_type"), analysis_strat=analysis_strat, ...))
  } else {
    stop("Only dataframe is supported for now")
  }
})

setMethod("export", signature=c("nca_analysis", "dataframe_type"), definition=function(object, dest, type="summary", analysis_strat=TRUE, ...) {
  retValue <- object@metrics@list %>% purrr::map_df(~.x %>% export(dest=dest, type=type, ...))
  if (analysis_strat) {
    retValue <- retValue %>%
      dplyr::mutate(analysis=object@name)
  }
  
  # For backwards-compatibility in tests
  retValue <- dplyr::relocate(retValue, dplyr::any_of(c("analysis", object@strat_vars)), .after=dplyr::last_col())
  
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
