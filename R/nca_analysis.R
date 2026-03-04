#_______________________________________________________________________________
#----                           nca_analysis class                          ----
#_______________________________________________________________________________

allStrataLevels <- function() {
  return("all")
}

getDefaultStrata <- function() {
  return(c(SCENARIO=allStrataLevels(), ARM=allStrataLevels()))
}

getEffectiveStratVars <- function(strata, x) {
  retValue <- NULL
  colnames_ <- colnames(x)
  for (index in seq_along(strata)) {
    strat_var <- names(strata)[index]
    strat_vars_level <- as.character(strata)[index]
    if (strat_var %in% colnames_) {
      if (strat_vars_level == allStrataLevels() && length(unique(x %>% dplyr::pull(strat_var))) > 1) {
        retValue <- c(retValue, strat_var)
      }
    } else {
      message(sprintf("Stratification variable '%s' not in dataframe", strat_var))
    }
  }
  if (is.null(retValue)) {
    retValue <- character(0)
  }
  return(retValue)
}

#' 
#' NCA analysis class.
#' 
#' @export
setClass(
  "nca_analysis",
  representation(
    name = "character",                # analysis name
    window = "nca_time_window",        # default time range
    variable = "character",            # default variable name
    metrics = "nca_metrics",           # metrics contained in this analysis
    strata = "character",              # named vector
    effective_strat_vars = "character" # effective stratification variables in data, transient field: updated when calculate is called
  ),
  contains="pmx_element",
  prototype=prototype(name="Default", window=TimeWindow(), variable=as.character(NA),
                      metrics=NCAMetrics(), strata=getDefaultStrata(), strat_vars=character(0))
)

#' 
#' Create an NCA analysis.
#' 
#' @param name name of this analysis, e.g. 'Day 1'
#' @param window time window, see \link{TimeWindow}
#' @param variable default variable which is analysed
#' @param strata strata levels this analysis refers to, named vector, e.g. c(ARM='1g QD').
#'  Note, the default strata are c(SCENARIO='all', ARM='all').
#'  Use 'all' if this analysis refers to all levels for the specified stratification variable.
#'  By default, a stratification variable that has only 1 level is ignored.
#' @export
NCAAnalysis <- function(name="Default", window=TimeWindow(), variable=NULL, strata=getDefaultStrata()) {
  if (is.null(variable)) {
    variable = as.character(NA)
  }
  if (is.null(strata)) {
    strata = getDefaultStrata()
  }
  return(new("nca_analysis", name=name, window=window, variable=variable, strata=strata))
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
setMethod("calculate", signature=c("nca_analysis", "data.frame", "numeric"), definition=function(object, x, quantile_type, ...) {
  # Effective stratification variables based on strata and x
  object@effective_strat_vars <- getEffectiveStratVars(strata=object@strata, x=x)
  
  # Detect the specific strata
  specific_strata <- object@strata[object@strata != allStrataLevels()]
  
  # Filter input data frame to specific strata
  x_reduced <- purrr::reduce(
    names(specific_strata),
    ~ dplyr::filter(.x, .data[[.y]] == specific_strata[[.y]]),
    .init = x
  )
  
  object@metrics@list <- object@metrics@list %>% purrr::map(.f=function(.x) {
    
    # Use default analysis variable
    if (is.na(.x@variable) && !is.na(object@variable)) {
      .x@variable <- object@variable
    }
    
    # Use default analysis window
    if (is(.x@window, "undefined_nca_time_window")) {
      .x@window <- object@window
    }
    
    return(.x %>% calculate(x=x_reduced, quantile_type=quantile_type, strat_vars=object@effective_strat_vars, ...))
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
  strat_vars <- names(object@strata)
  retValue <- dplyr::relocate(retValue, dplyr::any_of(c("analysis", strat_vars)), .after=dplyr::last_col())
  
  return(retValue)
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
