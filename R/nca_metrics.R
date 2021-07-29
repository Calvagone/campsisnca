#_______________________________________________________________________________
#----                         common process                                ----
#_______________________________________________________________________________

processDataframe <- function(x) {
  if (is.null(x)) {
    return(data.frame())
  } else {
    assertthat::assert_that(is.data.frame(x), msg="x not a dataframe")
    return(x)
  }
}

processVariable <- function(variable) {
  if (is.null(variable)) {
    return(as.character(NA))
  } else {
    assertthat::assert_that(is.character(variable) && length(variable),
                            msg="variable not a single character value")
    return(variable)
  }
}

#' Main metrics parameters.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
metricsParams <- function(x=NULL, variable=NULL) {
  # Do nothing
}

#' 
#' Standardise input dataframe to CAMPSIS dataframe.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @return dataframe with id and time columns, filtered on the observations
toCAMPSISDataframe <- function(x) {
  assertthat::assert_that(is.data.frame(x), msg="x is not a data frame")
  campsis <- isCAMPSIS(x)
  if (!campsis) {
    x <- x %>% dplyr::rename(id=ID, time=TIME)
    x <- x %>% dplyr::filter(MDV==0) # Keep observations only
  }
  return(x)
}

#' 
#' Standardise CAMPSIS/NONMEM dataframe for NCA analysis.
#' Additional checks will also be performed.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
standardise <- function(x, variable) {
  assertthat::assert_that(is.character(variable) && length(variable)==1, msg="variable must be a single character value")
  assertthat::assert_that(is.data.frame(x), msg="x is not a data frame")
  assertthat::assert_that(variable %in% colnames(x), msg=paste0("Variable '", variable, "' not found in data frame"))
  x <- x %>% dplyr::rename_at(variable, ~"dv_variable")
  
  # NONMEM -> CAMPSIS adaptations
  x <- x %>% toCAMPSISDataframe()
  
  # Check no time is NA
  checkNATimes(x, time_var="time") 
  
  # Check no observation is NA
  checkNAObservations(x, variable="dv_variable")
  
  # Check time is monotonically increasing
  checkTimesAreIncreasing(x)
  
  return(x)
}

#_______________________________________________________________________________
#----                          nca_metric class                             ----
#_______________________________________________________________________________

validateMetric <- function(object) {
  return(expectOne(object, "variable"))
}

#' 
#' NCA metric class. See this class as an interface.
#' 
#' @export
setClass(
  "nca_metric",
  representation(
    x = "data.frame",          # specific dataframe
    variable = "character",    # specific variable
    individual = "data.frame", # individual results
    summary = "data.frame"     # summary results
  ),
  contains="pmx_element",
  validity=validateMetric
)

summariseIndividualData <- function(x, level) {
  assertthat::assert_that(all(colnames(x)==c("id", "value")))
  level.low <- (1 - level)/2
  level.up <- 1 - level.low
  x@summary <- x@individual %>% summarise(low=quantile(value, level.low),
                                          med=median(value),
                                          up=quantile(value, level.up))
  return(x)
}

#_______________________________________________________________________________
#----                          nca_metrics class                            ----
#_______________________________________________________________________________

validateNCAMetrics <- function(object) {
  return(c(expectOne(object, "variable"), expectOneOrMore(object, "scenario")))
}

#' 
#' NCA metric class. See this class as an interface.
#' 
#' @export
setClass(
  "nca_metrics",
  representation(
    x = "data.frame",       # default dataframe
    variable = "character", # default variable
    scenario = "character"  # named character vector, e.g. c(day='Day 1', fasted='Fasted')
  ),
  contains="pmx_list",
  prototype = prototype(type="nca_metric"),
  validity=validateNCAMetrics
)

#' 
#' NCA metrics
#' 
#' @inheritParams metricsParams
#' @param time time value to read Ctrough
#' @export
NCAMetrics <- function(x=NULL, variable=NULL, scenario) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("nca_metrics", x=x, variable=variable, scenario=scenario))
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("nca_metrics"), definition=function(object, ...) {
  object@list <- object@list %>% purrr::map(.f=function(.x) {
    # Use default dataframe if specific dataframe is empty
    if (nrow(.x@x) == 0) {
      .x@x <- object@x
    }
    # Use default dependent variable if specific variable is empty
    if (is.na(.x@variable)) {
      .x@variable <- object@variable
    }
    return(.x %>% calculate(...))
  })
  return(object)    
})