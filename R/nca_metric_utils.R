
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

processUnit <- function(unit) {
  return(if(is.null(unit)) as.character(NA) else unit)
}

#' Main metrics parameters.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @param name custom metric name (will be exported into table headers)
#' @param unit metric unit (will be exported into table headers if provided)
#' @param stat_display statistics display (see package gtsummary)
metricsParams <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=NULL) {
  # Do nothing
}

#' 
#' Standardise CAMPSIS/NONMEM dataframe for NCA analysis.
#' Additional checks will also be performed.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @importFrom assertthat assert_that
#' @importFrom dplyr rename_at
#' @importFrom campsis obsOnly
#' 
standardise <- function(x, variable) {
  assertthat::assert_that(is.character(variable) && length(variable)==1, msg="variable must be a single character value")
  assertthat::assert_that(is.data.frame(x), msg="x is not a data frame")
  assertthat::assert_that(variable %in% colnames(x), msg=paste0("Variable '", variable, "' not found in data frame"))

  # Use only observations
  x <- x %>% campsis::obsOnly()
  
  # Check no time is NA
  checkNATimes(x, time_var="TIME") 
  
  # Check no observation is NA
  checkNAObservations(x, variable=variable)
  
  # Check time is monotonically increasing
  checkTimesAreIncreasing(x)
  
  return(x)
}