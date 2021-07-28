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
    x = "data.frame",                  # default dataframe
    variable = "character",            # default variable
    individual_results = "data.frame", # individual results
    summary_results = "data.frame"     # summary results
  ),
  contains="pmx_element",
  validity=validateMetric
)

#_______________________________________________________________________________
#----                              compute                                  ----
#_______________________________________________________________________________

#' Compute.
#' 
#' @param object metric to be computed
#' @param ... extra arguments
#' @return updated object
#' @export
#' @rdname compute
compute <- function(object, ...) {
  stop("No default function is provided")
}

setGeneric("compute", function(object, ...) {
  standardGeneric("compute")
})
