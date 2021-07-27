
processDataframe <- function(x) {
  if (is.null(x)) {
    return(data.frame())
  } else {
    assertthat::assert_that(is.data.frame(x), msg="x not a dataframe")
  }
}

processVariable <- function(variable) {
  if (is.null(x)) {
    return(as.character(NA))
  } else {
    assertthat::assert_that(is.character(variable) && length(variable), msg="variable not a single character value")
  }
}

#' Main metrics parameters.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
metricsParams <- function(x=NULL, variable=NULL) {
  # Do nothing
}

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
#----                          auc_metric class                             ----
#_______________________________________________________________________________

validateAUCMetric <- function(object) {
  return(expectOne(object, "method"))
}

#' 
#' AUC metric class.
#' 
#' @export
setClass(
  "auc_metric",
  representation(
    method = "integer"
  ),
  contains="nca_metric",
  validity=validateAUCMetric
)

#' 
#' Auc.
#' 
#' @inheritParams metricsParams
#' @param method method:
#' * 1: linear up - linear down
#' * 2: linear up - logarithmic down
#' * 3: linear before Tmax, logarithmic after Tmax
#' @export
AUC <- function(x=NULL, variable=NULL, method=1) {
  x = processDataframe(x)
  variable = processVariable(variable)
  assertthat::assert_that(method %in% c(1,2,3), msg="method must be 1, 2 or 3")
  return(new("auc_metric", x=x, variable=variable, method=method))
}

#_______________________________________________________________________________
#----                          cmax_metric class                             ----
#_______________________________________________________________________________

validateCmaxMetric <- function(object) {
  return(TRUE)
}

#' 
#' Cmax metric class.
#' 
#' @export
setClass(
  "cmax_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateCmaxMetric
)

#' 
#' Cmax.
#' 
#' @inheritParams metricsParams
#' @export
Cmax <- function(x=NULL, variable=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("cmax_metric", x=x, variable=variable))
}

#_______________________________________________________________________________
#----                          tmax_metric class                            ----
#_______________________________________________________________________________

validateTmaxMetric <- function(object) {
  return(TRUE)
}

#' 
#' Tmax metric class.
#' 
#' @export
setClass(
  "tmax_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateTmaxMetric
)

#' 
#' Tmax.
#' 
#' @inheritParams metricsParams
#' @export
Tmax <- function(x=NULL, variable=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("tmax_metric", x=x, variable=variable))
}

#_______________________________________________________________________________
#----                          cmin_metric class                            ----
#_______________________________________________________________________________

validateCminMetric <- function(object) {
  return(TRUE)
}

#' 
#' Cmin metric class.
#' 
#' @export
setClass(
  "cmin_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateCminMetric
)

#' 
#' Cmin.
#' 
#' @inheritParams metricsParams
#' @export
Cmin <- function(x=NULL, variable=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("cmin_metric", x=x, variable=variable))
}

#_______________________________________________________________________________
#----                          tmin_metric class                            ----
#_______________________________________________________________________________

validateTminMetric <- function(object) {
  return(TRUE)
}

#' 
#' Tmin metric class.
#' 
#' @export
setClass(
  "tmin_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateTminMetric
)

#' 
#' Tmin.
#' 
#' @inheritParams metricsParams
#' @export
Tmin <- function(x=NULL, variable=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("tmin_metric", x=x, variable=variable))
}

#_______________________________________________________________________________
#----                       ctrough_metric class                            ----
#_______________________________________________________________________________

validateCtroughMetric <- function(object) {
  return(expectOne(object, "time"))
}

#' 
#' Ctrough metric class.
#' 
#' @export
setClass(
  "ctrough_metric",
  representation(
    time = "numeric"
  ),
  contains="nca_metric",
  validity=validateCtroughMetric
)

#' 
#' Ctrough.
#' 
#' @inheritParams metricsParams
#' @param time time value to read Ctrough
#' @export
Ctrough <- function(x=NULL, variable=NULL, time) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("ctrough_metric", x=x, variable=variable, time=time))
}

#_______________________________________________________________________________
#----                         cavg_metric class                             ----
#_______________________________________________________________________________

validateCavgMetric <- function(object) {
  return(TRUE)
}

#' 
#' Cavg metric class.
#' 
#' @export
setClass(
  "cavg_metric",
  representation(
    time = "numeric"
  ),
  contains="nca_metric",
  validity=validateCavgMetric
)

#' 
#' Cavg.
#' 
#' @inheritParams metricsParams
#' @export
Cavg <- function(x=NULL, variable=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("ctrough_metric", x=x, variable=variable))
}
