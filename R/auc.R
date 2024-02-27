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
  prototype=prototype(method=1L, extra_args="method"),
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
Auc <- function(x=NULL, variable=NULL, method=1, name=NULL, unit=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "AUC" else name
  unit <- processUnit(unit)
  assertthat::assert_that(method %in% c(1,2,3), msg="method must be 1, 2 or 3")
  return(new("auc_metric", x=x, variable=variable, method=as.integer(method), name=name, unit=unit))
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("auc_metric", "numeric", "numeric"), definition=function(object, time, value, method) {
  return(trap(x=time, y=value, method=method))    
})
