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
  prototype=prototype(method=1L),
  validity=validateAUCMetric
)

#' 
#' Auc. Will be deprecated soon.
#' 
#' @inheritParams metricsParams
#' @param method method:
#' * 1: linear up - linear down
#' * 2: linear up - logarithmic down
#' * 3: linear before Tmax, logarithmic after Tmax
#' @export
Auc <- function(x=NULL, variable=NULL, method=1, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="auc_metric", def_name="AUC")
  assertthat::assert_that(method %in% c(1,2,3), msg="method must be 1, 2 or 3")
  metric@method <- as.integer(method)
  return(metric)
}

#' 
#' AUC.
#' 
#' @inheritParams metricsParams
#' @param method method:
#' * 1: linear up - linear down
#' * 2: linear up - logarithmic down
#' * 3: linear before Tmax, logarithmic after Tmax
#' @export
AUC <- function(x=NULL, variable=NULL, method=1, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="auc_metric", def_name="AUC")
  assertthat::assert_that(method %in% c(1,2,3), msg="method must be 1, 2 or 3")
  metric@method <- as.integer(method)
  return(metric)
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("auc_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(trap(x=time, y=value, method=object@method))    
})
