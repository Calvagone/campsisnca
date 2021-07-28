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
Auc <- function(x=NULL, variable=NULL, method=1) {
  x = processDataframe(x)
  variable = processVariable(variable)
  assertthat::assert_that(method %in% c(1,2,3), msg="method must be 1, 2 or 3")
  return(new("auc_metric", x=x, variable=variable, method=as.integer(method)))
}

#' @rdname calculate
setMethod("calculate", signature=c("auc_metric"), definition=function(object, ...) {
  object@individual <- auc_delegate(x=object@x, variable=object@variable, method=object@method)
  return(object)    
})

#_______________________________________________________________________________
#----                           implementation                              ----
#_______________________________________________________________________________

#' 
#' Compute AUC.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @param method method:
#' * 1: linear up - linear down
#' * 2: linear up - logarithmic down
#' * 3: linear before Tmax, logarithmic after Tmax
auc_delegate <- function(x, variable, method=1) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::summarise(value=trap(x=time, y=dv_variable, method=method), .groups="drop")
  return(x)
}
