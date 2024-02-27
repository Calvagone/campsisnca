#_______________________________________________________________________________
#----                          custom_metric class                          ----
#_______________________________________________________________________________

validateCustomMetric <- function(object) {
  return(TRUE)
}

#' 
#' Custom metric class.
#' 
#' @export
setClass(
  "custom_metric",
  representation(
    custom_function = "function"
  ),
  contains="nca_metric",
  prototype=prototype(extra_args="custom_function"),
  validity=validateCustomMetric
)

#' 
#' Custom metric.
#' 
#' @inheritParams metricsParams
#' @param fun any custom function with exactly 2 arguments: time and value
#' @export
CustomMetric <- function(x=NULL, variable=NULL, fun=function(time, value){0}, name=NULL, unit=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "Custom" else name
  unit <- processUnit(unit)
  return(new("custom_metric", x=x, variable=variable, name=name, unit=unit, custom_function=fun))
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("custom_metric", "numeric", "numeric"), definition=function(object, time, value, custom_function) {
  return(custom_function(time=time, value=value))    
})
