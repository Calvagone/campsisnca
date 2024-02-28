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
  prototype=prototype(ivalue_tibble=TRUE),
  validity=validateCustomMetric
)

#' 
#' Custom metric.
#' 
#' @inheritParams metricsParams
#' @param fun any custom function with exactly 1 argument: data
#' @export
CustomMetric <- function(x=NULL, variable=NULL, fun=function(data){0}, name=NULL, unit=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "Custom" else name
  unit <- processUnit(unit)
  return(new("custom_metric", x=x, variable=variable, name=name, unit=unit, custom_function=fun))
}

#_______________________________________________________________________________
#----                            iValueTbl                                  ----
#_______________________________________________________________________________

#' @rdname iValueTbl
setMethod("iValueTbl", signature=c("custom_metric", "tbl_df"), definition=function(object, data) {
  return(object@custom_function(data=data))    
})
