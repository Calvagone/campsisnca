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
    custom_function="character"
  ),
  contains="nca_metric",
  prototype=prototype(ivalue_tibble=TRUE),
  validity=validateCustomMetric
)

#' 
#' Custom metric (input data as time and value vectors).
#' 
#' @inheritParams metricsParams
#' @param fun any custom function with exactly 2 arguments: time and value
#' @export
CustomMetric <- function(x=NULL, variable=NULL, fun, name=NULL, unit=NULL,
                         categorical=FALSE, stat_display=getStatDisplayDefault(categorical), digits=NULL) {
  metric <- CustomMetricTbl(x=x, fun=fun, name=name, unit=unit,
                            categorical=categorical, stat_display=stat_display, digits=digits)
  metric@variable <- processVariable(variable)
  metric@ivalue_tibble <- FALSE
  return(metric)
}

#' 
#' Custom metric (input data as tibble).
#' 
#' @inheritParams metricsParams
#' @param fun any custom function with exactly 1 argument: data
#' @export
CustomMetricTbl <- function(x=NULL, fun, name=NULL, unit=NULL,
                         categorical=FALSE, stat_display=getStatDisplayDefault(categorical), digits=NULL) {
  x <- processDataframe(x)
  name <- if (is.null(name)) "Custom" else name
  unit <- processUnit(unit)
  digits <- deparseDigits(digits)
  fun <- deparseCustomFun(fun)
  return(new("custom_metric", x=x, variable=as.character(NA), name=name, unit=unit, custom_function=fun,
             categorical=categorical, stat_display=stat_display, digits=digits, ivalue_tibble=TRUE))
}

#_______________________________________________________________________________
#----                            iValueTbl                                  ----
#_______________________________________________________________________________

#' @rdname iValueTbl
setMethod("iValueTbl", signature=c("custom_metric", "tbl_df"), definition=function(object, data) {
  fun <- eval(expr=parse(text=object@custom_function))
  return(fun(data))    
})

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("custom_metric", "numeric", "numeric"), definition=function(object, time, value) {
  fun <- eval(expr=parse(text=object@custom_function))
  return(fun(time, value))    
})


#' @importFrom rlang is_function is_lambda
deparseCustomFun <- function(fun) {
  if (rlang::is_function(fun)) {
    retValue <- deparse1Line(fun)
    
  } else if (rlang::is_formula(fun)) {
    retValue <- paste0("rlang::as_function(", deparse1Line(fun), ")")
    
  } else {
    stop("Custom function must be a function or a purrr-style lambda function")
  }
  return(retValue)
}
