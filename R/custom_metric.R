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
  
  # Auto-replace known NCA metrics
  metric <- metric %>%
    replaceAll(pattern=NCAMetrics(), replacement="auto")
  
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

#_______________________________________________________________________________
#----                             replaceAll                                ----
#_______________________________________________________________________________

setMethod("replaceAll", signature=c("custom_metric", "character", "character"), definition=function(object, pattern, replacement, ...) {
  object@custom_function <- gsub(paste0("([^a-zA-Z0-9_]|^)(", pattern, ")([^a-zA-Z0-9_\\(]|$)"), replacement=paste0("\\1", replacement, "\\3"), x=object@custom_function)
  return(object)
})

setMethod("replaceAll", signature=c("custom_metric", "nca_metrics", "character"), definition=function(object, pattern, replacement, ...) {
  replacement <- "auto"
  object <- object %>%
    replaceAll(pattern=AUC(), replacement=replacement, fun_name="AUC") %>%
    replaceAll(pattern=CAt(), replacement=replacement, fun_name="CAt") %>%
    replaceAll(pattern=Clast(), replacement=replacement, fun_name="Clast") %>%
    replaceAll(pattern=Ctrough(), replacement=replacement, fun_name="Ctrough") %>%
    replaceAll(pattern=ValueAt(), replacement=replacement, fun_name="ValueAt") %>%
    replaceAll(pattern=Last(), replacement=replacement, fun_name="Last") %>%
    replaceAll(pattern=Cavg(), replacement=replacement, fun_name="Cavg") %>%
    replaceAll(pattern=Avg(), replacement=replacement, fun_name="Avg") %>%
    replaceAll(pattern=Cmax(), replacement=replacement, fun_name="Cmax") %>%
    replaceAll(pattern=Max(), replacement=replacement, fun_name="Max") %>%
    replaceAll(pattern=Cmin(), replacement=replacement, fun_name="Cmin") %>%
    replaceAll(pattern=Min(), replacement=replacement, fun_name="Min") %>%
    replaceAll(pattern=Thalf(), replacement=replacement, fun_name="Thalf") %>%
    replaceAll(pattern=Tmax(), replacement=replacement, fun_name="Tmax") %>%
    replaceAll(pattern=Tmin(), replacement=replacement, fun_name="Tmin")
  return(object)
})

setMethod("replaceAll", signature=c("custom_metric", "nca_metric", "character"), definition=function(object, pattern, replacement, fun_name=NULL, ...) {
  # Replace all only available for non-tibble custom function
  # In custom function tibble, variable of interest is unknown...
  if (object@ivalue_tibble) {
    return(object)
  }
  if (is.null(fun_name)) {
    name <- pattern %>% getName()
  } else {
    name <- fun_name
  }
  object <- object %>%
    replaceAll(pattern=name, replacement=sprintf("iValue(%s(),.x,.y)", name))
  return(object)
})
