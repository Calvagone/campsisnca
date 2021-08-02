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
#' @param name custom metric name
#' @export
Cmax <- function(x=NULL, variable=NULL, name=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "Cmax" else name
  return(new("cmax_metric", x=x, variable=variable, name=name))
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("cmax_metric", "numeric"), definition=function(object, level, ...) {
  object@individual <- cmax_delegate(x=object@x, variable=object@variable)
  return(object %>% summariseIndividualData(level=level))    
})

#_______________________________________________________________________________
#----                           implementation                              ----
#_______________________________________________________________________________

#' 
#' Compute Cmax.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
cmax_delegate <- function(x, variable) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.max(dv_variable)) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, value=dv_variable))
}
