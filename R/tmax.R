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
#' @param name custom metric name
#' @export
Tmax <- function(x=NULL, variable=NULL, name=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "tmax" else name
  return(new("tmax_metric", x=x, variable=variable, name=name))
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("tmax_metric", "numeric"), definition=function(object, level, ...) {
  object@individual <- tmax_delegate(x=object@x, variable=object@variable)
  return(object %>% summariseIndividualData(level=level))    
})

#_______________________________________________________________________________
#----                           implementation                              ----
#_______________________________________________________________________________

#' 
#' Compute tmax.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
tmax_delegate <- function(x, variable) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.max(dv_variable)) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, value=time))
}
