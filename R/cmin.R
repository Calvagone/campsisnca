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

#' @rdname calculate
setMethod("calculate", signature=c("cmin_metric"), definition=function(object, ...) {
  object@individual <- cmin_delegate(x=object@x, variable=object@variable)
  return(object)    
})

#_______________________________________________________________________________
#----                           implementation                              ----
#_______________________________________________________________________________

#' 
#' Compute Cmin.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
cmin_delegate <- function(x, variable) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.min(dv_variable)) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, value=dv_variable))
}
