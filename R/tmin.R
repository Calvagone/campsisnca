#_______________________________________________________________________________
#----                          tmin_metric class                            ----
#_______________________________________________________________________________

validateTminMetric <- function(object) {
  return(TRUE)
}

#' 
#' Tmin metric class.
#' 
#' @export
setClass(
  "tmin_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateTminMetric
)

#' 
#' Tmin.
#' 
#' @inheritParams metricsParams
#' @export
Tmin <- function(x=NULL, variable=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("tmin_metric", x=x, variable=variable))
}

#' @rdname compute
setMethod("compute", signature=c("tmin_metric"), definition=function(object) {
  return(tmin_delegate(x=object@x, variable=object@variable))    
})

#_______________________________________________________________________________
#----                           implementation                              ----
#_______________________________________________________________________________

#' 
#' Compute tmin.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
tmin_delegate <- function(x, variable) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.min(dv_variable)) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, tmin=time))
}