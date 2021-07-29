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
#' @export
Cmax <- function(x=NULL, variable=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("cmax_metric", x=x, variable=variable))
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("cmax_metric"), definition=function(object, level=0.9, ...) {
  object@individual <- cmax_delegate(x=object@x, variable=object@variable)
  return(object %>% summariseIndividualData(level=level))    
})

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature=c("cmax_metric"), definition = function(x) {
  return("Cmax")
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
