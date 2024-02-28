#_______________________________________________________________________________
#----                         cavg_metric class                             ----
#_______________________________________________________________________________

validateCavgMetric <- function(object) {
  return(TRUE)
}

#' 
#' Cavg metric class.
#' 
#' @export
setClass(
  "cavg_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateCavgMetric
)

#' 
#' Cavg.
#' 
#' @inheritParams metricsParams
#' @export
Cavg <- function(x=NULL, variable=NULL, name=NULL, unit=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "Cavg" else name
  unit <- processUnit(unit)
  return(new("cavg_metric", x=x, variable=variable, name=name, unit=unit))
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("cavg_metric", "numeric", "numeric"), definition=function(object, time, value) {
  start <- time[1]
  end <- time[length(time)]
  auc <- trap(x=time, y=value, method=1L)
  return(auc/(end - start))    
})
