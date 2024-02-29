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
Tmin <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=getStatDisplayDefault()) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "tmin" else name
  unit <- processUnit(unit)
  return(new("tmin_metric", x=x, variable=variable, name=name, unit=unit,
             stat_display=stat_display))
}

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature=c("tmin_metric"), definition = function(x) {
  return("tmin")
})

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("tmin_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(time[which.min(value)])    
})
