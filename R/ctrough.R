#_______________________________________________________________________________
#----                       ctrough_metric class                            ----
#_______________________________________________________________________________

validateCtroughMetric <- function(object) {
  return(expectOne(object, "trough_time"))
}

#' 
#' Ctrough metric class.
#' 
#' @export
setClass(
  "ctrough_metric",
  representation(
    trough_time = "numeric"
  ),
  contains="nca_metric",
  prototype=prototype(trough_time=as.numeric(NA)),
  validity=validateCtroughMetric
)

#' 
#' Ctrough.
#' 
#' @inheritParams metricsParams
#' @param time time value to read Ctrough. If not provided, last concentrations from x will be returned.
#' @export
Ctrough <- function(x=NULL, variable=NULL, time=NULL, name=NULL, unit=NULL, stat_display=getStatDisplayDefault(), digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="ctrough_metric", def_name="Ctrough")
  time <- if (is.null(time)) as.numeric(NA) else time
  metric@trough_time <- time
  return(metric)
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("ctrough_metric", "numeric", "numeric"), definition=function(object, time, value) {
  trough_time <- object@trough_time
  if (is.na(trough_time)) {
    return(value[length(value)])
  } else {
    index <- which(time==trough_time)
    if (length(index)==0) {
      stop(paste0("Could not find any sample at t=", trough_time))
    } else {
      return(value[index[1]])
    }
  }
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("ctrough_metric"), definition = function(x) {
  return(subscriptOccurrence(x %>% getName(), "trough"))
})

