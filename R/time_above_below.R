#_______________________________________________________________________________
#----                    time_above_below_limit class                       ----
#_______________________________________________________________________________

validateTimeAboveBelowLimitMetric <- function(object) {
  return(TRUE)
}

#' 
#' Time above/below class.
#' 
#' @export
setClass(
  "time_above_below_limit",
  representation(
    limit="numeric",
    above="logical"
  ),
  contains="nca_metric",
  validity=validateTmaxMetric
)

#' 
#' Time above a certain limit.
#' 
#' @inheritParams metricsParams
#' @export
TimeAboveLimit <- function(x=NULL, variable=NULL, limit=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="time_above_below_limit", def_name="Timeabove")
  metric@limit <- limit
  metric@above <- TRUE
  return(metric)
}

#' 
#' Time below a certain limit.
#' 
#' @inheritParams metricsParams
#' @export
TimeBelowLimit <- function(x=NULL, variable=NULL, limit=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="time_above_below_limit", def_name="Timebelow")
  metric@limit <- limit
  metric@above <- FALSE
  return(metric)
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("time_above_below_limit", "numeric", "numeric"), definition=function(object, time, value) {
  limit <- object@limit
  above <- object@above
  
  
  return(retValue)    
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("time_above_below_limit"), definition=function(x) {
  if (x@above) {
    return(subscriptOccurrence(x %>% getName(), "above"))
  } else {
    return(subscriptOccurrence(x %>% getName(), "below"))
  }
})

