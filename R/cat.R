#_______________________________________________________________________________
#----                       cat_metric class                            ----
#_______________________________________________________________________________

validateCAtMetric <- function(object) {
  return(expectOne(object, "observed_time"))
}

#' 
#' Ctrough metric class.
#' 
#' @export
setClass(
  "cat_metric",
  representation(
    observed_time = "numeric"
  ),
  contains="nca_metric",
  prototype=prototype(observed_time=as.numeric(NA)),
  validity=validateCAtMetric
)

#' 
#' CAt (concentration at specific time).
#' 
#' @inheritParams metricsParams
#' @param time what time to read the concentrations. If not provided, last concentrations from x will be returned.
#' @export
CAt <- function(x=NULL, variable=NULL, time=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cat_metric", def_name="Conc")
  metric@observed_time <- ifelse(is.null(time), as.numeric(NA), time)
  metric@concentration <- TRUE
  return(metric)
}

#' 
#' Clast. Last time in x shall be considered as the last concentration time.
#' 
#' @inheritParams metricsParams
#' @export
Clast <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cat_metric", def_name="Clast")
  metric@concentration <- TRUE
  return(metric)
}

#' 
#' Ctrough. Last time in x shall be considered as the trough time. Similar to Clast, only name is different.
#' 
#' @inheritParams metricsParams
#' @export
Ctrough <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cat_metric", def_name="Ctrough")
  metric@concentration <- TRUE
  return(metric)
}

#' 
#' Value at (value at specific time).
#' 
#' @inheritParams metricsParams
#' @param time what time to read the values. If not provided, last values from x will be returned.
#' @export
ValueAt <- function(x=NULL, variable=NULL, time=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cat_metric", def_name="Value")
  metric@observed_time <- ifelse(is.null(time), as.numeric(NA), time)
  metric@concentration <- FALSE
  return(metric)
}

#' 
#' Last value.
#' 
#' @inheritParams metricsParams
#' @export
Last <- function(x=NULL, variable=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="cat_metric", def_name="Last value")
  metric@concentration <- FALSE
  return(metric)
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("cat_metric", "numeric", "numeric"), definition=function(object, time, value) {
  observed_time <- object@observed_time
  if (is.na(observed_time)) {
    return(value[length(value)])
  } else {
    index <- which(time==observed_time)
    if (length(index)==0) {
      stop(paste0("Could not find any sample at t=", observed_time))
    } else {
      return(value[index[1]])
    }
  }
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("cat_metric"), definition = function(x) {
  concentration <- x@concentration
  if (concentration) {
    retValue <- subscriptOccurrence(x %>% getName(), "trough")
    retValue <- subscriptOccurrence(retValue, "last")
  }
  return(retValue)
})

