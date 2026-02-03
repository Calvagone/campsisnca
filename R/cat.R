#_______________________________________________________________________________
#----                   conc at/value at metric classes                     ----
#_______________________________________________________________________________

validateValueAtMetric <- function(object) {
  return(expectOne(object, "observed_time"))
}

#' 
#' Abstract value at metric class.
#' 
#' @export
setClass(
  "abstract_value_at_metric",
  representation(
    observed_time = "numeric"
  ),
  contains="nca_metric",
  prototype=prototype(observed_time=as.numeric(NA)),
  validity=validateValueAtMetric
)

#' 
#' Value at metric class.
#' 
#' @export
setClass(
  "value_at_metric",
  representation(
  ),
  contains="abstract_value_at_metric"
)

#' 
#' Concentration at metric class.
#' 
#' @export
setClass(
  "conc_at_metric",
  representation(
  ),
  contains="abstract_value_at_metric"
)

#' 
#' Value at (value at specific time).
#' 
#' @inheritParams metricsParams
#' @param time what time to read the values. If not provided, last values from x will be returned.
#' @export
ValueAt <- function(variable=NULL, window=NULL, time=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="value_at_metric")
  metric@observed_time <- ifelse(is.null(time), as.numeric(NA), time)
  return(setDefaultNameIfNA(metric))
}


#' 
#' CAt (concentration at specific time).
#' 
#' @inheritParams metricsParams
#' @param time what time to read the concentrations. If not provided, last concentrations from x will be returned.
#' @export
CAt <- function(variable=NULL, window=NULL, time=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="conc_at_metric")
  metric@observed_time <- ifelse(is.null(time), as.numeric(NA), time)
  return(setDefaultNameIfNA(metric))
}

#_______________________________________________________________________________
#----                           getDefaultName                              ----
#_______________________________________________________________________________

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("value_at_metric"), definition=function(object, ...) {
  return("Value")
})

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("conc_at_metric"), definition=function(object, ...) {
  return("Conc")
})

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("abstract_value_at_metric", "numeric", "numeric", "nca_time_window"), definition=function(object, time, value, window) {
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
setMethod("getLaTeXName", signature=c("abstract_value_at_metric"), definition = function(x) {
  retValue <- x %>% getName()
  return(retValue)
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("abstract_value_at_metric", "json_element"), definition=function(object, json) {
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(setDefaultNameIfNA(object))
})


