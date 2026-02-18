#_______________________________________________________________________________
#----                          auc_metric class                             ----
#_______________________________________________________________________________

validateAUCMetric <- function(object) {
  return(expectOne(object, "method"))
}

#' 
#' AUC metric class.
#' 
#' @export
setClass(
  "auc_metric",
  representation(
    method = "integer"
  ),
  contains="nca_metric",
  prototype=prototype(method=1L),
  validity=validateAUCMetric
)

#' 
#' AUC.
#' 
#' @inheritParams metricsParams
#' @param method method:
#' * 1: linear up - linear down
#' * 2: linear up - logarithmic down
#' * 3: linear before Tmax, logarithmic after Tmax
#' @export
AUC <- function(variable=NULL, window=NULL, method=1, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="auc_metric")
  assertthat::assert_that(method %in% c(1,2,3), msg="method must be 1, 2 or 3")
  metric@method <- as.integer(method)
  return(setDefaultNameIfNA(metric))
}

#_______________________________________________________________________________
#----                           getDefaultName                              ----
#_______________________________________________________________________________

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("auc_metric"), definition=function(object, ...) {
  return("AUC") 
})

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("auc_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(trap(x=time, y=value, method=object@method))    
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

aucMethodToInteger <- function(method) {
  if (length(method)==0) {
    return(1L) # Default
  } else {
    if (method=="linlin") {
      return(1L)
    } else if (method=="linlog") {
      return(2L)
    } else if (method=="tmax_linlog") {
      return(3L)
    }
  }
  stop(sprintf("Unknown AUC method '%s'", method))
}

setMethod("loadFromJSON", signature=c("auc_metric", "json_element"), definition=function(object, json) {
  json@data$method <- aucMethodToInteger(json@data$method)
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(setDefaultNameIfNA(object))
})

