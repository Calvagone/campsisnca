#_______________________________________________________________________________
#----                          auc_metric class                             ----
#_______________________________________________________________________________

validate_auc_metric <- function(object) {
  return(expect_one(object, "method"))
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
  validity=validate_auc_metric
)

#' 
#' AUC.
#' 
#' @inheritParams metrics_params
#' @param method method:
#' * 1: linear up - linear down
#' * 2: linear up - logarithmic down
#' * 3: linear before Tmax, logarithmic after Tmax
#' @export
AUC <- function(variable=NULL, window=NULL, method=1, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- nca_constructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="auc_metric")
  assertthat::assert_that(method %in% c(1,2,3), msg="method must be 1, 2 or 3")
  metric@method <- as.integer(method)
  return(set_default_name_if_na(metric))
}

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature=c("auc_metric"), definition=function(object, ...) {
  return("AUC") 
})

#_______________________________________________________________________________
#----                            i_value                                    ----
#_______________________________________________________________________________

#' @rdname i_value
setMethod("i_value", signature=c("auc_metric", "numeric", "numeric"), definition=function(object, time, value) {
  return(trap(x=time, y=value, method=object@method))    
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

auc_method_to_integer <- function(method) {
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

setMethod("load_from_json", signature=c("auc_metric", "json_element"), definition=function(object, json) {
  json@data$method <- auc_method_to_integer(json@data$method)
  return(load_metric_from_json(object=object, json=json))
})

