#_______________________________________________________________________________
#----                      baseline_metric classes                          ----
#_______________________________________________________________________________

validate_cfb_metric <- function(object) {
  valid_methods <- c("absolute", "percent", "ratio", "log")
  if (!object@method %in% valid_methods) {
    return(paste0("Method must be one of: ", paste(valid_methods, collapse = ", ")))
  }
  return(TRUE)
}

#' 
#' Change from Baseline metric class.
#' 
#' @slot method Character string specifying the CFB method ("absolute", "percent", "ratio", "log").
#' @export
setClass(
  "cfb_metric",
  representation(
    method="character"
  ),
  contains="nca_metric",
  prototype=prototype(method="absolute"),
  validity=validate_cfb_metric
)

#_______________________________________________________________________________
#----                         ChangeFromBaseline                            ----
#_______________________________________________________________________________

#' 
#' Change from Baseline (CFB).
#' 
#' @param method Character string specifying the calculation method. Must be one of 
#'   "absolute" (default), "percent", "ratio", or "log".
#' @inheritParams metrics_params
#' @export
ChangeFromBaseline <- function(variable=NULL, window=NULL, name=NULL, unit=NULL, 
                               stat_display=NULL, digits=NULL, method="absolute") {
  
  # Map or validate the incoming method argument (forces lowercase for robustness)
  method <- match.arg(tolower(method), c("absolute", "percent", "ratio", "log"))
  
  # Construct base metric using your package's S4 constructor
  metric <- nca_constructor(variable=variable, window=window, name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="cfb_metric")
  
  # Populate the custom slot
  metric@method <- method
  
  return(set_default_name_if_na(metric))
}

#' 
#' Alias for Change from Baseline (CFB).
#' 
#' @param method Character string specifying the calculation method. Must be one of 
#'   "absolute" (default), "percent", "ratio", or "log".
#' @inheritParams metrics_params
#' @export
CFB <- ChangeFromBaseline

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature=c("cfb_metric"), definition=function(object, ...) {
  # Translate the internal method slot into the standard pharmacometric acronym
  switch(object@method,
         "absolute" = "CFB",
         "percent"  = "PCFB",
         "ratio"    = "Ratio",
         "log"      = "CFBlog",
         "CFB") # Fallback
})

#_______________________________________________________________________________
#----                               i_value                                 ----
#_______________________________________________________________________________

get_baseline_value <- function(time, value) {
  if (length(value) == 0) return(NA_real_)
  # Safely gets the baseline corresponding to the earliest time
  return(value[which.min(time)]) 
}

#' @rdname i_value
setMethod("i_value", signature=c("cfb_metric", "numeric", "numeric"), definition=function(object, time, value) {
  # Guard: return NA if we have no observations
  if (length(value) == 0) return(NA_real_)
  
  # 1. Identify baseline (earliest time) and final (latest time) values
  y0 <- get_baseline_value(time, value)
  
  latest_idx <- which.max(time)
  y_last <- value[latest_idx]
  
  # If either the baseline or the latest value is missing, return NA
  if (is.na(y0) || is.na(y_last)) return(NA_real_)
  
  # 2. Execute calculation based on the selected method
  switch(object@method,
         "absolute" = {
           return(y_last - y0)
         },
         "percent" = {
           if (y0 == 0) return(NA_real_)
           return(((y_last - y0) / y0) * 100)
         },
         "ratio" = {
           if (y0 == 0) return(NA_real_)
           return(y_last / y0)
         },
         "log" = {
           if (y0 <= 0 || y_last <= 0) return(NA_real_)
           return(log(y_last) - log(y0))
         },
         stop("Unknown calculation method")
  )
})

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature=c("cfb_metric"), definition = function(x) {
  if (x@method=="log") {
    return(subscript_occurrence(x %>% getName(), "log"))
  } else {
    return(x %>% getName())
  }
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("cfb_metric", "json_element"), definition=function(object, json) {
  # Load the standard metric components first
  object <- loadMetricFromJSON(object=object, json=json)
    
  return(object)
})
