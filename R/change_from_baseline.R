#_______________________________________________________________________________
#----                      baseline_metric classes                          ----
#_______________________________________________________________________________

validate_baseline_metric <- function(object) {
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
  "baseline_metric",
  representation(
    method="character"
  ),
  contains="nca_metric",
  validity=validate_baseline_metric
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
                           metric_name="baseline_metric")
  
  # Populate the custom slot
  metric@method <- method
  
  return(set_default_name_if_na(metric))
}

# Alias for developers/users who prefer the quick acronym
#' @export
CFB <- ChangeFromBaseline

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature=c("baseline_metric"), definition=function(object, ...) {
  # Translate the internal method slot into the standard pharmacometric acronym
  switch(object@method,
         "absolute" = "CFB",
         "percent"  = "PCFB",
         "ratio"    = "Ratio",
         "log"      = "CFBlog",
         "CFB") # Fallback
})

#_______________________________________________________________________________
#----                                i_value                                ----
#_______________________________________________________________________________

get_baseline_value <- function(time, value) {
  if (length(value) == 0) return(NA_real_)
  return(value[1]) # Or value[which.min(time)] if time is not pre-sorted
}

#' @rdname i_value
setMethod("i_value", signature=c("baseline_metric", "numeric", "numeric"), definition=function(object, time, value) {
  y0 <- get_baseline_value(time, value)
  
  # If baseline is missing, the output vectors should gracefully propagate NA
  if (is.na(y0)) return(rep(NA_real_, length(value)))
  
  # Execute calculation based on the selected method
  switch(object@method,
         "absolute" = {
           return(value - y0)
         },
         "percent" = {
           if (y0 == 0) return(rep(NA_real_, length(value)))
           return(((value - y0) / y0) * 100)
         },
         "ratio" = {
           if (y0 == 0) return(rep(NA_real_, length(value)))
           return(value / y0)
         },
         "log" = {
           if (y0 <= 0 || any(value <= 0, na.rm = TRUE)) return(rep(NA_real_, length(value)))
           return(log(value) - log(y0))
         },
         stop("Unknown calculation method")
  )
})

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature=c("baseline_metric"), definition = function(x) {
  if (x@method=="log") {
    return(subscript_occurrence(x %>% getName(), "log"))
  } else {
    return(x %>% getName())
  }
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("baseline_metric", "json_element"), definition=function(object, json) {
  # Load the standard metric components first
  object <- loadMetricFromJSON(object=object, json=json)
  
  # Retrieve the custom method parameter from the parsed JSON list
  if ("method" %in% names(json)) {
    object@method <- json[["method"]]
  } else {
    object@method <- "absolute" # Fallback/Default
  }
  
  return(object)
})
