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
  above <- object@above
  value <- value - object@limit

  size <- length(time)

  lines <- tibble::tibble(
    x1=time[1:(size - 1)],
    y1=value[1:(size - 1)],
    x2=time[2:size],
    y2=value[2:size],
    above=rep(above, size - 1)
    )
  
  durations <- lines %>%
    purrr::pmap_dbl(computeDuration)

  return(sum(durations))    
})

computeDuration <- function(x1, y1, x2, y2, above) {
  duration <- x2 - x1
  
  if (y1 >= 0 && y2 >= 0) {
    return(ifelse(above, duration, 0))
  }
  if (y1 <= 0 && y2 <= 0) {
    return(ifelse(!above, duration, 0))
  }
  
  a <- (y2 - y1) / (x2 - x1)
  b <- y1 - a*x1
  intersection <- -b/a
  
  if (intersection > x1 && intersection < x2) {
    if (y1 >= 0) {
      return(ifelse(above, intersection - x1, x2 - intersection))
    } else {
      return(ifelse(!above, intersection - x1, x2 - intersection))
    }
  }
  stop("Should never occur")
}

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

