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
    above="logical",
    strictly="logical"
  ),
  contains="nca_metric",
  validity=validateTimeAboveBelowLimitMetric
)

#' 
#' Time above a certain limit.
#' 
#' @inheritParams metricsParams
#' @param limit the limit to compare the variable against
#' @param strictly whether the variable must be strictly above the limit
#' @export
TimeAboveLimit <- function(x=NULL, variable=NULL, limit=NULL, strictly=FALSE, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="time_above_below_limit", def_name="Timeabove")
  metric@limit <- limit
  metric@above <- TRUE
  metric@strictly <- strictly
  return(metric)
}

#' 
#' Time below a certain limit.
#' 
#' @inheritParams metricsParams
#' @param limit the limit to compare the variable against
#' @param strictly whether the variable must be strictly below the limit
#' @export
TimeBelowLimit <- function(x=NULL, variable=NULL, limit=NULL, strictly=FALSE, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  metric <- ncaConstructor(x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="time_above_below_limit", def_name="Timebelow")
  metric@limit <- limit
  metric@above <- FALSE
  metric@strictly <- strictly
  return(metric)
}

#_______________________________________________________________________________
#----                            iValue                                     ----
#_______________________________________________________________________________

#' @rdname iValue
setMethod("iValue", signature=c("time_above_below_limit", "numeric", "numeric"), definition=function(object, time, value) {
  above <- object@above
  value <- value - object@limit
  strictly <- object@strictly

  size <- length(time)

  lines <- tibble::tibble(
    x1=time[1:(size - 1)],
    y1=value[1:(size - 1)],
    x2=time[2:size],
    y2=value[2:size],
    above=rep(above, size - 1),
    strictly=rep(strictly, size - 1)
    )
  
  durations <- lines %>%
    purrr::pmap_dbl(computeTimeAboveBelow)

  return(sum(durations))    
})

#' 
#' Compute the duration of a line segment above or below a certain limit.
#' 
#' @param x1 x-coordinate of the first point
#' @param y1 y-coordinate of the first point
#' @param x2 x-coordinate of the second point
#' @param y2 y-coordinate of the second point
#' @param above whether the line segment is above the limit
#' @param strictly whether the line segment is strictly above or below the limit
#' @return the duration of the line segment above or below the limit
#' @export
computeTimeAboveBelow <- function(x1, y1, x2, y2, above, strictly) {
  duration <- x2 - x1
  if (duration==0) {
    stop("Sample times must be unique")
  }
  
  # Both values are 0
  if (y1 == 0 && y2 == 0) {
    return(ifelse(strictly, 0, duration))
  }
  
  # Both values above 0
  if (y1 > 0 && y2 > 0) {
    return(ifelse(above, duration, 0))
  }
  
  # Both values below 0
  if (y1 < 0 && y2 < 0) {
    return(ifelse(!above, duration, 0))
  }
  
  # Only 1 value is 0
  if ((y1 == 0 && y2 > 0) || (y1 > 0 && y2 == 0)) {
    return(ifelse(above, duration, 0))
  } else if ((y1 == 0 && y2 < 0) || (y1 < 0 && y2 == 0)) {
    return(ifelse(!above, duration, 0))
  }
  
  # Otherwise, find intersection
  a <- (y2 - y1) / (x2 - x1)
  b <- y1 - a*x1
  intersection <- -b/a
  
  if (intersection > x1 && intersection < x2) {
    if (y1 > 0) {
      return(ifelse(above, intersection - x1, x2 - intersection))
    } else {
      return(ifelse(!above, intersection - x1, x2 - intersection))
    }
  }
  stop("intersection should lie between x1 or x2")
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

