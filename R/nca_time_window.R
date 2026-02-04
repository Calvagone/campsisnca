#_______________________________________________________________________________
#----                         nca_time_window class                          ----
#_______________________________________________________________________________

validateTimeWindow <- function(object) {
  return(expectOneForAll(object, c("start", "end", "time_unit", "exclude_start", "exclude_end")))
}

#' 
#' NCA time window class.
#' 
#' @export
setClass(
  "nca_time_window",
  representation(
    start = "numeric",            # Any starting time
    end = "numeric",              # End time. Note: use Inf for 'last'
    time_unit = "character",      # Default is 'hour'
    exclude_start = "logical",    # Exclude first time point
    exclude_end = "logical"  
  ),
  prototype=prototype(start=0, end=Inf, time_unit="hour", exclude_start=FALSE, exclude_end=FALSE),
  validity=validateTimeWindow
)

#' 
#' Undefined NCA time window class.
#' 
#' @export
setClass(
  "undefined_nca_time_window",
  representation(
  ),
  contains="nca_time_window"
)

processEndArgument <- function(x) {
  if (x=="last") {
    return(Inf)
  }
  return(x)
}

#' 
#' Create a time window object.
#' 
#' @param start start time of window
#' @param end end time of window, use 'last' to specify the end of the simulation output
#' @param time_unit time unit of 'start' and 'end'
#' @param exclude_start exclude start time when filtering
#' @param exclude_end exclude end time when filtering
#' @return a time range object
#' @export
TimeWindow <- function(start=0, end="last", time_unit="hour", exclude_start=FALSE, exclude_end=FALSE) {
  return(new("nca_time_window",
             start=start,
             end=processEndArgument(end),
             time_unit=time_unit,
             exclude_start=exclude_start,
             exclude_end=exclude_end))
}

#' 
#' Create an undefined time window.
#' 
#' @return undefined time window
#' @export
UndefinedTimeWindow <- function() {
  return(new("undefined_nca_time_window"))
}

#_______________________________________________________________________________
#----                           applyTimeWindow                             ----
#_______________________________________________________________________________

#' @rdname applyTimeWindow
setMethod("applyTimeWindow", signature=c("data.frame", "nca_time_window"), definition=function(x, window) {
  return(timerange(x=x, min=window@start, max=window@end, exclmin=window@exclude_start, exclmax=window@exclude_end))
})

#' @rdname applyTimeWindow
setMethod("applyTimeWindow", signature=c("tbl_df", "nca_time_window"), definition=function(x, window) {
  return(timerange(x=x, min=window@start, max=window@end, exclmin=window@exclude_start, exclmax=window@exclude_end))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("nca_time_window", "json_element"), definition=function(object, json) {
  json@data$end <- processEndArgument(json@data$end)
  return(mapJSONPropertiesToS4Slots(object=object, json=json))
})
