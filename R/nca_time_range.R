#_______________________________________________________________________________
#----                         nca_time_range class                          ----
#_______________________________________________________________________________

validateTimeRange <- function(object) {
  return(expectOneForAll(object, c("start", "end", "time_unit", "exclude_start", "exclude_end")))
}

#' 
#' NCA time range class.
#' 
#' @export
setClass(
  "nca_time_range",
  representation(
    start = "numeric",            # Any starting time
    end = "numeric",              # End time. Note: use Inf for 'last'
    time_unit = "character",      # Default is 'hour'
    exclude_start = "logical",    # Exclude first time point
    exclude_end = "logical"       # Exclude last time point
  ),
  prototype=prototype(start=0, end=Inf, time_unit="hour", exclude_start=FALSE, exclude_end=FALSE),
  validity=validateTimeRange
)

processEndArgument <- function(x) {
  if (x=="last") {
    return(Inf)
  }
  return(x)
}

#' 
#' Create a time range object.
#' 
#' @param start start time of the range
#' @param end end time of the range, use 'last' to specify the end of the simulation output
#' @param time_unit time unit of 'start' and 'end'
#' @param exclude_start exclude start time when filtering
#' @param exclude_end exclude end time when filtering
#' @return a time range object
#' @export
NCATimeRange <- function(start=0, end="last", time_unit="hour", exclude_start=FALSE, exclude_end=FALSE) {
  return(new("nca_time_range",
             start=start,
             end=processEndArgument(end),
             time_unit=time_unit,
             exclude_start=exclude_start,
             exclude_end=exclude_end))
}

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("nca_time_range", "json_element"), definition=function(object, json) {
  json@data$end <- processEndArgument(json@data$end)
  return(mapJSONPropertiesToS4Slots(object=object, json=json))
})
