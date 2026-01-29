#_______________________________________________________________________________
#----                           nca_analysis class                          ----
#_______________________________________________________________________________

#' 
#' NCA analysis class.
#' 
#' @export
setClass(
  "nca_analysis",
  representation(
    name = "character",              # analysis name
    time_range = "nca_time_range",   # default time range
    variable = "character"              # default variable name
  ),
  contains="pmx_element"
)

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature=c("nca_analysis"), definition = function(x) {
  return(x@name)
})


#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("nca_analysis", "json_element"), definition=function(object, json) {
  json@data$time_range$type <- "nca_time_range"
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(object)
})
