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
    name = "character",               # analysis name
    default_start_time = "numeric",   # default start time
    default_end_time = "numeric"      # default end time
  ),
  contains="pmx_element"
)

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("nca_analysis", "json_element"), definition=function(object, json) {
  object <- mapJSONPropertiesToS4Slots(object=object, json=json)
  return(object)
})
