#_______________________________________________________________________________
#----                         nca_configuration class                       ----
#_______________________________________________________________________________

#' 
#' NCA configuration class.
#' 
#' @export
setClass(
  "nca_configuration",
  representation(
    strat_vars = "character",       # stratification variables
    nca_analyses = "nca_analyses",  # NCA analyses
    nca_metrics = "nca_metrics"     # NCA metrics
  )
)

#' 
#' Create an empty NCA configuration.
#' 
#' @export
NCAConfiguration <- function() {
  return(new("nca_configuration"))
}

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("nca_configuration", "json_element"), definition=function(object, json) {
  json <- json@data
  object@strat_vars <- as.character(json$strat_vars)
  object@nca_analyses@list <- json$nca_analyses %>%
    purrr::map(~loadFromJSON(new("nca_analysis"), JSONElement(.x)))
  object@nca_metrics@list <- json$nca_metrics %>%
    purrr::map(~loadFromJSON(new(.x$type), JSONElement(.x)))
  
  return(object)
})

setMethod("loadFromJSON", signature=c("nca_configuration", "character"), definition=function(object, json) {
  schema <- system.file("extdata", "campsisnca.schema.json", package="campsisnca")
  return(loadFromJSON(object=object, json=openJSON(json=json, schema=schema)))
})

