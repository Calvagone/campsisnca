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
    strat_vars = "character",      # stratification variables
    nca_analyses = "nca_analyses"  # NCA analyses
  ),
  prototype=prototype(strat_vars=character(0))
)

#' 
#' Create an empty NCA configuration.
#' 
#' @export
NCAConfiguration <- function() {
  return(new("nca_configuration"))
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("nca_configuration", "nca_analysis"), definition = function(object, x) {
  object@nca_analyses <- object@nca_analyses %>% add(x)
  return(object)
})

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("nca_configuration", "data.frame", "character", "numeric"), definition=function(object, x, strat_vars, quantile_type, ...) {
  object@nca_analyses <- object@nca_analyses %>%
    calculate(x=x, strat_vars=strat_vars, quantile_type=quantile_type, ...)
  return(object)
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("nca_configuration", "json_element"), definition=function(object, json) {
  json <- json@data
  object@strat_vars <- as.character(json$strat_vars)
  object@nca_analyses@list <- json$nca_analyses %>%
    purrr::map(~loadFromJSON(NCAAnalysis(), JSONElement(.x)))
  return(object)
})

setMethod("loadFromJSON", signature=c("nca_configuration", "character"), definition=function(object, json) {
  schema <- system.file("extdata", "campsisnca.schema.json", package="campsisnca")
  return(loadFromJSON(object=object, json=openJSON(json=json, schema=schema)))
})

