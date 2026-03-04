#_______________________________________________________________________________
#----                          nca_analyses class                           ----
#_______________________________________________________________________________

#' 
#' NCA analyses class.
#' 
#' @export
setClass(
  "nca_analyses",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="nca_analysis")
)

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("nca_analyses", "data.frame", "numeric"), definition=function(object, x, quantile_type, ...) {
  object@list <- object@list %>%
    purrr::map(~.x %>% calculate(x=x, quantile_type=quantile_type, ...))
  return(object)
})
