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
setMethod("calculate", signature=c("nca_analyses", "data.frame", "nca_options"), definition=function(object, x, options, ...) {
  object@list <- object@list %>%
    purrr::map(~.x %>% calculate(x=x, options=options, ...))
  return(object)
})
