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