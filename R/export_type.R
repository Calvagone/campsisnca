#_______________________________________________________________________________
#----                           export_type                                 ----
#_______________________________________________________________________________

#' Dataframe export type class.
#' 
#' @export
setClass(
  "dataframe_type",
  representation(
  ),
  contains="export_type" 
)

#' Gt summary export type class.
#' 
#' @export
setClass(
  "gtsummary_type",
  representation(
  ),
  contains="export_type" 
)