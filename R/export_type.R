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

#' Kable export type class.
#' 
#' @export
setClass(
  "kable_type",
  representation(
  ),
  contains="export_type" 
)