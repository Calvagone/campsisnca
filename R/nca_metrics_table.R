#_______________________________________________________________________________
#----                       nca_metrics_table class                         ----
#_______________________________________________________________________________

validateNCAMetricsTable <- function(object) {
  return(TRUE)
}

#' 
#' NCA metrics table class.
#' 
#' @export
setClass(
  "nca_metrics_table",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="nca_metrics"),
  validity=validateNCAMetricsTable
)

#' 
#' NCA metrics table.
#' 
#' @export
NCAMetricsTable <- function() {
  return(new("nca_metrics_table"))
}

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

setMethod("export", signature=c("nca_metrics_table", "character"), definition=function(object, dest, ...) {
  if (dest=="dataframe") {
    return(object %>% export(new("dataframe_type"), ...))
  } else if (dest=="kable") {
    return(object %>% export(new("kable_type"), ...))
  } else {
    stop("Only dataframe and kable are supported for now")
  }
})

setMethod("export", signature=c("nca_metrics_table", "dataframe_type"), definition=function(object, dest, ...) {
  return(object@list %>% purrr::map_df(.f=~.x %>% export(dest=dest)))
})

setMethod("export", signature=c("nca_metrics_table", "kable_type"),
          definition=function(object, dest, vgroup, vsubgroup, ...) {
  metrics <- object %>% export(dest="dataframe")
  metrics <- metrics %>% statsToCell()
  return(metrics %>% makeTable(vgroup=vgroup, vsubgroup=vsubgroup))
})

statsToCell <- function(x) {
  x <- x %>% dplyr::mutate(cell=paste0(med %>% roundCustom(), " [", low %>% roundCustom(), "-", up %>% roundCustom(), "]"))
  x <- x %>% dplyr::select(-med, -low, -up)
  return(x)
}

roundCustom <- function(x) {
  ifelse(x < 1, signif(x, digits=1), round(x))
}

