#_______________________________________________________________________________
#----                       nca_metrics_table class                         ----
#_______________________________________________________________________________

validateNCAMetricsTable <- function(object) {
  return(expectOneForAll(object, c("unit_linebreak")))
}

#' 
#' NCA metrics table class.
#' 
#' @export
setClass(
  "nca_metrics_table",
  representation(
    rounding = "function", # default rounding function
    unit_linebreak = "logical"
  ),
  contains="pmx_list",
  prototype = prototype(type="nca_metrics"),
  validity=validateNCAMetricsTable
)

#' 
#' NCA metrics table.
#' 
#' @param rounding rounding function (default arguments should be: x (values), 
#' metric (metric name), stat (low, med, up))
#' @param unitLineBreak line break between header and unit, logical value
#' @export
NCAMetricsTable <- function(rounding=NULL, unitLineBreak=FALSE) {
  rounding <- if (is.null(rounding)) defaultRounding else rounding
  assertthat::assert_that(is.function(rounding), msg="rounding must be a rounding function")
  return(new("nca_metrics_table", rounding=rounding, unit_linebreak=unitLineBreak))
}

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

setMethod("export", signature=c("nca_metrics_table", "character"), definition=function(object, dest, ...) {
  if (object %>% length() == 0) {
    stop("No metrics to export")
  }
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

setMethod("export", signature=c("nca_metrics_table", "kable_type"), definition=function(object, dest, ...) {
  format <- campsismod::processExtraArg(args=list(...), name="format", default="html")
  
  firstScenario <- object@list[[1]]@scenario
  names <- names(firstScenario)
  vgroup <- names[1]
  vsubgroup <- NULL
  if(names %>% length() > 1) {
    vsubgroup <- names[2]
  }
  df <- object %>% export(dest="dataframe")
  df <- df %>% statsToCell(rounding=object@rounding)
  df <- df %>% makeTable(vgroup=vgroup, vsubgroup=vsubgroup)
  kable <- makeKable(x=object, df=df, vgroup=vgroup, vsubgroup=vsubgroup, format=format)
  return(kable)
})

statsToCell <- function(x, rounding) {
  x <-
    x %>% dplyr::mutate(
      cell=paste0(
        med %>% rounding(metric=metric, stat="med"),
        " [",
        low %>% rounding(metric=metric, stat="low"),
        "-",
        up %>% rounding(metric=metric, stat="up"),
        "]"
      )
    )
  x <- x %>% dplyr::select(-med, -low, -up)
  return(x)
}

#_______________________________________________________________________________
#----                              getUnit                                  ----
#_______________________________________________________________________________

setMethod("getUnit", signature=c("nca_metrics_table", "character"), definition=function(object, metric, ...) {
  if (object %>% length()==0) {
    stop("No metrics in table at this stage")
  }
  return(object@list[[1]] %>% getUnit(metric=metric, ...))
})
