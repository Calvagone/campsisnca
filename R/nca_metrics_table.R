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
    return(object %>% export(dest=new("dataframe_type"), ...))
  } else if (dest=="gtsummary") {
    return(object %>% export(dest=new("gtsummary_type"), ...))
  } else {
    stop("Only dataframe and gtsummary are supported for now")
  }
})

#' @importFrom purrr map_df
#' @importFrom tidyr pivot_wider
setMethod("export", signature=c("nca_metrics_table", "dataframe_type"), definition=function(object, dest, type="summary", ...) {
  
  retValue <- object@list %>% purrr::map_df(.f=~.x %>% export(dest=dest, type=type, ...))
  
  # Apply transformation is wide format is requested
  if (type == "individual_wide") {
    retValue <- retValue %>%
      tidyr::pivot_wider(names_from=metric, values_from=value) %>%
      dplyr::select(-id) # Automatically remove id column (since 1 row per individual in wide format)
  }
  
  return(retValue)
})

setMethod("export", signature=c("nca_metrics_table", "gtsummary_type"), definition=function(object, dest, ...) {
  code <- object %>% generateTableCode()
  table <- object # Table variable needs to be there!
  retValue <- tryCatch(
    expr=eval(expr=parse(text=code)),
    error=function(cond) {
      return(sprintf("Failed to create gtsummary table: %s", cond$message))
    })
  return(retValue)
})

#_______________________________________________________________________________
#----                       generateTableCode                               ----
#_______________________________________________________________________________

#' @rdname generateTableCode
setMethod("generateTableCode", signature=c("nca_metrics_table"), definition=function(object, ...) {
  
  scenarios <- object %>% getScenarios()
  stratVariables <- unique(scenarios$name)
  
  init <- "individual <- table %>% campsismod::export(dest=\"dataframe\", type=\"individual_wide\")"
  stats <- getStatisticsCode(object)
  
  if (length(stratVariables)==0) {
    code <- getTableSummaryCode(var="gttable", data="individual", by="NULL", stats=stats)
    
  } else if (length(stratVariables)==1) {
    code <- getTableSummaryCode(var="gttable", data="individual", by=stratVariables, stats=stats)
    
  } else if (length(stratVariables)==2) {
    code <- getTableSummaryCode(var="gttable", data="individual", by=stratVariables, stats=stats)
    
  } else {
    stop("Too many stratification variables")
  }

  return(paste0(c(init, code, "gttable"), collapse="\n"))
})

#_______________________________________________________________________________
#----                           getScenarios                                ----
#_______________________________________________________________________________

#' @rdname getScenarios
setMethod("getScenarios", signature=c("nca_metrics_table"), definition=function(object, ...) {
  retValue <- object@list %>% purrr::map_df(~tibble::enframe(.x@scenario))
  return(retValue)
})

#_______________________________________________________________________________
#----                              getUnit                                  ----
#_______________________________________________________________________________

#' @rdname getUnit
setMethod("getUnit", signature=c("nca_metrics_table", "character"), definition=function(object, metric, ...) {
  if (object %>% length()==0) {
    stop("No metrics in table at this stage")
  }
  return(object@list[[1]] %>% getUnit(metric=metric, ...))
})
