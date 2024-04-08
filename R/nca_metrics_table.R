#_______________________________________________________________________________
#----                       nca_metrics_table class                         ----
#_______________________________________________________________________________

#' 
#' NCA metrics table class.
#' 
#' @export
setClass(
  "nca_metrics_table",
  representation(
  ),
  contains="pmx_list",
  prototype = prototype(type="nca_metrics")
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
  if (object %>% length() == 0) {
    stop("No metrics to export")
  }
  if (dest=="dataframe") {
    return(object %>% export(dest=new("dataframe_type"), ...))
  } else if (dest=="gtsummary") {
    return(object %>% export(dest=new("gtsummary_type"), ...))
  } else if (dest=="gt") {
    return(object %>% export(dest=new("gt_type"), ...))
  } else {
    stop("Only dataframe and gtsummary are supported for now")
  }
})

#' @importFrom purrr map_df
#' @importFrom tidyr pivot_wider
setMethod("export", signature=c("nca_metrics_table", "dataframe_type"), definition=function(object, dest, type="summary", ...) {
  
  retValue <- object@list %>% purrr::map_df(.f=~.x %>% export(dest=dest, type=type, ...))
  
  # Remove names on values
  retValue <- retValue %>%
    mutate(value=as.numeric(value))
  
  # Apply transformation is wide format is requested
  if (type == "individual_wide") {
    retValue <- retValue %>%
      tidyr::pivot_wider(names_from=metric, values_from=value) %>%
      dplyr::select(-id) # Automatically remove id column (since 1 row per individual in wide format)
  }
  
  return(retValue)
})

setMethod("export", signature=c("nca_metrics_table", "gtsummary_type"), definition=function(object, dest, subscripts=FALSE, ...) {
  code <- object %>% generateTableCode(subscripts=subscripts)
  table <- object # Table variable needs to be there!
  retValue <- tryCatch(
    expr=eval(expr=parse(text=code)),
    error=function(cond) {
      return(sprintf("Failed to create gtsummary table: %s", cond$message))
    })
  return(retValue)
})

setMethod("export", signature=c("nca_metrics_table", "gt_type"), definition=function(object, dest, subscripts=FALSE, ...) {
  gtsummaryTable <- object %>%
    export(dest=new("gtsummary_type"), subscripts=subscripts, ...)
  
  gtTable <- gtsummaryTable %>%
    toGt(subscripts=subscripts)

  return(gtTable)
})

#' 
#' Gtsummary to Gt.
#' 
#' @param x gtsummary table
#' @param subscripts use subscripts
#' @importFrom gtsummary as_gt
#' @importFrom gt cells_body text_transform
#' @importFrom stringr str_replace_all
#' @export
toGt <- function(x, subscripts=FALSE) {
  gtTable <- x %>%
    gtsummary::as_gt()
  
  if (subscripts) {
    gtTable <- gtTable  %>%
      gt::text_transform(
        locations=gt::cells_body(),
        fn=function(x) {
          return(stringr::str_replace_all(string=x, pattern="(_\\{)([^\\}]+)(\\})", replacement="<sub>\\2</sub>"))
        }
      )
  }
  return(gtTable)
}

#_______________________________________________________________________________
#----                       generateTableCode                               ----
#_______________________________________________________________________________

#' @rdname generateTableCode
setMethod("generateTableCode", signature=c("nca_metrics_table", "logical"), definition=function(object, subscripts, ...) {
  
  scenarios <- object %>% getScenarios()
  stratVariables <- unique(scenarios$name)
  
  init <- "individual <- table %>% campsismod::export(dest=\"dataframe\", type=\"individual_wide\")"
  stats <- getStatisticsCode(object)
  labels <- getLabelsCode(object, subscripts=subscripts)
  digits <- getDigitsCode(object)
  
  if (length(stratVariables)==0) {
    code <- getTableSummaryCode(var="gttable", data="individual", by="NULL", stats=stats, labels=labels, digits=digits)
    
  } else if (length(stratVariables)==1) {
    code <- getTableSummaryCode(var="gttable", data="individual", by=stratVariables, stats=stats, labels=labels, digits=digits)
    
  } else if (length(stratVariables)==2) {
    code <- getTableSummaryCode(var="gttable", data="individual", by=stratVariables, stats=stats, labels=labels, digits=digits)
    
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
