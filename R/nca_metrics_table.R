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
#' @importFrom dplyr all_of any_of filter full_join mutate select pull
#' @importFrom tidyr pivot_wider
setMethod("export", signature=c("nca_metrics_table", "dataframe_type"), definition=function(object, dest, type="summary", ...) {
  
  retValue <- object@list %>% purrr::map_df(.f=~.x %>% export(dest=dest, type=type, ...))
  
  # Apply transformation is wide format is requested
  if (type == "individual_wide") {
    allMetrics <- unique(retValue$metric)
    
    continuousData <- retValue %>%
      dplyr::filter(!categorical) %>%
      dplyr::select(-dplyr::all_of(c("discrete_value", "categorical"))) %>%
      tidyr::pivot_wider(names_from=metric, values_from=value)
    categoricalData <- retValue %>%
      dplyr::filter(categorical) %>%
      dplyr::select(-dplyr::all_of(c("value", "categorical"))) %>%
      tidyr::pivot_wider(names_from=metric, values_from=discrete_value)
    
    categoricalVars <- retValue %>%
      dplyr::filter(categorical) %>%
      dplyr::pull(metric)
    
    # Force "TRUE" or "FALSE" to be recognised as logical
    # Otherwise, auto-detection of dichotomous data will not work with gtsummary
    if (length(categoricalVars) > 0) {
      autoCastLogical <- function(x) {
        if (all(x %in% c("TRUE", "FALSE"))) {
          return(as.logical(x))
        } else {
          return(x)
        }
      }
      categoricalData <- categoricalData %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(categoricalVars), autoCastLogical))
    }

    by <- c("id", names(object@list[[1]]@scenario))
    retValue <- continuousData %>%
      dplyr::full_join(categoricalData, by=by) %>%
      dplyr::relocate(dplyr::any_of(c(by, allMetrics)))
  }
  
  if (type == "individual") {
    # Categorical not needed since 2 columns 'value' or 'discrete_value' 
    retValue <- retValue %>%
      dplyr::select(-dplyr::all_of(c("categorical")))
  }
  
  return(retValue)
})

#' @inheritParams generateTableCode
setMethod("export", signature=c("nca_metrics_table", "gtsummary_type"),
          definition=function(object, dest, subscripts=NULL, all_dichotomous_levels=NULL, max_2dim=NULL, combine_with=NULL, header_label=NULL, ...) {
  code <- object %>% generateTableCode(subscripts=subscripts, all_dichotomous_levels=all_dichotomous_levels, max_2dim=max_2dim, combine_with=combine_with, header_label=header_label, ...)
  table <- object # Table variable needs to be there!
  retValue <- tryCatch(
    expr=eval(expr=parse(text=code)),
    error=function(cond) {
      return(sprintf("Failed to create gtsummary table: %s", cond$message))
    })
  return(retValue)
})

#' @inheritParams generateTableCode
setMethod("export", signature=c("nca_metrics_table", "gt_type"),
          definition=function(object, dest, subscripts=NULL, all_dichotomous_levels=NULL, max_2dim=NULL, combine_with=NULL, header_label=NULL, ...) {
  gtsummaryTable <- object %>%
    export(dest=new("gtsummary_type"), subscripts=subscripts, all_dichotomous_levels=all_dichotomous_levels, max_2dim=max_2dim, combine_with=combine_with, header_label=header_label, ...)
  
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
  if (is.null(subscripts)) {
    subscripts <- FALSE
  }
  
  # Adapt footnote (conversion of custom statistics)
  footnote <- x$table_styling$footnote
  if (is(footnote, "tbl_df")) {
    x$table_styling$footnote$footnote <- adaptFootnote(footnote$footnote)
  }
  
  # Convert to GT table
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
setMethod("generateTableCode", signature=c("nca_metrics_table", "logical", "logical", "logical", "character", "character"),
          definition=function(object, subscripts, all_dichotomous_levels, max_2dim, combine_with, header_label, ...) {
  
  init <- "individual <- table"
  if (max_2dim) {
    init <- init %>%
      addPipeLayer("reduceTo2Dimensions()")
    object <- object %>% reduceTo2Dimensions()
  }
  init <- init %>%
    addPipeLayer("export(dest=\"dataframe\", type=\"individual_wide\")") %>%
    addPipeLayer("dplyr::select(-id)")
  
  scenarios <- object %>% getScenarios()
  stratVariables <- unique(scenarios$name)
  
  stats <- getStatisticsCode(object)
  type <- getVariableTypeCode(object, all_dichotomous_levels=all_dichotomous_levels)
  labels <- getLabelsCode(object, subscripts=subscripts)
  digits <- getDigitsCode(object)
  
  if (length(stratVariables) <= 2) {
    code <- getTableSummaryCode(var="gttable", data="individual", by=stratVariables,
                                stats=stats, type=type, labels=labels, digits=digits,
                                combine_with=combine_with, header_label=header_label)
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

#_______________________________________________________________________________
#----                       reduceTo2Dimensions                             ----
#_______________________________________________________________________________

#' @rdname reduceTo2Dimensions
setMethod("reduceTo2Dimensions", signature=c("nca_metrics_table"), definition=function(object, ...) {
  object@list <- object@list %>%
    purrr::map(~reduceTo2Dimensions(.x, ...))
  
  return(object)
})
