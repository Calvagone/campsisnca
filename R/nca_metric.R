#_______________________________________________________________________________
#----                          nca_metric class                             ----
#_______________________________________________________________________________

validateMetric <- function(object) {
  return(expectOneForAll(object, c("variable", "window", "name", "unit", "ivalue_tibble",
                                   "stat_display", "categorical", "concentration")))
}

getStatDisplayDefault <- function(categorical=FALSE) {
  if (categorical) {
    return("{n} / {N} ({p}%)")
  } else {
    return("{median} ({p5}\U2013{p95})") # En dash
  }
}

#' 
#' NCA metric class. See this class as abstract class.
#' 
#' @export
setClass(
  "nca_metric",
  representation(
    variable = "character",       # specific variable, NA if ivalue_tibble=FALSE
    window = "nca_time_window",    # time range for filtering data
    name = "character",           # metric name (exported into header)
    unit = "character",           # metric unit (exported into header)
    ivalue_tibble = "logical",    # TRUE, iValue called, FALSE iValueTbl called
    categorical = "logical",      # FALSE (default): continuous data, TRUE: categorical data
    stat_display = "character",   # statistics display (see package gtsummary)
    digits = "character",         # rounding digits definitions for gtsummary
    concentration = "logical",    # concentration-related metric, NA by default
    individual = "data.frame",    # transient individual results
    summary = "data.frame",       # transient summary results
    summary_pretty = "data.frame" # transient summary_pretty results (statistics are glued)
  ),
  contains="pmx_element",
  prototype=prototype(variable=as.character(NA), window=UndefinedTimeWindow(), name=as.character(NA), unit=as.character(NA),
                      ivalue_tibble=FALSE, categorical=FALSE, stat_display=getStatDisplayDefault(categorical=FALSE),
                      digits=character(0), concentration=as.logical(NA)),
  validity=validateMetric
)

ncaConstructor <- function(variable, window, name, unit, stat_display, digits, metric_name) {
  if (is.null(name)) {
    name <- as.character(NA)
  }
  if (is.null(window)) {
    window <- UndefinedTimeWindow()
  }
  variable <- processVariable(variable)
  unit <- processUnit(unit)
  digits <- deparseDigits(digits)
  if (is.null(stat_display)) {
    stat_display <- getStatDisplayDefault(categorical=FALSE) # Continuous by default
  }
  metric <- new(metric_name, variable=variable, window=window, name=name, unit=unit, stat_display=stat_display, digits=digits)
  return(metric)
}

setDefaultNameIfNA <- function(object) {
  if (is.na(object@name)) {
    object@name <- object %>% getDefaultName()
  }
  return(object)
} 

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("nca_metric", "data.frame", "character", "numeric"), definition=function(object, x, strat_vars, quantile_type, ...) {
  object@individual <- iValues(object=object, x=x, strat_vars=strat_vars)
  structuredObj <- computeNCAMetricSummary(object=object, strat_vars=strat_vars, quantile_type=quantile_type)
  object@summary <- structuredObj$summary
  object@summary_pretty <- structuredObj$summary_pretty
  return(object)    
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @importFrom stringr str_replace_all
subscriptOccurrence <- function(x, occurrence, replacement=NULL) {
  if (is.null(replacement)) {
     replacement <- sprintf("_{%s}", occurrence)
  } else {
    replacement <- sprintf("_{%s}", replacement)
  }
  return(stringr::str_replace_all(string=x, pattern=occurrence, replacement=replacement))
}

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("nca_metric"), definition = function(x) {
  return(x %>% getName())
})

#_______________________________________________________________________________
#----                           getDefaultName                              ----
#_______________________________________________________________________________

#' @rdname getDefaultName
setMethod("getDefaultName", signature=c("nca_metric"), definition=function(object, ...) {
  return("Unknown metric name") 
})

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature=c("nca_metric"), definition = function(x) {
  return(x@name)
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

setMethod("export", signature=c("nca_metric", "character"), definition=function(object, dest, ...) {
  if (dest=="dataframe") {
    return(object %>% export(dest=new("dataframe_type"), ...))
  } else {
    stop("Only dataframe is supported for now")
  }
})

#' @importFrom tibble tibble
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr all_of distinct mutate select relocate
setMethod("export", signature=c("nca_metric", "dataframe_type"), definition=function(object, dest, type="summary", ...) {
  if (type == "summary" || type == "summary_wide" || type == "summary_pretty") {
    if (nrow(object@summary) == 0) {
      stop(paste0("Metric ", object %>% getName(), " is empty (please call calculate())"))
    }
    retValue <- tibble::tibble(metric=object %>% getName(), object@summary) %>%
      dplyr::mutate(value=as.numeric(value)) # Remove names on values (e.g. if quantile was used)
    
    if (type == "summary_wide") {
      retValue <- retValue %>%
        tidyr::pivot_wider(names_from=stat, values_from=value)

    } else if (type == "summary_pretty") {
      retValue <- tibble::tibble(metric=object %>% getName(), object@summary_pretty)
    }
  
  } else if (type == "individual" || type == "individual_wide") {
    if (nrow(object@individual) == 0) {
      stop(paste0("Metric ", object %>% getName(), " is empty (please call calculate())"))
    }
    # Always 2 columns 'value' or 'discrete_value' based on field categorical
    individual <- object@individual
    if (object@categorical) {
      individual <- individual %>%
        dplyr::mutate(discrete_value=as.character(value)) %>%
        dplyr::mutate(value=as.numeric(NA)) %>%
        dplyr::relocate(dplyr::all_of(c("value", "discrete_value"))) # value first
    } else {
      individual <- individual %>%
        dplyr::mutate(value=as.numeric(value)) %>%
        dplyr::mutate(discrete_value=as.character(NA))
    }
    # Keep track of categorical in dataframe
    individual <- individual %>%
      dplyr::mutate(categorical=object@categorical)
    
    retValue <- tibble::tibble(metric=object %>% getName(), individual)
    
  } else {
    stop("Argument type can be 'summary', 'summary_wide', 'summary_pretty', 'individual' or 'individual_wide'.")
  }
  return(retValue)
})

#_______________________________________________________________________________
#----                             iValues                                   ----
#_______________________________________________________________________________

#' @rdname iValues
#' @importFrom dplyr group_by summarise transmute ungroup
#' @importFrom tibble tibble
#' @importFrom purrr map_df
setMethod("iValues", signature=c("nca_metric"), definition=function(object, x, strat_vars, ...) {
  variable <- object@variable
  if (length(variable)==0) {
    stop(sprintf("No variable provided for metric '%s'", x %>% getName()))
  }
  x <- x %>% 
    standardise(variable=variable, strat_vars=strat_vars)
  x <- x %>%
    applyTimeWindow(object@window)

  if (object@ivalue_tibble) {
    retValue <- x %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(strat_vars, "ID")))) %>%
      dplyr::group_modify(~ {
        ivalue <- object %>% iValueTbl(data=.x)
        tibble::tibble(value=ivalue)
      }) %>%
      dplyr::ungroup()
  } else {
    retValue <- x %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(c(strat_vars, "ID")))) %>%
      dplyr::summarise(value=object %>% iValue(time=.data$TIME, value=.data[[variable]])) %>%
      dplyr::ungroup()
  }
  
  # Stratification variables back to character vectors
  retValue <- retValue %>%
    dplyr::mutate(dplyr::across(dplyr::all_of(strat_vars), as.character))

  return(retValue %>% dplyr::rename(id=ID))  
})

#_______________________________________________________________________________
#----                         statDisplayString                             ----
#_______________________________________________________________________________

getDiscreteCategories <- function(object) {
  # Unfortunately, this is no other way to retrieve the categories corresponding to the categorical stat values in gtsummary
  # gtsummary categorical stat values are given in the same order as the alphabetically-sorted categories present in the data
  return(base::sort(unique(object@individual$value)))
}

#' @rdname statDisplayString
setMethod("statDisplayString", signature=c("nca_metric"), definition=function(object, ...) {
  if (nrow(object@summary_pretty) > 0) {
    if ("category" %in% colnames(object@summary_pretty)) {
      temp <- object@summary_pretty %>% dplyr::mutate(summary_stats_final=paste0(category, ": ", summary_stats))
      return(as.character(temp$summary_stats_final))
    } else {
      return(as.character(object@summary_pretty$summary_stats))
    }
  } else {
    stop("Summary does not exist yet and must be calculated first")
  }
})
