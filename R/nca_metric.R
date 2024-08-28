#_______________________________________________________________________________
#----                          nca_metric class                             ----
#_______________________________________________________________________________

validateMetric <- function(object) {
  return(expectOneForAll(object, c("variable", "name", "unit", "ivalue_tibble", "stat_display")))
}

#' 
#' NCA metric class. See this class as an interface.
#' 
#' @export
setClass(
  "nca_metric",
  representation(
    x = "data.frame",             # specific dataframe
    variable = "character",       # specific variable, NA if ivalue_tibble=FALSE
    individual = "data.frame",    # individual results
    summary = "data.frame",       # summary results
    name = "character",           # metric name (exported into header)
    unit = "character",           # metric unit (exported into header)
    ivalue_tibble = "logical",    # TRUE, iValue called, FALSE iValueTbl called
    stat_display = "character",   # statistics display (see package gtsummary)
    categorical = "logical",      # FALSE (default): continuous data, TRUE: categorical data
    digits = "character",         # rounding digits definitions for gtsummary
    concentration = "logical"     # concentration-related metric, NA by default
  ),
  contains="pmx_element",
  prototype=prototype(ivalue_tibble=FALSE, categorical=FALSE, concentration=as.logical(NA)),
  validity=validateMetric
)

getStatDisplayDefault <- function(categorical=FALSE) {
  if (categorical) {
    return("{n} / {N} ({p}%)")
  } else {
    return("{median} ({p5}\U2013{p95})") # En dash
  }
}

ncaConstructor <- function(x, variable, name, unit, stat_display, digits, metric_name, def_name) {
  x <- processDataframe(x)
  variable <- processVariable(variable)
  name <- if (is.null(name)) def_name else name
  unit <- processUnit(unit)
  digits <- deparseDigits(digits)
  if (is.null(stat_display)) {
    stat_display <- getStatDisplayDefault(categorical=FALSE) # Continuous by default
  }
  metric <- new(metric_name, x=x, variable=variable, name=name, unit=unit, stat_display=stat_display, digits=digits)
  return(metric)
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("nca_metric", "numeric"), definition=function(object, level, ...) {
  object@individual <- iValues(object=object)
  object@summary <- computeNCAMetricSummary(object=object)
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
      display <- object %>% statDisplayString()
      retValue <- retValue %>%
        dplyr::select(-dplyr::all_of(c("value", "stat")))
      
      if (object@categorical) {
        retValue <- retValue %>%
          dplyr::select(-dplyr::all_of(c("category")))
      }
      
      retValue <- retValue %>%
        dplyr::distinct()
      
      assertthat::assert_that(nrow(retValue)==1, msg="There must be exactly 1 row here")
      
      retValue <- retValue %>%
        dplyr::mutate("summary_stats"=display)
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
setMethod("iValues", signature=c("nca_metric"), definition=function(object, ...) {
  x <- object@x
  variable <- object@variable
  x <- x %>% 
    standardise(variable)

  if (object@ivalue_tibble) {
    # Use group_split and map_df, this way data is a real tibble
    # Otherwise using group_by, .data is a pronoun
    retValue <- x %>%
      dplyr::ungroup() %>%
      dplyr::group_split(ID) %>%
      purrr::map_df(.f=function(data) {
        id <- unique(data$ID)
        ivalue <- object %>% iValueTbl(data=data)
        return(tibble::tibble(id=id, value=ivalue))
      })
  } else {
    retValue <- x %>%
      dplyr::group_by(ID) %>%
      dplyr::summarise(ivalue=object %>% iValue(time=.data$TIME, value=.data[[variable]])) %>%
      dplyr::ungroup() %>%
      dplyr::transmute(id=ID, value=ivalue)
  }

  return(retValue)  
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
  if (nrow(object@summary) > 0) {
    attr <- attributes(object@summary)
    return(attr$comment)
  } else {
    stop("Summary does not exist yet and must be calculated first")
  }
})
