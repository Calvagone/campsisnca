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
    digits = "character"          # rounding digits definitions for gtsummary 
  ),
  contains="pmx_element",
  prototype=prototype(ivalue_tibble=FALSE, categorical=FALSE),
  validity=validateMetric
)

getStatDisplayDefault <- function(categorical=FALSE) {
  if (categorical) {
    return("{n} / {N} ({p}%)")
  } else {
    return("{median} [{p5}-{p95}]")
  }
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("nca_metric", "numeric"), definition=function(object, level, ...) {
  object@individual <- iValues(object=object)
  object@summary <- computeTableSummary(object=object)
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
setMethod("export", signature=c("nca_metric", "dataframe_type"), definition=function(object, dest, type="summary", ...) {
  if (type == "summary") {
    if (nrow(object@summary) == 0) {
      stop(paste0("Metric ", object %>% getName(), " is empty (please call calculate())"))
    }
    retValue <- tibble::tibble(metric=object %>% getName(), object@summary)
  
  } else if (type == "individual" || type == "individual_wide") {
    if (nrow(object@individual) == 0) {
      stop(paste0("Metric ", object %>% getName(), " is empty (please call calculate())"))
    }
    retValue <- tibble::tibble(metric=object %>% getName(), object@individual)

  } else {
    stop("Argument type can only be 'summary', 'individual' or 'individual_wide'.")
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

#' @rdname statDisplayString
setMethod("statDisplayString", signature=c("nca_metric"), definition=function(object, ...) {
  if (nrow(object@summary) > 0) {
    attr <- attributes(object@summary)
    
    if (object@categorical) {
      comment <- attr$comment
      comment <- comment[!is.na(comment)] # First item always NA (don't know why)
      categories <- unique(object@individual$value)
      if (length(comment)==length(categories)) {
        order <- order(categories)
        retValue <- paste0(paste0(categories[order], ": ", comment[order]), collapse=", ")
      } else {
        retValue <- "Can't derive stat display"
      }
    } else {
      retValue <- attr$comment
    }
    return(retValue)
  } else {
    stop("Summary does not exist yet and must be calculated first")
  }
})
