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
    title = "character",
    subtitle = "character",
    nca_analyses = "nca_analyses",  # NCA analyses
    nca_options = "nca_options",
    tab_options = "list"
  ),
  prototype = prototype(nca_analyses=new("nca_analyses"), title=NA_character_,
                        subtitle=NA_character_, nca_options=NCAOptions(),
                        tab_options=list())
)

#' 
#' NCA metrics table (deprecated).
#' 
#' @param title table title, optional character value
#' @param subtitle table subtitle, optional character value
#' @param nca_options NCA options, see ?NCAOptions
#' @param tab_options list of options to pass to gt::tab_options
#' @param json path to JSON table file or JSON content in string form
#' @export
NCAMetricsTable <- function(title=NULL, subtitle=NULL, nca_options=NCAOptions(), tab_options=list(), json=NULL) {
  .Deprecated("NCATable")
  return(NCATable(title=title, subtitle=subtitle, nca_options=nca_options,
                  tab_options=tab_options, json=json))
}

#' 
#' NCA table.
#' 
#' @param title table title, optional character value
#' @param subtitle table subtitle, optional character value
#' @param nca_options NCA options, see ?NCAOptions
#' @param tab_options list of options to pass to gt::tab_options
#' @param json path to JSON table file or JSON content in string form
#' @export
NCATable <- function(title=NULL, subtitle=NULL, nca_options=NCAOptions(), tab_options=list(), json=NULL) {
  if (is.null(json)) {
    if (is.null(title)) {
      title = NA_character_
    }
    if (is.null(subtitle)) {
      subtitle = NA_character_
    }
    table <- new("nca_metrics_table", nca_options=nca_options, tab_options=tab_options,
                 title=title, subtitle=subtitle)
  } else {
    if (is.list(json)) {
      json <- JSONElement(json)
    }
    table <- loadFromJSON(object=new("nca_metrics_table"), json=json)
  }
  return(table)
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("nca_metrics_table", "nca_analysis"), definition = function(object, x) {
  object@nca_analyses <- object@nca_analyses %>% add(x)
  return(object)
})

setMethod("add", signature = c("nca_metrics_table", "list"), definition = function(object, x) {
  object@nca_analyses <- object@nca_analyses %>% add(x)
  return(object)
})

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("nca_metrics_table", "data.frame", "nca_options"), definition=function(object, x, options, ...) {
  if (is(options, "undefined_nca_options")) {
    options_ <- object@nca_options # Use embedded NCA options
  } else {
    options_ <- options # Use external NCA options
  }
  
  object@nca_analyses <- object@nca_analyses %>%
    calculate(x=x, options=options_, ...)
  return(object)  
})

#_______________________________________________________________________________
#----                                export                                 ----
#_______________________________________________________________________________

setMethod("export", signature=c("nca_metrics_table", "character"), definition=function(object, dest, ...) {
  if (object@nca_analyses %>% length() == 0) {
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
  
  analysis_strat <- length(object@nca_analyses) > 1
  retValue <- object@nca_analyses@list %>% purrr::map_df(.f=~.x %>% export(dest=dest, type=type, analysis_strat=analysis_strat, ...))

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

    by <- c("id", object %>% getStrata(keep_single=FALSE))
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
          definition=function(object, dest, init=NULL, subscripts=NULL, all_dichotomous_levels=NULL, combine_with=NULL, header_label=NULL, ...) {
  code <- object %>% generateTableCode(init=init, subscripts=subscripts, all_dichotomous_levels=all_dichotomous_levels, combine_with=combine_with, header_label=header_label, ...)
  table <- object # Table variable needs to be there!
  #cat(code)
  # browser()
  retValue <- tryCatch(
    expr=eval(expr=parse(text=code)),
    error=function(cond) {
      print(cond)
      return(sprintf("Failed to create gtsummary table: %s", cond$message))
    })
  return(retValue)
})

#' @inheritParams generateTableCode
setMethod("export", signature=c("nca_metrics_table", "gt_type"),
          definition=function(object, dest, init=NULL, subscripts=NULL, all_dichotomous_levels=NULL, combine_with=NULL, header_label=NULL, ...) {
  gtsummaryTable <- object %>%
    export(dest=new("gtsummary_type"), init=init, subscripts=subscripts, all_dichotomous_levels=all_dichotomous_levels, combine_with=combine_with, header_label=header_label, ...)
  
  gtTable <- gtsummaryTable %>%
    toGt(subscripts=subscripts, title=object@title, subtitle=object@subtitle, opts=object@tab_options, ...)

  return(gtTable)
})

#' 
#' Gtsummary to Gt.
#' 
#' @param x gtsummary table
#' @param title table title
#' @param subtitle table subtitle
#' @param subscripts use subscripts
#' @param fmt_markdown transform any markdown-formatted text, logical value. Default is FALSE.
#' @param opts gt tab options
#' @importFrom gtsummary as_gt
#' @importFrom gt cells_body fmt_markdown tab_options text_transform
#' @importFrom stringr str_replace_all
#' @export
toGt <- function(x, title=NULL, subtitle=NULL, opts=list(), subscripts=FALSE, fmt_markdown=FALSE) {
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
  
  if (fmt_markdown) {
    gtTable <- gtTable %>%
      gt::fmt_markdown()
  }
  
  if (length(opts) > 0) {
    print(opts)
    gtTable <- do.call(
      gt::tab_options,
      c(list(data = gtTable), opts)
    )
  }
  if (!is.null(title) && !is.na(title) && title != "") {
    gtTable <- gtTable %>%
      gt::tab_header(title=title, subtitle=subtitle)
  }

  return(gtTable)
}

#_______________________________________________________________________________
#----                       generateTableCode                               ----
#_______________________________________________________________________________

#' @rdname generateTableCode
setMethod("generateTableCode", signature=c("nca_metrics_table", "logical", "logical", "logical", "character", "character"),
          definition=function(object, init, subscripts, all_dichotomous_levels, combine_with, header_label, ...) {
  
  if (init) {
    initCode <- "individual <- table" %>%
      addPipeLayer("export(dest=\"dataframe\", type=\"individual_wide\")") %>%
      addPipeLayer("dplyr::select(-id)")
  } else {
    initCode <- NULL
  }
 
  stratVariables <- object %>% getStrata(keep_single=FALSE)
  
  stats <- getStatisticsCode(object)
  type <- getVariableTypeCode(object, all_dichotomous_levels=all_dichotomous_levels)
  labels <- getLabelsCode(object, subscripts=subscripts)
  digits <- getDigitsCode(object)
  
  if (length(stratVariables) <= 2) {
    body <- getTableSummaryCode(var="gttable", data="individual", by=stratVariables,
                                stats=stats, type=type, labels=labels, digits=digits,
                                combine_with=combine_with, header_label=header_label)
  } else {
    stop("Too many stratification variables")
  }

  return(paste0(c(initCode, body, "gttable"), collapse="\n"))
})

#_______________________________________________________________________________
#----                             getStrata                                 ----
#_______________________________________________________________________________

#' @rdname getStrata
setMethod("getStrata", signature=c("nca_metrics_table", "logical"), definition=function(object, keep_single, ...) {
  retValue <- NULL
  
  if (length(object@nca_analyses) > 1) {
    retValue <- "analysis"
  }
  
  strat_vars <- object@nca_analyses@list %>%
    purrr::map(~.x@effective_strat_vars) %>%
    purrr::flatten_chr()
  
  return(c(retValue, unique(strat_vars)))
})

#_______________________________________________________________________________
#----                              getUnit                                  ----
#_______________________________________________________________________________

#' @rdname getUnit
setMethod("getUnit", signature=c("nca_metrics_table", "character"), definition=function(object, metric, ...) {
  if (object@nca_analyses %>% length()==0) {
    stop("No metrics in table at this stage")
  }
  return(object@nca_analyses@list[[1]] %>% getUnit(metric=metric, ...))
})

#_______________________________________________________________________________
#----                           loadFromJSON                                ----
#_______________________________________________________________________________

setMethod("loadFromJSON", signature=c("nca_metrics_table", "json_element"), definition=function(object, json) {
  json <- json@data
  object@nca_analyses@list <- json$nca_analyses %>%
    purrr::map(~loadFromJSON(NCAAnalysis(), JSONElement(.x)))
  
  # Extract possible tab options
  if (!is.null(json$tab_options)) {
    object@tab_options <- json$tab_options
  }
  
  # Extract possible NCA options
  if (!is.null(json$nca_options)) {
    object@nca_options <- loadFromJSON(NCAOptions(), JSONElement(json$nca_options))
  }
  
  # Extract title and subtitle
  if (!is.null(json$title)) {
    object@title <- json$title
  }
  if (!is.null(json$subtitle)) {
    object@subtitle <- json$subtitle
  }
  
  return(object)
})

setMethod("loadFromJSON", signature=c("nca_metrics_table", "character"), definition=function(object, json) {
  schema <- system.file("extdata", "campsisnca.schema.json", package="campsisnca")
  return(loadFromJSON(object=object, json=openJSON(json=json, schema=schema)))
})

