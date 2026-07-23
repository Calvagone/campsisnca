#_______________________________________________________________________________
#----                       nca_table class                         ----
#_______________________________________________________________________________

#' 
#' NCA table class.
#' 
#' @export
setClass(
  "nca_table",
  representation(
    nca_analyses = "nca_analyses",  # NCA analyses
    title = "character",
    subtitle = "character",
    swap_strat = "logical",
    combine_with = "character",
    show_all_levels = "logical",
    header_label = "character",
    subscripts = "logical",
    nca_options = "nca_options",
    tab_options = "list"
  ),
  prototype = prototype(nca_analyses=new("nca_analyses"),
                        title=NA_character_,
                        subtitle=NA_character_,
                        swap_strat=FALSE,
                        combine_with="tbl_stack",
                        show_all_levels=FALSE,
                        header_label="Metric",
                        subscripts=TRUE,
                        nca_options=NCAOptions(),
                        tab_options=list())
)

#' 
#' NCA table (deprecated).
#' 
#' @param title table title, optional character value
#' @param subtitle table subtitle, optional character value
#' @param swap_strat swap stratification variables in table (only useful when 2 stratification variables are given)
#' @param combine_with either 'tbl_stack' or 'tbl_merge'
#' @param show_all_levels show all dichotomous levels in table
#' @param header_label 'Metric' by default
#' @param subscripts use LaTeX subcripts/superscripts notation when writing labels
#' @param nca_options NCA options, see ?NCAOptions
#' @param tab_options list of options to pass to gt::tab_options
#' @param json path to JSON table file or JSON content in string form
#' @export
NCAMetricsTable <- function(title=NULL, subtitle=NULL, swap_strat=FALSE, combine_with="tbl_stack", show_all_levels=FALSE,
                            header_label="Metric", subscripts=TRUE,
                            nca_options=NCAOptions(), tab_options=list(), json=NULL) {
  .Deprecated("NCATable")
  return(NCATable(title=title, subtitle=subtitle,
                  swap_strat=swap_strat, combine_with=combine_with, show_all_levels=show_all_levels,
                  header_label=header_label, subscripts=subscripts,
                  nca_options=nca_options, tab_options=tab_options, json=json))
}

#' 
#' NCA table.
#' 
#' @param title table title, optional character value
#' @param subtitle table subtitle, optional character value
#' @param swap_strat swap stratification variables in table (only useful when 2 stratification variables are given)
#' @param combine_with either 'tbl_stack' or 'tbl_merge'
#' @param show_all_levels show all dichotomous levels in table
#' @param header_label 'Metric' by default
#' @param subscripts use LaTeX subcripts/superscripts notation when writing labels
#' @param nca_options NCA options, see ?NCAOptions
#' @param tab_options list of options to pass to gt::tab_options
#' @param json path to JSON table file or JSON content in string form
#' @export
NCATable <- function(title=NULL, subtitle=NULL, swap_strat=FALSE, combine_with="tbl_stack", show_all_levels=FALSE,
                     header_label="Metric", subscripts=TRUE,
                     nca_options=NCAOptions(), tab_options=list(), json=NULL) {
  if (is.null(json)) {
    if (is.null(title)) {
      title = NA_character_
    }
    if (is.null(subtitle)) {
      subtitle = NA_character_
    }
    table <- new("nca_table", title=title, subtitle=subtitle,
                 swap_strat=swap_strat, combine_with=combine_with, show_all_levels=show_all_levels,
                 header_label=header_label, subscripts=subscripts,
                 nca_options=nca_options, tab_options=tab_options)
  } else {
    table <- load_from_json(object=new("nca_table"), json=json)
  }
  return(table)
}

#_______________________________________________________________________________
#----                           add                                   ----
#_______________________________________________________________________________

setMethod("add", signature = c("nca_table", "nca_analysis"), definition = function(object, x) {
  object@nca_analyses <- object@nca_analyses %>% add(x)
  return(object)
})

setMethod("add", signature = c("nca_table", "list"), definition = function(object, x) {
  object@nca_analyses <- object@nca_analyses %>% add(x)
  return(object)
})

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("nca_table", "campsis_output", "nca_options"), definition=function(object, x, options, ...) {
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

setMethod("export", signature=c("nca_table", "character"), definition=function(object, dest, ...) {
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
setMethod("export", signature=c("nca_table", "dataframe_type"), definition=function(object, dest, type="summary", ...) {
  
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

    by <- c("id", object %>% get_strata(keep_single=FALSE))
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

#' @inheritParams generate_table_code
setMethod("export", signature=c("nca_table", "gtsummary_type"),
          definition=function(object, dest, init=NULL, ...) {
  code <- object %>% generate_table_code(init=init, ...)
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

#' @inheritParams generate_table_code
setMethod("export", signature=c("nca_table", "gt_type"),
          definition=function(object, dest, init=NULL, ...) {
  gtsummaryTable <- object %>%
    export(dest=new("gtsummary_type"), init=init, ...)
  
  gtTable <- gtsummaryTable %>%
    toGt(subscripts=object@subscripts, title=object@title, subtitle=object@subtitle, opts=object@tab_options, ...)

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
    x$table_styling$footnote$footnote <- adapt_footnote(footnote$footnote)
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
#----                      generate_table_code                              ----
#_______________________________________________________________________________

#' @rdname generate_table_code
setMethod("generate_table_code", signature=c("nca_table", "logical"),
          definition=function(object, init, ...) {
  
  if (init) {
    initCode <- "individual <- table" %>%
      add_pipe_layer("export(dest=\"dataframe\", type=\"individual_wide\")") %>%
      add_pipe_layer("dplyr::select(-id)")
  } else {
    initCode <- NULL
  }
 
  stratVariables <- object %>% get_strata(keep_single=FALSE)
  if (object@swap_strat) {
    stratVariables <- rev(stratVariables)
  }
  stats <- get_statistics_code(object)
  type <- get_variable_type_code(object, all_dichotomous_levels=object@show_all_levels)
  labels <- get_labels_code(object, subscripts=object@subscripts)
  digits <- get_digits_code(object)
  
  if (length(stratVariables) <= 2) {
    body <- get_table_summary_code(var="gttable", data="individual", by=stratVariables,
                                stats=stats, type=type, labels=labels, digits=digits,
                                combine_with=object@combine_with, header_label=object@header_label)
  } else {
    stop("Too many stratification variables")
  }

  return(paste0(c(initCode, body, "gttable"), collapse="\n"))
})

#_______________________________________________________________________________
#----                             get_strata                                ----
#_______________________________________________________________________________

#' @rdname get_strata
setMethod("get_strata", signature=c("nca_table", "logical"), definition=function(object, keep_single, ...) {
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
#----                              get_unit                                 ----
#_______________________________________________________________________________

#' @rdname get_unit
setMethod("get_unit", signature=c("nca_table", "character"), definition=function(object, metric, ...) {
  if (object@nca_analyses %>% length()==0) {
    stop("No metrics in table at this stage")
  }
  return(object@nca_analyses@list[[1]] %>% get_unit(metric=metric, ...))
})

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature=c("nca_table", "json_element"), definition=function(object, json) {
  json <- json@data
  object@nca_analyses@list <- json$nca_analyses %>%
    purrr::map(~load_from_json(NCAAnalysis(), JSONElement(.x)))
  
  # Extract possible tab options
  if (!is.null(json$tab_options)) {
    object@tab_options <- json$tab_options
  }
  
  # Extract possible NCA options
  if (!is.null(json$nca_options)) {
    object@nca_options <- load_from_json(NCAOptions(), JSONElement(json$nca_options))
  }
  
  # Extract title and subtitle
  if (!is.null(json$title)) {
    object@title <- json$title
  }
  if (!is.null(json$subtitle)) {
    object@subtitle <- json$subtitle
  }
  
  # Extract extra fields
  if (!is.null(json$combine_with)) {
    object@combine_with <- json$combine_with
  }
  if (!is.null(json$show_all_levels)) {
    object@show_all_levels <- json$show_all_levels
  }
  if (!is.null(json$header_label)) {
    object@header_label <- json$header_label
  }
  if (!is.null(json$subscripts)) {
    object@subscripts <- json$subscripts
  }
  if (!is.null(json$swap_strat)) {
    object@swap_strat <- json$swap_strat
  }
  
  return(object)
})

setMethod("load_from_json", signature=c("nca_table", "character"), definition=function(object, json) {
  schema <- system.file("extdata", "campsisnca.schema.json", package="campsisnca")
  return(load_from_json(object=object, json=open_json(json=json, schema=schema)))
})

setMethod("load_from_json", signature=c("nca_table", "list"), definition=function(object, json) {
  schema <- system.file("extdata", "campsisnca.schema.json", package="campsisnca")
  return(load_from_json(object=object, json=open_json(json=json, schema=schema)))
})

#_______________________________________________________________________________
#----                        summarise_replicates                           ----
#_______________________________________________________________________________

#' Does the data contain more than one replicate?
#' 
#' @return \code{TRUE} if the data contains a \code{replicate} column with more than one distinct value, \code{FALSE} otherwise
#' @param x a data frame (typically \code{std_campsis_tbl})
#' @importFrom dplyr n_distinct
#' @keywords internal
.is_replicated <- function(x) {
  return("replicate" %in% colnames(x) && dplyr::n_distinct(x$replicate) > 1)
}

#' @param dest destination for the summarised table, either "dataframe", "gtsummary" or "gt"
#' @importFrom dplyr across filter if_any if_else matches mutate select transmute
#' @importFrom gtsummary all_categorical all_continuous all_stat_cols modify_footnote modify_header tbl_summary
#' @importFrom rlang as_function
#' @importFrom tibble add_column
#' @importFrom tidyr pivot_wider
#' @rdname summarise_replicates
setMethod(
  "summarise_replicates",
  signature = c("nca_table", "campsisnca_output", "nca_options"),
  definition = function(object, x, options, dest = "dataframe", ...) {
    if (is(options, "undefined_nca_options")) {
      options_ <- object@nca_options@replicated_nca_options # Use embedded NCA options
    } else {
      options_ <- options@replicated_nca_options # Use external NCA options
    }

    # Check data frame class
    if (!is(x, "summary_campsisnca_tbl")) {
      stop("x must be of class 'summary_campsisnca_tbl' for now")
    }

    # Check if the data contains replicates
    if (!.is_replicated(x)) {
      stop(
        "x must contain a 'replicate' column with more than one distinct value"
      )
    }

    # Add category if not existing
    if (!"category" %in% colnames(x)) {
      x$category <- NA_character_
    }

    # Detect stratification variables
    all_cols <- colnames(x)
    strata_vars <- all_cols[
      !all_cols %in% c("replicate", "metric", "stat", "value", "category")
    ]

    categories <- x %>%
      tibble::add_column(!!!c(category = NA)[!"category" %in% names(.)]) %>%
      dplyr::filter(!is.na(category)) %>%
      dplyr::transmute(metric, stat, category) %>%
      dplyr::distinct()

    # Filter NCA metric statistics
    if (length(options_@selected_statistics) > 0) {
      x_filtered <- x %>%
        dplyr::filter(stat %in% options_@selected_statistics)
    } else {
      x_filtered <- x
    }

    x_wider <- x_filtered %>%
      dplyr::filter(dplyr::if_any(
        dplyr::matches("category"),
        ~ is.na(.x) | .x != "FALSE"
      )) %>%
      discard_category_column() %>%
      tidyr::pivot_wider(
        names_from = c("metric", "stat"),
        values_from = c("value"),
        names_glue = "{metric} ({stat})"
      ) %>%
      dplyr::mutate(dplyr::across(
        dplyr::any_of(sprintf(
          "%s (%s_%s)",
          categories$metric,
          categories$stat,
          categories$category
        )),
        .fns = ~ dplyr::if_else(is.na(.x), 0, .x)
      )) %>%
      dplyr::select(-replicate)

    if (dest == "dataframe") {
      return(x_wider)
    }

    stat_type <- if (length(options_@summary_stat_display) > 1) {
      "continuous2"
    } else {
      "continuous"
    }

    if (dest %in% c("gt", "gtsummary")) {
      gtsummary_table <- gtsummary::tbl_summary(
        data = x_wider,
        by = strata_vars,
        statistic = list(
          gtsummary::all_continuous() ~ options_@summary_stat_display,
          gtsummary::all_categorical() ~ options_@summary_stat_display
        ),
        type = list(
          gtsummary::all_continuous() ~ stat_type,
          gtsummary::all_categorical() ~ stat_type
        ),
        label = list(),
        digits = list(
          gtsummary::all_continuous() ~ list(rlang::as_function(
            ~ gtsummary::style_sigfig(.x, options_@summary_stat_signif_digits)
          )),
          gtsummary::all_categorical() ~ list(rlang::as_function(
            ~ gtsummary::style_sigfig(.x, options_@summary_stat_signif_digits)
          ))
        )
      ) %>%
      gtsummary::modify_header(
        gtsummary::all_stat_cols() ~ "**{level}**",
        label = "**Metric**"
      )

      if (length(options_@summary_stat_display) == 1) {
        gtsummary_table <- gtsummary_table %>%
          gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~ sprintf("%s, N<sub>rep</sub> = {n}", translate_stat_string(options_@summary_stat_display))
          )
      } else {
        gtsummary_table <- gtsummary_table %>%
          gtsummary::modify_footnote(
            gtsummary::all_stat_cols() ~ "N<sub>rep</sub> = {n}"
          )
      }

      if (dest == "gtsummary") {
        return(gtsummary_table)
      }
      if (dest == "gt") {
        gt_table <- gtsummary_table %>% toGt(subscripts = TRUE)
        return(gt_table)
      }
    } else {
      stop("Unsupported destination for summarise_replicates")
    }
  }
)

translate_stat_string <- function(stat_string) {
  # Standard map for common base statistics
  stat_map <- c(
    "median" = "Median",
    "mean"   = "Mean",
    "sd"     = "SD",
    "se"     = "SE",
    "var"    = "Variance",
    "min"    = "Minimum",
    "max"    = "Maximum",
    "sum"    = "Sum",
    "n"      = "n",
    "N"      = "N"
  )
  
  # Replace base statistics (e.g., {median} -> Median)
  for (stat in names(stat_map)) {
    stat_string <- gsub(paste0("{", stat, "}"), stat_map[stat], stat_string, fixed = TRUE)
  }
  
  # Dynamic map for percentiles (e.g., {p5} -> 5th percentile)
  while(grepl("\\{p[0-9]+\\}", stat_string)) {
    p_match <- regmatches(stat_string, regexpr("\\{p([0-9]+)\\}", stat_string))
    p_num <- gsub("[^0-9]", "", p_match)
    
    # Determine ordinal suffix (st, nd, rd, th)
    suffix <- switch(
      ifelse(p_num %in% c("11", "12", "13"), "th", substr(p_num, nchar(p_num), nchar(p_num))),
      "1" = "st", "2" = "nd", "3" = "rd", "th"
    )
    
    # CRITICAL FIX: Add fixed = TRUE here so R treats "{p5}" as a literal string
    stat_string <- sub(p_match, paste0(p_num, suffix, " percentile"), stat_string, fixed = TRUE)
  }
  
  # Clean up duplicate trailing " percentile" strings if they appear in ranges
  stat_string <- gsub(" percentile([\U2013-])([0-9]+[a-z]{2} percentile)", "\\1\\2", stat_string)
  
  return(stat_string)
}
