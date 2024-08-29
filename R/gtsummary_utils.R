
#' 
#' Extract values from within braces.
#' 
#' @param x input string
#' @return a character vector
#' @importFrom stringr str_extract_all
#' @export
extractBraceValues <- function(x) {
  return(stringr::str_extract_all(x, '(?<=\\{)[^\\}]+')[[1]])
}

#' 
#' Compute NCA metric summary.
#' 
#' @param object NCA metric
#' @param quantile_type type of quantile
#' @return data frame
#' @importFrom dplyr all_of arrange desc filter transmute
#' @importFrom cards ard_categorical ard_continuous
#' @importFrom tibble as_tibble
#' @importFrom purrr map_chr
#' @importFrom stringr str_detect
#' @export
computeNCAMetricSummary <- function(object, quantile_type) {
  
  data <- object@individual
  stat_display <- object@stat_display
  digits <- object@digits
  stats <- extractBraceValues(stat_display) %>%
    trimws()
  categorical <- object@categorical
  
  if (categorical) {
    summary <-
      cards::ard_categorical(
        data,
        by=NULL,
        variables=dplyr::all_of("value"),
        statistic=~c("n", "p", "N")
      )
  
    summary <- tibble::as_tibble(summary) %>%
      dplyr::transmute(stat=stat_name, value=as.numeric(summary$stat), category=as.character(variable_level))
    
    categories <- unique(summary$category)
    tmp <- categories %>%
      purrr::map_chr(~glueStatDisplay(stat_display=stat_display, stats=stats, summary=summary %>% dplyr::filter(category==.x), digits=digits))
    
    comment <- paste0(paste0(categories, ": ", tmp), collapse=", ")
    
  } else {
    availableContinuousStats <- list(
      "N"=function(x) length(x),
      "mean"=mean,
      "sd"=sd,
      "median"=median,
      "min"=min,
      "max"=max,
      "geomean"=geomean,
      "geocv"=geocv,
      "cv"=cv,
      "se"=se
    )
    
    # Detect stats that are percentiles
    percentileStats <- stats[stringr::str_detect(stats, "^p[0-9\\.]+$")]
    
    # Add requested percentile functions
    for (percentileStat in percentileStats) {
      availableContinuousStats[[percentileStat]] <- quantileFun(str=percentileStat, type=quantile_type)
    }
    
    # In case some stats are not available
    stats_ <- stats[stats %in% names(availableContinuousStats)]
    
    summary <-
      cards::ard_continuous(
        data,
        by=NULL,
        variables=dplyr::all_of("value"),
        statistic=~cards::continuous_summary_fns(
          summaries=character(0),
          other_stats=availableContinuousStats[stats_]
        )
      )
    
    summary <- tibble::as_tibble(summary) %>%
      dplyr::transmute(stat=stat_name, value=as.numeric(summary$stat))

    comment <- glueStatDisplay(stat_display=stat_display, stats=stats_, summary=summary, digits=digits)
  }

  # Add evaluated stat_display string as a comment to the data frame
  comment(summary) <- comment

  return(summary)
}

quantileFun <- function(str, type) {
  myFun <- sprintf("function(x) {stats::quantile(x, probs=as.numeric(substr('%s', 2, nchar('%s')))/100, type=%i) %%>%% unname()}", str, str, type)
  return(eval(parse(text=myFun)))
}

#' 
#' Glue stat display string.
#' 
#' @param stat_display stat display string
#' @param stats statistics, character vector
#' @param summary summary data frame
#' @param digits digits to be used for rounding
#' @return glued string
#' @importFrom glue glue
#' @export
glueStatDisplay <- function(stat_display, stats, summary, digits) {
  env <- new.env()
  values <- summary$value
  names(values) <- summary$stat
  
  for (statIndex in seq_along(stats)) {
    stat <- stats[statIndex]
    value <- values[stat]
    percentage <- stat=="p"
    
    if (!stat %in% c("n", "N")) {
      if (length(digits) == 0) {
        # Default rounding
        if (percentage) {
          value <- gtsummary::style_percent(value, digits=1, symbol=FALSE)
        } else {
          value <- gtsummary::style_sigfig(value, digits=3)
        }
      } else {
        if (statIndex <= length(digits)) {
          digit <- digits[statIndex]
        } else {
          digit <- digits[1]
        }
        fun <- eval(expr=parse(text=digit))
        if (is.numeric(fun)) {
          if (percentage) {
            value <- round(value*100, fun)
          } else {
            value <- round(value, fun)
          }
        } else if (rlang::is_function(fun)) {
          value <- fun(value)
        } else {
          stop("Digit element must be a function, a purrr-style lambda function or simply an integer")
        }
      }
    }
    env[[stat]] <- value
  }
  retValue <- glue::glue(stat_display, .envir=env) %>% as.character()
  
  return(retValue)
}

#' 
#' Get table summary code.
#' 
#' @param variable assigned variable name
#' @param data data frame code
#' @param by variable
#' @param stats stats to compute
#' @param type type of the variables
#' @param labels the labels to display
#' @param digits the digits to be used for rounding
#' @param combine_with either 'tbl_stack' or 'tbl_merge'
#' @param header_label header label name
#' @importFrom gtsummary tbl_summary modify_header
#' @return data frame
getTableSummaryCode <- function(variable, data, by, stats, type, labels, digits, combine_with, header_label) {
  
  assertthat::assert_that(combine_with %in% c("tbl_stack", "tbl_merge"),
                          msg="combine_with must be 'tbl_stack' or 'tbl_merge'")
  
  # If no stratification, use the NULL string
  if (length(by)==0) {
    by <- "NULL"
  }
  
  if (length(by)==1) {
  retValue <- sprintf(
"%s <- %s%s  %%>%%
tbl_summary(
  by=%s,
  statistic=list(
    %s
  ),
  type=list(
    %s
  ),
  label=list(
    %s
  ),
  digits=list(
    %s
  )
) %%>%%
modify_header(label=\"**%s**\")
", variable, data, factorUsingNaturalOrder(by), by, stats, type, labels, digits, header_label)
  
  } else if (length(by)==2) {
    retValue <- sprintf(
"%s <- tbl_strata(data=%s%s,
strata=%s,
~.x %%>%% tbl_summary(
  by=%s,
  statistic=list(
    %s
  ),
  type=list(
    %s
  ),
  label=list(
    %s
  ),
  digits=list(
    %s
  )
),
.combine_with=\"%s\") %%>%%
modify_header(label=\"**%s**\")
", variable, data, factorUsingNaturalOrder(by), by[1], by[2], stats, type, labels, digits, combine_with, header_label)    

}
  return(retValue)
}

#' 
#' Get statistics code for gtsummary.
#' 
#' @param table NCA table
#' @return code
getStatisticsCode <- function(table) {
  # Always look at first NCA metric only
  metrics <- table@list[[1]]
  
  retValue <- metrics@list %>% purrr::map_chr(~sprintf("%s ~ \"%s\"", addBackticks(.x %>% getName()), .x@stat_display))
  
  return(paste0(retValue, collapse=",\n    "))
}

#' 
#' Get the variable type code for gtsummary.
#' 
#' @param table NCA table
#' @param all_dichotomous_levels show all dichotomous levels (0 and 1) when data is dichotomous
#' @importFrom gtsummary all_dichotomous
#' @return code
getVariableTypeCode <- function(table, all_dichotomous_levels) {
  # Always look at first NCA metric only
  metrics <- table@list[[1]]
  
  retValue <- metrics@list %>% purrr::map_chr(.f=function(x) {
    categorical <- x@categorical
    if (categorical) {
      typeStr <-  "categorical"
    } else {
      typeStr <-  "continuous"
    }
    type <- sprintf("%s ~ \"%s\"", addBackticks(x %>% getName()), typeStr)
    return(type)
  })
  
  if (!all_dichotomous_levels) {
    retValue <- c(retValue, "all_dichotomous() ~ \"dichotomous\"")
  }
  
  return(paste0(retValue, collapse=",\n    "))
}

#' 
#' Get labels code for gtsummary.
#' 
#' @param table NCA table
#' @param subscripts use subscripts, logical value
#' @return code
getLabelsCode <- function(table, subscripts) {
  # Always look at first NCA metric only
  metrics <- table@list[[1]]
  
  retValue <- metrics@list %>% purrr::map_chr(.f=function(x) {
    unit <- x@unit
    if (subscripts) {
      resultingName <-  x %>% getLaTeXName()
    } else {
      resultingName <-  x %>% getName()
    }
    if (is.na(unit)) {
      label <- sprintf("%s ~ \"%s\"", addBackticks(x %>% getName()), resultingName)
    } else {
      label <- sprintf("%s ~ \"%s (%s)\"", addBackticks(x %>% getName()), resultingName, unit)
    }
    return(label)
  })

  return(paste0(retValue, collapse=",\n    "))
}

#' 
#' Get digits code for gtsummary.
#' 
#' @param table NCA table
#' @return code
getDigitsCode <- function(table) {
  # Always look at first NCA metric only
  metrics <- table@list[[1]]
  
  retValue <- metrics@list %>% purrr::map_chr(.f=function(x) {
    digits <- x@digits
    if (length(digits) > 0) {
      digit <- sprintf("%s ~ list(%s)", addBackticks(x %>% getName()), paste0(digits, collapse=","))
    } else {
      digit <- ""
    }
    return(digit)
  })
  
  retValue <- retValue[retValue!=""]
  
  return(paste0(retValue, collapse=",\n    "))
}

addBackticks <- function(x) {
  return(paste0("`", x, "`")) 
}

factorUsingNaturalOrder <- function(by) {
  if (length(by)==1 && by != "NULL") {
    return(sprintf(" %%>%% mutate(%s=factor(%s, levels=unique(%s)))", by, by, by))
  } else if (length(by)==2) {
    return(sprintf(" %%>%% mutate(%s=factor(%s, levels=unique(%s)), %s=factor(%s, levels=unique(%s)))", by[1], by[1], by[1], by[2], by[2], by[2]))
  }
  return("")
}

addPipeLayer <- function(x, layer) {
  assertthat::assert_that(x %>% length() != 0, msg="x cannot be length 0")
  x[x %>% length()] <- paste0(x[x %>% length()], " %>%")
  return(x %>% append(paste0(getDefaultIndent(), layer)))
}

getDefaultIndent <- function() {
  return("\t")
}

