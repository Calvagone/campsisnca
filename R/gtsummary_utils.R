
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
#' Extract gtsummary table data.
#' 
#' @param tbl gtsummary table
#' @return data frame
#' @importFrom tidyr pivot_longer
#' @importFrom assertthat assert_that
#' @importFrom dplyr all_of bind_rows select 
extractTableInfo <- function(tbl) {
  # by <- tbl$by
  meta_data <- tbl$meta_data
  #meta_data <-  tbl
  ret <- NULL
  
  for (index in seq_len(nrow(meta_data))) {
    row <- meta_data[index, ]
    variable <- row$variable
    df_stats <- row$df_stats
    stat_display <- row$stat_display
    
    assertthat::assert_that(length(df_stats)==1)
    assertthat::assert_that(length(stat_display)==1)
    
    df_stats <- df_stats[[1]]
    stat_display <- stat_display[[1]]
    stats <- extractBraceValues(stat_display)
    
    tmp <- df_stats %>%
      dplyr::select(dplyr::all_of(c("variable", stats))) %>%
      # rename(!!by:=by) %>%
      tidyr::pivot_longer(cols=dplyr::all_of(stats), names_to="stat")

    ret <- dplyr::bind_rows(ret, tmp)
  }
  
  return(ret)
}

#' 
#' Compute table summary.
#' 
#' @param object NCA metric
#' @return data frame
#' @importFrom dplyr select
computeTableSummary <- function(object) {
  # Mock a table object with the given metric
  object@name <- "value"
  table <- NCAMetricsTable() %>%
    add(NCAMetrics() %>% add(object))
  
  # Re-use 'standard' table generation code
  stats <- getStatisticsCode(table)
  type <- getVariableTypeCode(table, all_dichotomous_levels=TRUE) # We expect all levels to be computed
  labels <- getLabelsCode(table, subscripts=TRUE)
  digits <- getDigitsCode(table)

  individual <- object@individual %>%
	  dplyr::select(-id)
  code <- getTableSummaryCode(var="gtTable", data="individual", by="NULL",
                              stats=stats, type=type, labels=labels, digits=digits,
                              combine_with="tbl_stack", header_label="Metric")
  gtTable <- tryCatch(
    expr=eval(expr=parse(text=code)),
    error=function(cond) {
      return(sprintf("Failed to create gtsummary table: %s", cond$message))
    })
  
  # Extract main info (-> stat_display)
  summary <- extractTableInfo(gtTable) %>%
    dplyr::select(-variable)
  
  # Add evaluated stat_display string as a comment to the data frame
  comment(summary) <- gtTable$table_body$stat_0
  
  return(summary)
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

