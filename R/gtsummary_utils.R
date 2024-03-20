
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
#' @param idata individual values for a given metric
#' @param stat_display statistics display for gtsummary
#' @return data frame
#' @importFrom gtsummary tbl_summary
#' @importFrom dplyr select
computeTableSummary <- function(idata, stat_display) {
  gtTable <- idata %>%
    dplyr::select(-id) %>%
    gtsummary::tbl_summary(
      by = NULL,
      statistic = list(
        value ~ stat_display
      ),
      type = list(
        value ~ "continuous"
      )
    )

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
#' @param labels the labels to display
#' @return data frame
getTableSummaryCode <- function(variable, data, by, stats, labels) {
  if (length(by) %in% c(0,1)) {
  retValue <- sprintf(
"%s <- %s  %%>%% 
tbl_summary(
  by=%s,
  statistic=list(
    %s
  ),
  type=list(
    all_continuous() ~ \"continuous\",
    all_categorical() ~ \"continuous\"
  ),
  label=list(
    %s
  )
) %%>%%
modify_header(label=\"**Metric**\")
", variable, data, by, stats, labels)
  
  } else if (length(by)==2) {
    retValue <- sprintf(
"%s <- tbl_strata(data=%s,
strata=%s,
~.x %%>%% tbl_summary(
  by=%s,
  statistic=list(
    %s
  ),
  type=list(
    all_continuous() ~ \"continuous\",
    all_categorical() ~ \"continuous\"
  ),
  label=list(
    %s
  )
),
.combine_with=\"tbl_stack\") %%>%%
modify_header(label=\"**Metric**\")
", variable, data, by[1], by[2], stats, labels)    

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
  
  retValue <- metrics@list %>% purrr::map_chr(~sprintf("%s ~ \"%s\"", .x %>% getName(), .x@stat_display))
  
  return(paste0(retValue, collapse=",\n"))
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
      retValue <- sprintf("%s ~ \"%s\"", x %>% getName(), resultingName)
    } else {
      retValue <- sprintf("%s ~ \"%s (%s)\"", x %>% getName(), resultingName, unit)
    }
  })

  return(paste0(retValue, collapse=",\n"))
}
