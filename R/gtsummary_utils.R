
#' 
#' Extract values from within braces.
#' 
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