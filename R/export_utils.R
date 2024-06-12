#_______________________________________________________________________________
#----                       fillMissingCategories                           ----
#_______________________________________________________________________________

#' 
#' Fill missing categories when summary statistics are computed by campsisnca across.
#' 
#' @param x summary export across replicates
#' @param replace_na replace NA's by 0
#' @param show_all_dichotomous_levels show all dichotomous levels (TRUE/FALSE) or only keep TRUE's if FALSE
#' @param preserve_category_column preserve category column
#' @importFrom dplyr across all_of distinct filter group_by left_join mutate group_split reframe select transmute pull
#' @importFrom purrr map_df
#' @export
fillMissingCategories <- function(x, replace_na=TRUE, show_all_dichotomous_levels=TRUE, preserve_category_column=FALSE) {
  colnames <- colnames(x)
  if (!"category" %in% colnames) {
    return(x)
  }
  
  idCols <- c("replicate", "metric", "stat", "value", "category")
  catCols <- colnames[!colnames %in% idCols]
  catStats <- c("p", "n", "N")
  
  categoricalData <- x %>%
    dplyr::filter(stat %in% catStats) %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::transmute(replicate, metric, stat, category)
  
  # Search for all categories across replicates
  completeCategories <- categoricalData %>%
    dplyr::group_by(metric, stat) %>%
    dplyr::reframe(category=unique(category))
  
  # Convert to factors because of group_split
  x <- x %>% 
    dplyr::mutate(dplyr::across(c("metric", catCols), function(x) factor(x, levels=unique(x))))
  
  retValue <- x %>%
    dplyr::group_split(dplyr::across(c("replicate", "metric", "stat"))) %>%
    purrr::map_df(.f=function(data) {
      if (unique(data$stat) %in% catStats) {
        replicate <- unique(data$replicate)
        metric <- unique(data$metric)
        stat <- unique(data$stat)
        
        catValues <- completeCategories %>%
          dplyr::filter(.data$metric==metric) %>%
          dplyr::pull(category)
        
        allButValue <- data %>%
          dplyr::select(dplyr::all_of(c("replicate", "metric", "stat", catCols))) %>%
          dplyr::distinct() %>%
          dplyr::group_by(dplyr::across(c("replicate", "metric", "stat", catCols))) %>%
          dplyr::reframe(category=catValues)
        
        data <- allButValue %>%
          dplyr::left_join(data, by=c("replicate", "metric", "stat", "category", catCols))
        return(data)
      } else {
        return(data)
      }
      return(data)
    }) %>%
    dplyr::relocate(dplyr::all_of(idCols))
  
  # Same ordering as original data
  retValue <- retValue %>%
    dplyr::arrange(dplyr::across(c("replicate", catCols, "metric", "stat")))
  
  # Back to characters
  retValue <- retValue %>% 
    dplyr::mutate(dplyr::across(c("metric", catCols), function(x) as.character(x)))
  
  # Don't show all dichotomous levels
  if (!show_all_dichotomous_levels) {
    retValue <- retValue %>%
      filter(is.na(category) | category != "FALSE")
  }
  
  # Replace NA value by zero
  if (replace_na) {
    retValue <- retValue %>% 
      dplyr::mutate(value=ifelse(is.na(value) & !is.na(category), 0, value))
  }
  
  # Remove category column
  if (!preserve_category_column) {
    retValue <- retValue %>%
      dplyr::mutate(stat=ifelse(is.na(category), stat, paste0(stat, "_", category))) %>%
      dplyr::select(-dplyr::all_of("category"))
  }
  
  return(retValue)
}
