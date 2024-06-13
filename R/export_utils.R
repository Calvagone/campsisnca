
#' 
#' Fill missing categorical levels when summary statistics are derived across replicates.
#' 
#' @param x summary export across replicates
#' @param replace_na replace NA's by 0
#' @importFrom dplyr across all_of distinct filter group_by left_join mutate group_split reframe select transmute pull
#' @importFrom purrr map_df
#' @export
fillMissingCategoricalLevels <- function(x, replace_na=TRUE) {
  colnames <- colnames(x)
  if (!"category" %in% colnames) {
    return(x)
  }

  idCols <- c("replicate", "metric", "stat", "value", "category")
  catCols <- colnames[!colnames %in% idCols]
  catStats <- c("p", "n", "N")
  
  # Select categorical data (all but value)
  categoricalData <- x %>%
    dplyr::filter(stat %in% catStats) %>%
    dplyr::filter(!is.na(category)) %>%
    dplyr::transmute(replicate, metric, stat, category)
  
  # Search for all categories across replicates
  completeCategories <- categoricalData %>%
    dplyr::group_by(dplyr::across(c("metric", "stat"))) %>%
    dplyr::reframe(category=unique(category))
  
  # Convert to factors because of group_split
  x <- x %>% 
    dplyr::mutate(dplyr::across(c("metric", catCols), function(x) factor(x, levels=unique(x))))
  
  retValue <- x %>%
    dplyr::group_split(dplyr::across(c("replicate", "metric", "stat"))) %>%
    purrr::map_df(.f=function(data) {
      if (unique(data$stat) %in% catStats) {
        currentMetric <- unique(data$metric)
        currentstat <- unique(data$stat)
        
        catValues <- completeCategories %>%
          dplyr::filter(.data$metric==currentMetric & .data$stat==currentstat) %>%
          dplyr::pull(category)
        print(catValues)
        
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
  
  # Replace NA value by zero
  if (replace_na) {
    retValue <- retValue %>% 
      dplyr::mutate(value=ifelse(is.na(value) & !is.na(category), 0, value))
  }

  return(retValue)
}

#' 
#' Discard category column. 
#' 
#' @param x summary export
#' @param split string use to concatenate the 'stat' and 'category' column
#' @importFrom dplyr all_of mutate select
#' @export
discardCategoryColumn <- function(x, split="_") {
  retValue <- x %>%
    dplyr::mutate(stat=ifelse(is.na(category), stat, paste0(stat, split, category))) %>%
    dplyr::select(-dplyr::all_of("category"))
  return(retValue)
}
