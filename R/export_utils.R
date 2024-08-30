
#' 
#' Discard category column. 
#' 
#' @param x summary export
#' @param split string use to concatenate the 'stat' and 'category' column
#' @importFrom dplyr all_of mutate select
#' @export
discardCategoryColumn <- function(x, split="_") {
  if (!"category" %in% colnames(x)) {
    return(x)
  }
  retValue <- x %>%
    dplyr::mutate(stat=ifelse(is.na(category), stat, paste0(stat, split, category))) %>%
    dplyr::select(-dplyr::all_of("category"))
  return(retValue)
}
