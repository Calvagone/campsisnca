
#' 
#' Discard category column. 
#' 
#' @param x summary export
#' @param split string use to concatenate the 'stat' and 'category' column
#' @return updated data frame
#' @importFrom dplyr all_of mutate select
#' @export
discard_category_column <- function(x, split="_") {
  if (!"category" %in% colnames(x)) {
    return(x)
  }
  retValue <- x %>%
    dplyr::mutate(stat=ifelse(is.na(category), stat, paste0(stat, split, category))) %>%
    dplyr::select(-dplyr::all_of("category"))
  return(retValue)
}
