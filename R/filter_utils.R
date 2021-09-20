
#' 
#' Filter CAMPSIS dataset based on min and max time.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param min min time
#' @param max max time
#' @param exclmin exclude min time when filtering
#' @param exclmax exclude max time when filtering
#' @param rebase rebase first time to origin, logical value, FALSE by default
#' @return dataset subset
#' @export
timerange <- function(x, min=0, max=Inf, exclmin=FALSE, exclmax=FALSE, rebase=FALSE) {
  time_var <- "TIME"
  id_var <- "ID"
  checkNATimes(x, time_var=time_var) 
  
  if (exclmin) {
    x <- x %>% dplyr::filter_at(.vars=time_var, .vars_predicate=~.x > min)
  } else {
    x <- x %>% dplyr::filter_at(.vars=time_var, .vars_predicate=~.x >= min)
  }
  if (max != Inf) {
    if (exclmax) {
      x <- x %>% dplyr::filter_at(.vars=time_var, .vars_predicate=~.x < max)
    } else {
      x <- x %>% dplyr::filter_at(.vars=time_var, .vars_predicate=~.x <= max)
    }
  }
  if (rebase) {
    x <- x %>% dplyr::group_by_at(id_var) %>% dplyr::mutate_at(.vars=time_var, .funs=~.x-.x[1])
  }
  return(x)
}
