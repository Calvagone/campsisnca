
#' 
#' Filter CAMPSIS dataset based on min and max time.
#' 
#' @param min min time
#' @param max max time
#' @param exclmin exclude min time when filtering
#' @param exclmax exclude max time when filtering
#' @return dataset subset
#' @export
timerange <- function(x, min=0, max=Inf, exclmin=FALSE, exclmax=FALSE) {
  campsis <- isCAMPSIS(x)
  time_var <- ifelse(campsis, "time", "TIME")
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
  return(x)
}

#' 
#' Say if the given data frame is a CAMPSIS output or a NONMEM dataset .
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @return TRUE for CAMPSIS output, FALSE for NONMEM dataset, error otherwise
isCAMPSIS <- function(x) {
  assertthat::assert_that(is.data.frame(x), msg="x is not a data frame")
  colnames <- colnames(x)
  campsisColnames <- c("id", "time")
  nonmemColnames <- c("ID", "TIME", "MDV")
  if (all(campsisColnames %in% colnames)) {
    return(TRUE)
  } else if (all(nonmemColnames %in% colnames)) {
    return(FALSE)
  } else {
    stop("x not recognised as CAMPSIS output nor NONMEM dataset")
  }
}
