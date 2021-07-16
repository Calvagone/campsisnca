
checkNATimes <- function(x, time_var="time") {
  naTimes <- x %>% dplyr::filter_at(.vars=time_var, .vars_predicate=~is.na(.x))
  assertthat::assert_that(nrow(naTimes)==0, msg="Sample times cannot be NA")
}

checkNAObservations <- function(x, variable) {
  naObs <- x %>% dplyr::filter_at(.vars=variable, .vars_predicate=~is.na(.x))
  assertthat::assert_that(nrow(naObs)==0, msg=paste0("Observations at times '", paste0(unique(naObs$time), collapse=",") , "' are NA"))
}

checkTimesAreIncreasing <- function(x) {
  tmp <- x %>% dplyr::group_by(id) %>% dplyr::summarise(INC=all(time==cummax(time)))
  assertthat::assert_that(all(tmp$INC), msg="Times must be monotonically increasing")
}