
check_na_times <- function(x, time_var="TIME") {
  naTimes <- x %>% dplyr::filter_at(.vars=time_var, .vars_predicate=~is.na(.x))
  assertthat::assert_that(nrow(naTimes)==0, msg="Sample times cannot be NA")
}

check_na_observations <- function(x, variable) {
  # Only make sense if variable is provided, i.e. not NA (like with the CustomMetricTbl)
  if (!is.na(variable)) {
    naObs <- x %>% dplyr::filter_at(.vars=variable, .vars_predicate=~is.na(.x))
    assertthat::assert_that(nrow(naObs)==0, msg=paste0("Observations at times '", paste0(unique(naObs$TIME), collapse=",") , "' are NA"))
  }
}

check_times_are_increasing <- function(x, strat_vars) {
  tmp <- x %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(c(strat_vars, "ID")))) %>%
    dplyr::summarise(INC=all(TIME==cummax(TIME)))
  assertthat::assert_that(all(tmp$INC), msg="Times must be monotonically increasing")
}