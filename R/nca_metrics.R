
#' 
#' Standardise input dataframe to CAMPSIS dataframe.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @return dataframe with id and time columns, filtered on the observations
toCAMPSISDataframe <- function(x) {
  assertthat::assert_that(is.data.frame(x), msg="x is not a data frame")
  campsis <- isCAMPSIS(x)
  if (!campsis) {
    x <- x %>% dplyr::rename(id=ID, time=TIME)
    x <- x %>% dplyr::filter(MDV==0) # Keep observations only
  }
  return(x)
}

#' 
#' Standardise CAMPSIS/NONMEM dataframe for NCA analysis.
#' Additional checks will also be performed.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
standardise <- function(x, variable) {
  assertthat::assert_that(is.character(variable) && length(variable)==1, msg="variable must be a single character value")
  assertthat::assert_that(is.data.frame(x), msg="x is not a data frame")
  assertthat::assert_that(variable %in% colnames(x), msg=paste0("Variable '", variable, "' not found in data frame"))
  x <- x %>% dplyr::rename_at(variable, ~"dv_variable")
  
  # NONMEM -> CAMPSIS adaptations
  x <- x %>% toCAMPSISDataframe()
  
  # Check no time is NA
  checkNATimes(x, time_var="time") 
  
  # Check no observation is NA
  checkNAObservations(x, variable="dv_variable")
  
  # Check time is monotonically increasing
  checkTimesAreIncreasing(x)
  
  return(x)
}

#' 
#' Compute AUC.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @export
auc <- function(x, variable, method=1) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::summarise(auc=trap(x=time, y=dv_variable, method=method), .groups="drop")
  return(x)
}

#' 
#' Compute Cmax.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @export
cmax <- function(x, variable) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.max(dv_variable)) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, cmax=dv_variable))
}

#' 
#' Compute tmax.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @export
tmax <- function(x, variable) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.max(dv_variable)) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, tmax=time))
}

#' 
#' Compute Cmin.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @export
cmin <- function(x, variable) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.min(dv_variable)) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, cmin=dv_variable))
}

#' 
#' Compute tmin.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @export
tmin <- function(x, variable) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.min(dv_variable)) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, tmin=time))
}

#' 
#' Compute Ctrough
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @export
ctrough <- function(x, variable, t) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::filter(time==t) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, ctrough=dv_variable))
}

#' 
#' Compute Cavg (C average).
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @export
cavg <- function(x, variable) {
  auc <- auc(x=x, variable=variable)
  x <- x %>% standardise(variable)
  diff <- x %>% dplyr::group_by(id) %>% dplyr::summarise(diff_time=time[dplyr::n()]-time[1], .groups="drop")
  auc <- auc %>% dplyr::left_join(diff, by="id")
  cavg <- auc %>% dplyr::mutate(cavg=auc/diff_time) %>% dplyr::select(-auc, -diff_time) 
  return(cavg)
}