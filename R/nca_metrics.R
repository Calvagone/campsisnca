
#' 
#' Standardise CAMPSIS dataframe for NCA analysis.
#' Additional checks will also be performed.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
standardise <- function(x, variable) {
  x <- x %>% dplyr::rename_at(variable, ~"dv_variable")
  return(x)
}

#' 
#' Compute AUC.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
auc <- function(x, variable, method=1) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::summarise(auc=trap(x=time, y=dv_variable, method=method))
  return(x)
}

#' 
#' Compute Cmax.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
cmax <- function(x, variable) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.max(dv_variable))
  return(x %>% dplyr::transmute(id=id, cmax=dv_variable))
}

#' 
#' Compute tmax.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
tmax <- function(x, variable) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.max(dv_variable))
  return(x %>% dplyr::transmute(id=id, tmax=time))
}

#' 
#' Compute Cmin.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
cmin <- function(x, variable, after) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::filter(time >= after)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.min(dv_variable))
  return(x %>% dplyr::transmute(id=id, cmin=dv_variable))
}

#' 
#' Compute tmin.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
tmin <- function(x, variable, after) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::filter(time >= after) 
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.min(dv_variable))
  return(x %>% dplyr::transmute(id=id, tmin=time))
}

#' 
#' Compute Ctrough
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
ctrough <- function(x, variable, t) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::filter(time==t)
  return(x %>% dplyr::transmute(id=id, ctrough=dv_variable))
}