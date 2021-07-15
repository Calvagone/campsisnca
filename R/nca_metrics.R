
#' 
#' Compute AUC.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
auc <- function(x, variable, method=1) {
  x <- x %>% dplyr::rename_at(variable, ~"variable")
  x <- x %>% dplyr::group_by(id) %>% dplyr::summarise(auc=trap(x=time, y=variable, method=method))
  return(x)
}

#' 
#' Compute Cmax.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
cmax <- function(x, variable) {
  x <- x %>% dplyr::rename_at(variable, ~"variable")
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.max(variable))
  return(x %>% dplyr::transmute(id=id, cmax=variable))
}

#' 
#' Compute tmax.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
tmax <- function(x, variable) {
  x <- x %>% dplyr::rename_at(variable, ~"variable")
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.max(variable))
  return(x %>% dplyr::transmute(id=id, tmax=time))
}

#' 
#' Compute Cmin.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
cmin <- function(x, variable, after) {
  x <- x %>% dplyr::rename_at(variable, ~"variable") %>% dplyr::filter(time >= after)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.min(variable))
  return(x %>% dplyr::transmute(id=id, cmin=variable))
}

#' 
#' Compute tmin.
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
tmin <- function(x, variable, after) {
  x <- x %>% dplyr::rename_at(variable, ~"variable") %>% dplyr::filter(time >= after) 
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.min(variable))
  return(x %>% dplyr::transmute(id=id, tmin=time))
}

#' 
#' Compute Ctrough
#' 
#' @param x CAMPSIS dataframe
#' @param variable dependent variable
#' @export
ctrough <- function(x, variable, t) {
  x <- x %>% dplyr::rename_at(variable, ~"variable")
  x <- x %>% dplyr::group_by(id) %>% dplyr::filter(time==t)
  return(x %>% dplyr::transmute(id=id, ctrough=variable))
}