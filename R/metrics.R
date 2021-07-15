auc <- function(x, variable) {
  x <- x %>% dplyr::rename_at(variable, ~"variable")
  x <- x %>% dplyr::group_by(id) %>% dplyr::summarise(auc=sum(diff(time)*zoo::rollmean(variable, 2)))
  return(x)
}

cmax <- function(x, variable) {
  x <- x %>% dplyr::rename_at(variable, ~"variable")
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.max(variable))
  return(x %>% dplyr::transmute(id=id, cmax=variable))
}

tmax <- function(x, variable) {
  x <- x %>% dplyr::rename_at(variable, ~"variable")
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.max(variable))
  return(x %>% dplyr::transmute(id=id, tmax=time))
}

cmin <- function(x, variable, after) {
  x <- x %>% dplyr::rename_at(variable, ~"variable") %>% dplyr::filter(time >= after)
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.min(variable))
  return(x %>% dplyr::transmute(id=id, cmin=variable))
}

tmin <- function(x, variable, after) {
  x <- x %>% dplyr::rename_at(variable, ~"variable") %>% dplyr::filter(time >= after) 
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(which.min(variable))
  return(x %>% dplyr::transmute(id=id, tmin=time))
}

ctrough <- function(x, variable, t) {
  x <- x %>% dplyr::rename_at(variable, ~"variable")
  x <- x %>% dplyr::group_by(id) %>% dplyr::filter(time==t)
  return(x %>% dplyr::transmute(id=id, ctrough=variable))
}