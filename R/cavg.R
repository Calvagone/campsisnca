#_______________________________________________________________________________
#----                         cavg_metric class                             ----
#_______________________________________________________________________________

validateCavgMetric <- function(object) {
  return(TRUE)
}

#' 
#' Cavg metric class.
#' 
#' @export
setClass(
  "cavg_metric",
  representation(
  ),
  contains="nca_metric",
  validity=validateCavgMetric
)

#' 
#' Cavg.
#' 
#' @inheritParams metricsParams
#' @export
Cavg <- function(x=NULL, variable=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("cavg_metric", x=x, variable=variable))
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("cavg_metric"), definition=function(object, ...) {
  object@individual <- cavg_delegate(x=object@x, variable=object@variable)
  return(object)    
})

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature=c("cavg_metric"), definition = function(x) {
  return("cavg")
})

#_______________________________________________________________________________
#----                           implementation                              ----
#_______________________________________________________________________________

#' 
#' Compute Cavg (C average).
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
cavg_delegate <- function(x, variable) {
  auc <- auc_delegate(x=x, variable=variable)
  x <- x %>% standardise(variable)
  diff <- x %>% dplyr::group_by(id) %>% dplyr::summarise(diff_time=time[dplyr::n()]-time[1], .groups="drop")
  auc <- auc %>% dplyr::left_join(diff, by="id")
  cavg <- auc %>% dplyr::mutate(value=value/diff_time) %>% dplyr::select(-diff_time) 
  return(cavg)
}