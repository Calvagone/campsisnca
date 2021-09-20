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
Cavg <- function(x=NULL, variable=NULL, name=NULL, unit=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "Cavg" else name
  unit <- processUnit(unit)
  return(new("cavg_metric", x=x, variable=variable, name=name, unit=unit))
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("cavg_metric", "numeric"), definition=function(object, level, ...) {
  object@individual <- cavg_delegate(x=object@x, variable=object@variable)
  return(object %>% summariseIndividualData(level=level))    
})

#_______________________________________________________________________________
#----                           implementation                              ----
#_______________________________________________________________________________

#' 
#' Compute Cavg (C average).
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @return individual Cavg
#' @importFrom dplyr group_by left_join mutate n select
cavg_delegate <- function(x, variable) {
  auc <- auc_delegate(x=x, variable=variable)
  x <- x %>% standardise(variable)
  diff <- x %>% dplyr::group_by(ID) %>% dplyr::summarise(diff_time=TIME[dplyr::n()]-TIME[1], .groups="drop") %>% dplyr::rename(id=ID)
  auc <- auc %>% dplyr::left_join(diff, by="id")
  cavg <- auc %>% dplyr::mutate(value=value/diff_time) %>% dplyr::select(-diff_time) 
  return(cavg)
}
