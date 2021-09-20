#_______________________________________________________________________________
#----                          auc_metric class                             ----
#_______________________________________________________________________________

validateAUCMetric <- function(object) {
  return(expectOne(object, "method"))
}

#' 
#' AUC metric class.
#' 
#' @export
setClass(
  "auc_metric",
  representation(
    method = "integer"
  ),
  contains="nca_metric",
  validity=validateAUCMetric
)

#' 
#' Auc.
#' 
#' @inheritParams metricsParams
#' @param method method:
#' * 1: linear up - linear down
#' * 2: linear up - logarithmic down
#' * 3: linear before Tmax, logarithmic after Tmax
#' @export
Auc <- function(x=NULL, variable=NULL, method=1, name=NULL, unit=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "AUC" else name
  unit <- processUnit(unit)
  assertthat::assert_that(method %in% c(1,2,3), msg="method must be 1, 2 or 3")
  return(new("auc_metric", x=x, variable=variable, method=as.integer(method), name=name, unit=unit))
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("auc_metric", "numeric"), definition=function(object, level, ...) {
  object@individual <- auc_delegate(x=object@x, variable=object@variable, method=object@method)
  return(object %>% summariseIndividualData(level=level))    
})

#_______________________________________________________________________________
#----                           implementation                              ----
#_______________________________________________________________________________

#' 
#' Compute AUC.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @param method method:
#' * 1: linear up - linear down
#' * 2: linear up - logarithmic down
#' * 3: linear before Tmax, logarithmic after Tmax
#' @return individual AUC
#' @importFrom dplyr group_by rename summarise
auc_delegate <- function(x, variable, method=1) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(ID) %>% dplyr::summarise(value=trap(x=TIME, y=dv_variable, method=method), .groups="drop")
  return(x %>% dplyr::rename(id=ID))
}
