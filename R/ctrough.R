#_______________________________________________________________________________
#----                       ctrough_metric class                            ----
#_______________________________________________________________________________

validateCtroughMetric <- function(object) {
  return(expectOne(object, "time"))
}

#' 
#' Ctrough metric class.
#' 
#' @export
setClass(
  "ctrough_metric",
  representation(
    time = "numeric"
  ),
  contains="nca_metric",
  validity=validateCtroughMetric
)

#' 
#' Ctrough.
#' 
#' @inheritParams metricsParams
#' @param time time value to read Ctrough
#' @param name custom metric name
#' @export
Ctrough <- function(x=NULL, variable=NULL, time, name=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "Ctrough" else name
  return(new("ctrough_metric", x=x, variable=variable, time=time, name=name))
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("ctrough_metric", "numeric"), definition=function(object, level, ...) {
  object@individual <- ctrough_delegate(x=object@x, variable=object@variable, time=object@time)
  return(object %>% summariseIndividualData(level=level))    
})

#_______________________________________________________________________________
#----                           implementation                              ----
#_______________________________________________________________________________


#' 
#' Compute Ctrough
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @param time time value to read Ctrough
ctrough_delegate <- function(x, variable, time) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::filter_at(.vars="time", .vars_predicate=~.x==time) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, value=dv_variable))
}
