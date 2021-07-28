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
#' @export
Ctrough <- function(x=NULL, variable=NULL, time) {
  x = processDataframe(x)
  variable = processVariable(variable)
  return(new("ctrough_metric", x=x, variable=variable, time=time))
}

#' @rdname calculate
setMethod("calculate", signature=c("ctrough_metric"), definition=function(object, ...) {
  object@individual <- ctrough_delegate(x=object@x, variable=object@variable, time=object@time)
  return(object)    
})

#_______________________________________________________________________________
#----                           implementation                              ----
#_______________________________________________________________________________


#' 
#' Compute Ctrough
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param variable dependent variable
#' @param t time value to read Ctrough
ctrough_delegate <- function(x, variable, t) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(id) %>% dplyr::filter(time==t) %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=id, value=dv_variable))
}
