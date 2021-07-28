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

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("ctrough_metric"), definition=function(object, ...) {
  object@individual <- ctrough_delegate(x=object@x, variable=object@variable, time=object@time)
  return(object)    
})

#_______________________________________________________________________________
#----                             getName                                   ----
#_______________________________________________________________________________

setMethod("getName", signature=c("ctrough_metric"), definition = function(x) {
  return("ctrough")
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
