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
#' @param time time value to read Ctrough. If not provided, last concentrations from x will be returned.
#' @export
Ctrough <- function(x=NULL, variable=NULL, time=NULL, name=NULL, unit=NULL) {
  x = processDataframe(x)
  variable = processVariable(variable)
  name <- if (is.null(name)) "Ctrough" else name
  unit <- processUnit(unit)
  time <- if (is.null(time)) as.numeric(NA) else time
  return(new("ctrough_metric", x=x, variable=variable, time=time, name=name, unit=unit))
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
#' @param time time value to read Ctrough. If not provided, last concentrations from x will be returned.
#' @return individual ctrough
#' @importFrom dplyr group_by filter_at slice transmute ungroup
ctrough_delegate <- function(x, variable, time) {
  x <- x %>% standardise(variable)
  x <- x %>% dplyr::group_by(ID)
  if (is.na(time)) {
    x <- x %>% dplyr::slice(dplyr::n())
  } else {
    x <- x %>% dplyr::filter_at(.vars="TIME", .vars_predicate=~.x==time)
  }
  x <- x %>% dplyr::ungroup()
  return(x %>% dplyr::transmute(id=ID, value=dv_variable))
}
