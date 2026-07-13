#' Compute the individual value of an individual.
#' 
#' `iValue()` is deprecated in favor of `i_value()`.
#' 
#' @description
#' `r lifecycle::badge("deprecated")`
#' 
#' @inheritParams i_value
#' @return individual value
#' @export
#' @rdname iValue
iValue <- function(object, time, value) {
  lifecycle::deprecate_warn("1.7.0", "iValue()", "i_value()")
  i_value(object = object, time = time, value = value)
}

setGeneric("iValue", function(object, time, value = NULL) {
  lifecycle::deprecate_warn("1.7.0", "iValue()", "i_value()")
  i_value(object = object, time = time, value = value)
})
