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

#' Apply time window.
#'
#' `applyTimeWindow()` is deprecated in favor of `apply_time_window()`.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams apply_time_window
#' @return updated object
#' @export
#' @rdname applyTimeWindow
applyTimeWindow <- function(x, window, data_time_unit, ...) {
  lifecycle::deprecate_warn("1.7.0", "applyTimeWindow()", "apply_time_window()")
  apply_time_window(x = x, window = window, data_time_unit = data_time_unit, ...)
}

setGeneric("applyTimeWindow", function(x, window, data_time_unit, ...) {
  lifecycle::deprecate_warn("1.7.0", "applyTimeWindow()", "apply_time_window()")
  apply_time_window(x = x, window = window, data_time_unit = data_time_unit, ...)
})

#'
#' Discard category column.
#'
#' `discardCategoryColumn()` is deprecated in favor of `discard_category_column()`.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' @inheritParams discard_category_column
#' @return updated object
#' @export
#' @rdname discardCategoryColumn
discardCategoryColumn <- function(x, split = "_") {
  lifecycle::deprecate_warn("1.7.0", "discardCategoryColumn()", "discard_category_column()")
  discard_category_column(x = x, split = split)
}
