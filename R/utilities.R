#'
#' Get the Campsisnca options (R options).
#'
#' @return global options for Campsisnca
#' @export
#' @keywords internal
getCampsisncaOptions <- function() {
  return(getOption("campsisnca.options"))
}

#'
#' Get Campsisnca option logic.
#'
#' @param name option to search
#' @param default default value if option not found
#' @return option value
#' @export
getCampsisncaOption <- function(name, default) {
  option <- getCampsisncaOptions()
  if (is.null(option)) {
    return(default)
  } else {
    value <- option[[name]]
    if (is.null(value)) {
      return(default)
    } else {
      return(value)
    }
  }
}