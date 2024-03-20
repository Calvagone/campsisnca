
#' 
#' Custom deparse function. Works similarly to deparse1.
#' However, lines are trimmed before being concatenated.
#' 
#' @param x expression to deparse
#' @return a single string
#' 
deparse1Line <- function(x) {
  return(paste0(trimws(deparse(x)), collapse = ""))
}

#' 
#' Custom deparse function. Works similarly to deparse1.
#' However, lines are trimmed before being concatenated.
#' 
#' @param digits rounding digits definitions (integer, function, purrr-style lambda function or list of these)
#' @return a character vector, which will be pasted and given to gtsummary
#' 
deparseDigits <- function(digits) {
  if (is.null(digits)) {
    return(character(0))
  }
  retValue <- NULL
  for (item in digits) {
    if (rlang::is_function(item)) {
      retValue <- retValue %>%
        append(deparse1Line(item))
      
    } else if (rlang::is_lambda(item)) {
      retValue <- retValue %>%
        append(paste0("rlang::as_function(", deparse1Line(item), ")"))
      
    } else if (is.numeric(item)) {
      retValue <- retValue %>%
        append(as.character(item))
      
    } else {
      stop("Digit element must be a function, a purrr-style lambda function or simply an integer")
    }
  }
  return(retValue)
}
