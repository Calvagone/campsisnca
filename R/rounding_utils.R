
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
#' @importFrom purrr map_chr
#' 
deparseDigits <- function(digits) {
  if (is.null(digits)) {
    return(character(0))
  }
  if (is.numeric(digits) || is.list(digits)) {
    retValue <- digits %>% purrr::map_chr(~deparseDigit(.x))
  } else {
    retValue <- deparseDigit(digits)
  }
  return(retValue)
}

#' @importFrom rlang is_function is_lambda
deparseDigit <- function(digit) {
  if (rlang::is_function(digit)) {
    retValue <- deparse1Line(digit)
    
  } else if (rlang::is_formula(digit)) {
    retValue <- paste0("rlang::as_function(", deparse1Line(digit), ")")
    
  } else if (is.numeric(digit)) {
    retValue <- as.character(digit)
    
  } else {
    stop("Digit element must be a function, a purrr-style lambda function or simply an integer")
  }
  return(retValue)
}
