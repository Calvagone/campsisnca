
#' Compute the geometric mean.
#' 
#' @param x numeric vector
#' @export
geomean <- function(x) {
  return(exp(mean(log(x))))
}

#' Compute the geometric CV.
#' 
#' @param x numeric vector
#' @export
geocv <- function(x) {
  return(100 * sqrt(exp(sd(log(x))^2)-1))
}

#' Compute the coefficient of variation.
#' 
#' @param x numeric vector
#' @export
cv <- function(x) {
  return(100 * sd(x)/mean(x))
}

#' Compute the standard error.
#' 
#' @param x numeric vector
#' @export
se <- function(x) {
  return(sd(x)/sqrt(length(x)))
}

adaptFootnote <- function(x) {
  x <- x %>% replaceAll(pattern=VariablePattern("geomean"), replacement="Geometric Mean")
  x <- x %>% replaceAll(pattern=VariablePattern("geocv"), replacement="Geometric CV")
  x <- x %>% replaceAll(pattern=VariablePattern("cv"), replacement="Coefficient of Variation")
  x <- x %>% replaceAll(pattern=VariablePattern("se"), replacement="Standard Error")
  return(x)
}
