
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
