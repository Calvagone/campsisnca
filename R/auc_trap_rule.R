#'
#' NOTE: This method has been copied from the qpNCA package (qPharmetra).
#'  
#' Calculate Area Under the Curve Using Trapezoids.
#'
#' Calculates AUC using the trapezoidal method. Assumes data represent a single profile.
#' Despite choice of method, only linear interpolation is used
#' for areas of intervals beginning or ending with y: 0.
#'
#' @param x x variable, i.e. time
#' @param y y variable, i.e. concentration
#' @param method method:
#' * 1: linear up - linear down
#' * 2: linear up - logarithmic down
#' * 3: linear before Tmax, logarithmic after Tmax
#' @return area (length-one numeric)
#' @importFrom dplyr first lag
trap <- function(x = NA, y = NA, method = 1){
  stopifnot(length(x) == length(y))
  cm <- max(y, na.rm = T )
  tmax <- dplyr::first(x[y == cm & !is.na(y) ])
  if (method == 1){
    z <- sum( (x-dplyr::lag(x)) * (y+dplyr::lag(y)) / 2, na.rm = T )
  }
  if (method==2) {
    z <- sum(
      ifelse(
        dplyr::lag(y) > y & dplyr::lag(y) > 0 & y > 0,
        (x - dplyr::lag(x)) * (y - dplyr::lag(y)) / log(y/dplyr::lag(y)), # See Rowland & Tozer 471
        (x - dplyr::lag(x)) * (y + dplyr::lag(y)) / 2
      ),
      na.rm=T
    )
  }
  if (method==3) {
    z <- sum(
      ifelse(
        x > tmax    &
          dplyr::lag(y) > 0  &
          y > 0       &
          dplyr::lag(y) != y,
        
        (x-dplyr::lag(x))*(y-dplyr::lag(y))/log(y/dplyr::lag(y)),
        (x-dplyr::lag(x))*(y+dplyr::lag(y))/2
      ),
      na.rm=T
    )
  }
  
  return(z)
}