
#' 
#' Default rounding function (3 significant numbers).
#' 
#' @param x values to be rounded
#' @param ... extra arguments, not used
#' @export
defaultRoundingFunction <- function(x, ...) {
  return(signif(x=x, digits=3))
}