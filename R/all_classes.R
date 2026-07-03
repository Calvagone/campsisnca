#' The Campsis Output Class Union
#'
#' @description A class union containing standard data frames, tibbles, 
#' and Campsis-specific table objects.
#'
#' @name campsis_output-class
#' @aliases campsis_output
#' @docType class
#' @import tibble
#' @import campsis
#' @importClassesFrom campsis campsis_tbl std_campsis_tbl
#' @export
setClassUnion("campsis_output", c("data.frame", "tbl_df", "campsis_tbl", "std_campsis_tbl"))
