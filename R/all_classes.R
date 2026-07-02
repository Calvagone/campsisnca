#' @import tibble
#' @import campsis
#' @importClassesFrom campsis campsis_tbl std_campsis_tbl
#' @export
setClassUnion("campsis_output", c("data.frame", "tbl_df", "campsis_tbl", "std_campsis_tbl"))
