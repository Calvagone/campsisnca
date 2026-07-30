#' Campsisnca table class (see this class as an interface)
#'
#' @name campsisnca_tbl-class
#' @aliases campsisnca_tbl
#' @docType class
#' @exportClass campsisnca_tbl
setOldClass(c("campsisnca_tbl", "campsis_tbl", "tbl_df", "data.frame"))

#' Summary Campsisnca table class
#'
#' @name summary_campsisnca_tbl-class
#' @aliases summary_campsisnca_tbl
#' @docType class
#' @exportClass summary_campsisnca_tbl
setOldClass(c(
  "summary_campsisnca_tbl",
  "campsisnca_tbl",
  "campsis_tbl",
  "tbl_df",
  "data.frame"
))

#' Summary (wide format) Campsisnca table class
#'
#' @name summary_wide_campsisnca_tbl-class
#' @aliases summary_wide_campsisnca_tbl
#' @docType class
#' @exportClass summary_wide_campsisnca_tbl
setOldClass(c(
  "summary_wide_campsisnca_tbl",
  "campsisnca_tbl",
  "campsis_tbl",
  "tbl_df",
  "data.frame"
))

#' Summary (pretty format) Campsisnca table class
#'
#' @name summary_pretty_campsisnca_tbl-class
#' @aliases summary_pretty_campsisnca_tbl
#' @docType class
#' @exportClass summary_pretty_campsisnca_tbl
setOldClass(c(
  "summary_pretty_campsisnca_tbl",
  "campsisnca_tbl",
  "campsis_tbl",
  "tbl_df",
  "data.frame"
))

#' Individual Campsisnca table class
#'
#' @name individual_campsisnca_tbl-class
#' @aliases individual_campsisnca_tbl
#' @docType class
#' @exportClass summary_campsisnca_tbl
setOldClass(c(
  "individual_campsisnca_tbl",
  "campsisnca_tbl",
  "campsis_tbl",
  "tbl_df",
  "data.frame"
))

#' Individual (wide format) Campsisnca table class
#'
#' @name individual_wide_campsisnca_tbl-class
#' @aliases individual_wide_campsisnca_tbl
#' @docType class
#' @exportClass individual_wide_campsisnca_tbl
setOldClass(c(
  "individual_wide_campsisnca_tbl",
  "campsisnca_tbl",
  "campsis_tbl",
  "tbl_df",
  "data.frame"
))

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
setClassUnion(
  "campsis_output",
  c("data.frame", "tbl_df", "campsis_tbl", "std_campsis_tbl")
)

#' The Campsisnca Output Class Union
#'
#' @description A class union containing standard data frames, tibbles,
#' and Campsisnca-specific table objects.
#'
#' @name campsisnca_output-class
#' @aliases campsisnca_output
#' @docType class
#' @import tibble
#' @export
setClassUnion(
  "campsisnca_output",
  c(
    "data.frame",
    "tbl_df",
    "campsisnca_tbl",
    "individual_campsisnca_tbl",
    "individual_wide_campsisnca_tbl",
    "summary_campsisnca_tbl",
    "summary_wide_campsisnca_tbl",
    "summary_pretty_campsisnca_tbl"
  )
)

#' @importFrom methods is new
#' @importFrom stats lm median rnorm sd
#' @importFrom campsismod find replace add export get_name load_from_json replaceAll Equation VariablePattern JSONElement process_extra_arg
NULL

# Prevent R CMD check notes on global variables
utils::globalVariables(c(
  ".",
  ".data",
  "A",
  "ALPHA",
  "AUC_Z",
  "AUC_Z_1",
  "AUC_Z_1_pc",
  "AUC_Z_pc",
  "B",
  "BETA",
  "C",
  "CL",
  "COEFF_A",
  "COEFF_B",
  "COEFF_C",
  "DETER",
  "DOSE",
  "ID",
  "K",
  "K20",
  "K23",
  "K32",
  "KA",
  "Q",
  "THALF",
  "THALF_D",
  "THALF_EFF",
  "THALF_Z",
  "TIME",
  "V2",
  "V3",
  "categorical",
  "category",
  "discrete_value",
  "metric",
  "stat",
  "stat_name",
  "summary_stats",
  "value",
  "variable_level"
))
