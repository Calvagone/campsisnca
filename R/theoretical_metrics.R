rename_original_column <- function(x, from, to) {
  assertthat::assert_that(
    is.character(from) && length(from) == 1,
    msg = paste0("argument ", to, " must be a single character value")
  )
  if (from %in% colnames(x)) {
    x <- x %>% dplyr::rename_at(.vars = from, .funs = ~to)
  } else {
    stop(paste0("Column ", from, " not found in data frame (renaming has failed)"))
  }
  return(x)
}

#'
#' Pre-processing for metrics.1cpt and metrics.2cpt.
#'
#' @param x CAMPSIS/NONMEM dataframe
#' @param map character vector used for column mapping, only one key is possible: K
#' @param thalf.1cpt logical value
#' @return theoretical metrics
#' @importFrom dplyr all_of group_by select slice ungroup
#' @importFrom purrr map2
#' @importFrom campsis obs_only
#' @importFrom rlang .data
#' @export
metrics.common <- function(x, map, thalf.1cpt) {
  x <- x %>% campsis::obs_only()
  map <- check_map(map, thalf.1cpt = thalf.1cpt)

  # Remap columns
  purrr::map2(.x = names(map), .y = as.character(map), .f = function(key, value) {
    x <<- x %>% rename_original_column(from = value, to = key)
  })

  # Check all columns are there
  required <- if (thalf.1cpt) thalf.1cpt.required() else thalf.2cpt.required()
  checkCols <- required %in% colnames(x)
  assertthat::assert_that(
    all(checkCols),
    msg = paste0("Missing columns in x: ", paste0(required[!checkCols], collapse = ", "))
  )

  # Get rid of useless columns
  x <- x %>% dplyr::select(dplyr::all_of(c("ID", "TIME", required)))

  # Keep first row
  x <- x %>% dplyr::group_by(.data$ID) %>% dplyr::slice(1) %>% dplyr::ungroup()
  return(x)
}

#'
#' Theoretical metrics for 1-cpt model results.
#'
#' @param x CAMPSIS/NONMEM dataframe
#' @param map character vector used for column mapping, only one key is possible: K
#' @return theoretical metrics
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
metrics.1cpt <- function(x, map = character(0)) {
  # Pre-processing
  x <- x %>% metrics.common(map = map, thalf.1cpt = TRUE)

  # Compute
  x <- x %>% dplyr::mutate(THALF = log(2) / .data$K) # Elimination half life

  return(x)
}

#'
#' Theoretical metrics for 2-cpt model results.
#'
#' @param x CAMPSIS/NONMEM dataframe
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @return theoretical metrics
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @export
metrics.2cpt <- function(x, map = character(0)) {
  # Pre-processing
  x <- x %>% metrics.common(map = map, thalf.1cpt = FALSE)

  # Compute
  x <- x %>% dplyr::mutate(K20 = .data$CL / .data$V2, K23 = .data$Q / .data$V2, K32 = .data$Q / .data$V3)
  x <- x %>%
    dplyr::mutate(
      A = 1,
      B = .data$K20 + .data$K23 + .data$K32,
      C = .data$K20 * .data$K32,
      DETER = .data$B^2 - 4 * .data$A * .data$C
    )
  x <- x %>% dplyr::mutate(ALPHA = (.data$B + sqrt(.data$DETER)) / 2)
  x <- x %>% dplyr::mutate(BETA = (.data$B - sqrt(.data$DETER)) / 2)
  x <- x %>%
    dplyr::mutate(
      COEFF_A = .data$DOSE *
        .data$KA *
        (.data$K32 - .data$ALPHA) /
        (.data$V2 * (.data$BETA - .data$ALPHA) * (.data$KA - .data$ALPHA))
    )
  x <- x %>%
    dplyr::mutate(
      COEFF_B = .data$DOSE *
        .data$KA *
        (.data$K32 - .data$BETA) /
        (.data$V2 * (.data$ALPHA - .data$BETA) * (.data$KA - .data$BETA))
    )
  x <- x %>%
    dplyr::mutate(
      COEFF_C = .data$DOSE *
        .data$KA *
        (.data$K32 - .data$KA) /
        (.data$V2 * (.data$ALPHA - .data$KA) * (.data$BETA - .data$KA))
    )
  x <- x %>% dplyr::mutate(AUC = .data$DOSE / .data$CL)
  x <- x %>% dplyr::mutate(AUC_Z_1 = .data$COEFF_A / .data$ALPHA)
  x <- x %>% dplyr::mutate(AUC_Z = .data$COEFF_B / .data$BETA)
  x <- x %>% dplyr::mutate(AUC_KA = .data$COEFF_C / .data$KA)
  x <- x %>% dplyr::mutate(AUC_Z_1_pc = .data$AUC_Z_1 / (.data$AUC_Z_1 + .data$AUC_Z) * 100)
  x <- x %>% dplyr::mutate(AUC_Z_pc = .data$AUC_Z / (.data$AUC_Z_1 + .data$AUC_Z) * 100)
  x <- x %>% dplyr::mutate(THALF_D = log(2) / .data$ALPHA) # Distribution half life
  x <- x %>% dplyr::mutate(THALF_Z = log(2) / .data$BETA) # Elimination half life
  x <- x %>% dplyr::mutate(THALF_EFF = .data$THALF_D * .data$AUC_Z_1_pc / 100 + .data$THALF_Z * .data$AUC_Z_pc / 100) # Effective half life

  return(x)
}
