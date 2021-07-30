
renameOriginalColumn <- function(x, from, to) {
  assertthat::assert_that(is.character(from) && length(from)==1, msg=paste0("argument ", to, " must be a single character value"))
  if (from %in% colnames(x)) {
    x <- x %>% dplyr::rename_at(.vars=from, .funs=~to)
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
#' @export
metrics.common <- function(x, map, thalf.1cpt) {
  x <- x %>% toCAMPSISDataframe()
  map <- checkMap(map, thalf.1cpt=thalf.1cpt)
  
  # Remap columns
  purrr::map2(.x=names(map), .y=as.character(map), .f=function(key, value) {
    x <<- x %>% renameOriginalColumn(from=value, to=key)
  })
  
  # Check all columns are there
  required <- if (thalf.1cpt) thalf.1cpt.required() else thalf.2cpt.required()
  checkCols <- required %in% colnames(x)
  assertthat::assert_that(all(checkCols), msg=paste0("Missing columns in x: ",
                                                    paste0(required[!checkCols], collapse=", ")))
  
  # Get rid of useless columns
  x <- x %>% dplyr::select(dplyr::all_of(c("id", "time", required)))
  
  # Keep first row
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(1) %>% dplyr::ungroup()
  return(x)
}

#' 
#' Theoretical metrics for 1-cpt model results.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param map character vector used for column mapping, only one key is possible: K
#' @return theoretical metrics
#' @importFrom dplyr mutate
#' @export
metrics.1cpt <- function(x, map=character(0)) {
  # Pre-processing
  x <- x %>% metrics.common(map=map, thalf.1cpt=TRUE)
  
  # Compute
  x <- x %>% dplyr::mutate(THALF=log(2)/K) # Elimination half life
  
  return(x)
}

#' 
#' Theoretical metrics for 2-cpt model results.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @return theoretical metrics
#' @importFrom dplyr mutate
#' @export
metrics.2cpt <- function(x, map=character(0)) {
  # Pre-processing
  x <- x %>% metrics.common(map=map, thalf.1cpt=FALSE)
  
  # Compute
  x <- x %>% dplyr::mutate(K20=CL/V2, K23=Q/V2, K32=Q/V3)
  x <- x %>% dplyr::mutate(A=1, B=K20+K23+K32, C=K20*K32, DETER=B^2-4*A*C)
  x <- x %>% dplyr::mutate(ALPHA=(B+sqrt(DETER))/2)
  x <- x %>% dplyr::mutate(BETA=(B-sqrt(DETER))/2)
  x <- x %>% dplyr::mutate(COEFF_A=DOSE*KA*(K32-ALPHA)/(V2*(BETA-ALPHA)*(KA-ALPHA)))
  x <- x %>% dplyr::mutate(COEFF_B=DOSE*KA*(K32-BETA)/(V2*(ALPHA-BETA)*(KA-BETA)))
  x <- x %>% dplyr::mutate(COEFF_C=DOSE*KA*(K32-KA)/(V2*(ALPHA-KA)*(BETA-KA)))
  x <- x %>% dplyr::mutate(AUC=DOSE/CL)
  x <- x %>% dplyr::mutate(AUC_Z_1=COEFF_A/ALPHA)
  x <- x %>% dplyr::mutate(AUC_Z=COEFF_B/BETA)
  x <- x %>% dplyr::mutate(AUC_KA=COEFF_C/KA)
  x <- x %>% dplyr::mutate(AUC_Z_1_pc=AUC_Z_1/(AUC_Z_1+AUC_Z)*100)
  x <- x %>% dplyr::mutate(AUC_Z_pc=AUC_Z/(AUC_Z_1+AUC_Z)*100)
  x <- x %>% dplyr::mutate(THALF_D=log(2)/ALPHA) # Distribution half life
  x <- x %>% dplyr::mutate(THALF_Z=log(2)/BETA)  # Elimination half life
  x <- x %>% dplyr::mutate(THALF_EFF=THALF_D*AUC_Z_1_pc/100 + THALF_Z*AUC_Z_pc/100) # Effective half life
  
  return(x)
}
