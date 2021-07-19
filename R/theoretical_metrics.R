
renameOriginalColumn <- function(x, from, to) {
  if (is.null(from)) {
    if (!(to %in% colnames(x))) {
      stop(paste0("Column ", to, " not found in data frame"))
    }
  } else {
    assertthat::assert_that(is.character(from) && length(from)==1, msg=paste0("argument ", to, " must be a single character value"))
    if (from %in% colnames(x)) {
      x <- x %>% dplyr::rename_at(.vars=from, .funs=~to)
    } else {
      stop(paste0("Column ", from, " not found in data frame (renaming has failed)"))
    }
  }
  return(x)
}

#' 
#' Theoretical metrics for 2-cpt model results.
#' 
#' @param x CAMPSIS/NONMEM dataframe
#' @param DOSE column name corresponding to DOSE in x, character value
#' @param TAU column name corresponding to TAU in x, character value
#' @param CL column name corresponding to CL in x, character value
#' @param V2 column name corresponding to V2 in x, character value
#' @param Q column name corresponding to Q in x, character value
#' @param V3 column name corresponding to V3 in x, character value
#' @param KA column name corresponding to KA in x, character value
#' @return theoretical metrics
#' @export
metrics.2cpt <- function(x, DOSE=NULL, TAU=NULL, CL=NULL, V2=NULL, Q=NULL, V3=NULL, KA=NULL) {
  x <- x %>% toCAMPSISDataframe()
  x <- x %>% renameOriginalColumn(from=DOSE, to="DOSE")
  x <- x %>% renameOriginalColumn(from=TAU, to="TAU")
  x <- x %>% renameOriginalColumn(from=CL, to="CL")
  x <- x %>% renameOriginalColumn(from=V2, to="V2")
  x <- x %>% renameOriginalColumn(from=Q, to="Q")
  x <- x %>% renameOriginalColumn(from=V3, to="V3")
  x <- x %>% renameOriginalColumn(from=KA, to="KA")
  x <- x %>% dplyr::select(id, time, DOSE, CL, V2, Q, V3, KA)
  
  # Keep first row
  x <- x %>% dplyr::group_by(id) %>% dplyr::slice(1) %>% dplyr::ungroup()
  
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
  x <- x %>% dplyr::mutate(THALF=log(2)/ALPHA)
  x <- x %>% dplyr::mutate(THALF_Z=log(2)/BETA)
  x <- x %>% dplyr::mutate(THALF_EFF=THALF*AUC_Z_1_pc/100 + THALF_Z*AUC_Z_pc/100)
  
  return(x)
}
