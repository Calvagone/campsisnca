
#'
#' Make table.
#' 
#' @param metrics dataframe, mandatory columns: metric & cell
#' @param vgroup vertical group variable names
#' @param vsubgroup vertical subgroup variable names
#' @export
#' 
makeTable <- function(metrics, vgroup=NULL, vsubgroup=NULL) {
  original_subgroup = vsubgroup
  if (is.null(vsubgroup)) {
    vsubgroup = vgroup
  }
  assertthat::assert_that(is.character(vgroup) && length(vgroup)==1,
                          msg=paste0("argument '", vgroup, "' must be a single character value"))
  assertthat::assert_that(is.character(vsubgroup) && length(vsubgroup)==1,
                          msg=paste0("argument '", vsubgroup, "' must be a single character value"))
  assertthat::assert_that("metric" %in% colnames(metrics),
                          msg="Column name 'metric' is missing")
  assertthat::assert_that("cell" %in% colnames(metrics),
                          msg="Column name 'cell' is missing")
  
  vgroupCats <- metrics %>% dplyr::pull(vgroup) %>% unique()
  vsubgroupCats <- metrics %>% dplyr::pull(vsubgroup) %>% unique()
  
  metrics_ <- NULL
  
  # Remember order because spread does not preserve the initial order
  preferredOrder <- c(vgroup, vsubgroup, metrics$metric) %>% unique()

  for (vgroupCat in vgroupCats) {
    row_ <- metrics %>% dplyr::filter_at(.vars=vgroup, .vars_predicate=~.x==vgroupCat)
    for (vsubgroupCat in vsubgroupCats) {
      row <- row_ %>% dplyr::filter_at(.vars=vsubgroup, .vars_predicate=~.x==vsubgroupCat)
      metrics_ <- dplyr::bind_rows(metrics_, tidyr::spread(data=row, key=metric, value=cell))
    }
  }
  # Reorder
  metrics_ <- metrics_[, order(match(colnames(metrics_), preferredOrder))]
  
  return(metrics_ %>% makeKable(vgroup=vgroup, vsubgroup=original_subgroup))
}

#'
#' Make kable.
#' 
#' @param metrics_ dataframe, mandatory columns: metric & cell
#' @param vgroup vertical group variable names
#' @param vsubgroup vertical subgroup variable names
#' @export
#' 
makeKable <- function(metrics_, vgroup=NULL, vsubgroup=NULL) {
  escape <- FALSE
  if (is.null(vsubgroup)) {
    # Remove vertical group column header
    tmp <- metrics_ %>% dplyr::rename_at(.vars=vgroup, .funs=~" ")
    
    # Make kable
    retValue <- kableExtra::kbl(tmp, format="html", escape=escape, row.names=FALSE) %>% kableExtra::kable_paper("striped", full_width=F)
    
  } else {
    group_info <- metrics_ %>% dplyr::mutate(INDEX_COL=seq_len(dplyr::n())) %>%
      dplyr::group_by_at(vgroup) %>%
      dplyr::summarise(MIN_INDEX=min(INDEX_COL), MAX_INDEX=max(INDEX_COL)) %>%
      dplyr::arrange(MIN_INDEX)
    
    # Remove vertical group column as info is stored in group_info
    tmp <- metrics_ %>% dplyr::select(-dplyr::all_of(c(vgroup)))
    
    # Remove vertical subgroup column header
    tmp <- tmp %>% dplyr::rename_at(.vars=vsubgroup, .funs=~" ")
    
    # Make kable
    retValue <- kableExtra::kbl(tmp, format="html", escape=escape, row.names=FALSE) %>% kableExtra::kable_paper("striped", full_width=F)
    
    # Make groups
    for (rowIndex in seq_len(nrow(group_info))) {
      row <- group_info[rowIndex, ]
      retValue <- retValue %>% kableExtra::pack_rows(row %>% dplyr::pull(vgroup), row$MIN_INDEX, row$MAX_INDEX)
    }
  }
  
  return(retValue)
}
