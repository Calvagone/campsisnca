
#'
#' Make table.
#' 
#' @param metrics dataframe, mandatory columns: metric & cell
#' @param vgroup vertical group variable names
#' @param vsubgroup vertical subgroup variable names
#' @export
#' 
makeTable <- function(metrics, vgroup=NULL, vsubgroup=NULL) {
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
  
  retValue <- NULL
  
  # Remember order because spread does not preserve the initial order
  preferredOrder <- c(vgroup, vsubgroup, metrics$metric) %>% unique()

  for (vgroupCat in vgroupCats) {
    row_ <- metrics %>% dplyr::filter_at(.vars=vgroup, .vars_predicate=~.x==vgroupCat)
    for (vsubgroupCat in vsubgroupCats) {
      row <- row_ %>% dplyr::filter_at(.vars=vsubgroup, .vars_predicate=~.x==vsubgroupCat)
      retValue <- dplyr::bind_rows(retValue, tidyr::spread(data=row, key=metric, value=cell))
    }
  }
  # Reorder
  retValue <- retValue[, order(match(colnames(retValue), preferredOrder))]
  return(retValue)
}

#'
#' Make kable.
#' 
#' @param x table object
#' @param df output of makeTable, dataframe
#' @param vgroup vertical group variable names
#' @param vsubgroup vertical subgroup variable names
#' @param format can be 'html' or "pdf'
#' @export
#' 
makeKable <- function(x, df, vgroup=NULL, vsubgroup=NULL, format="html") {
  escape <- FALSE
  
  
  if (is.null(vsubgroup)) {
    # Remove vertical group column header
    tmp <- df %>% dplyr::rename_at(.vars=vgroup, .funs=~" ")
    
    # Header and units processing
    headers <- colnames(tmp)

    # Make kable
    retValue <- kableExtra::kbl(tmp, format=format, escape=escape, row.names=FALSE, col.names=addUnits(x, headers, format), align="c") %>%
      kableExtra::kable_paper("striped", full_width=F)
    
  } else {
    group_info <- df %>% dplyr::mutate(INDEX_COL=seq_len(dplyr::n())) %>%
      dplyr::group_by_at(vgroup) %>%
      dplyr::summarise(MIN_INDEX=min(INDEX_COL), MAX_INDEX=max(INDEX_COL)) %>%
      dplyr::arrange(MIN_INDEX)
    
    # Remove vertical group column as info is stored in group_info
    tmp <- df %>% dplyr::select(-dplyr::all_of(c(vgroup)))
    
    # Remove vertical subgroup column header
    tmp <- tmp %>% dplyr::rename_at(.vars=vsubgroup, .funs=~" ")
    
    # Header and units processing
    headers <- colnames(tmp)

    # Make kable
    retValue <- kableExtra::kbl(tmp, format=format, escape=escape, row.names=FALSE, col.names=addUnits(x, headers, format), align="c") %>%
      kableExtra::kable_paper("striped", full_width=F)
    
    # Make groups
    for (rowIndex in seq_len(nrow(group_info))) {
      row <- group_info[rowIndex, ]
      retValue <- retValue %>% kableExtra::pack_rows(row %>% dplyr::pull(vgroup), row$MIN_INDEX, row$MAX_INDEX)
    }
  }
  return(retValue %>% kableExtra::kable_styling(latex_options="HOLD_position"))
}

addUnits <- function(x, headers, format) {
  isHtml <- format=="html"
  units <- headers %>% purrr::map_chr(.f=~ifelse(.x != " ", x %>% getUnit(metric=.x), as.character(NA)))
  separator <- ifelse(x@unit_linebreak, ifelse(isHtml, "<br>", "\n"), " ")
  headers <- paste0(headers, ifelse(is.na(units), "", paste0(separator, "(", units, ")")))
  if (!isHtml) {
    headers <- kableExtra::linebreak(headers, align="c")
    # See https://stackoverflow.com/questions/52944533/wrap-text-in-knitrkable-table-cell-using-n
  }
  return(headers)
}

