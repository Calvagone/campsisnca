#'
#' Replicated NCA options class.
#'
#' @export
setClass(
  "replicated_nca_options",
  representation(
    selected_statistics = "character", # vector
    summary_stat_display = "character", # vector
    summary_stat_signif_digits = "integer", # integer value
    strata = "character" # Named vector
  ),
  prototype = prototype(
    selected_statistics = character(), # empty means no filter (all stats used)
    summary_stat_display = get_stat_display_default(),
    summary_stat_signif_digits = 3L,
    strata = get_default_strata()
  )
)

#'
#' Replicated NCA options.
#'
#' @param selected_statistics NCA metrics statistics to keep (e.g. mean, etc) when summary statistics are computed on replicated output.
#'  Default is the empty character vector (all statistics are computed).
#' @param summary_stat_display display format for replicate statistics, character vector. Default is \verb{'{median} ({p5}–{p95})'}.
#' @param summary_stat_signif_digits number of significant digits to display for replicate statistics, default is 3.
#' @param strata strata levels this analysis refers to, named vector, e.g. c(ARM='1g QD').
#'  Note, the default strata are c(SCENARIO='all', ARM='all').
#'  Use 'all' if this analysis refers to all levels for the specified stratification variable.
#'  By default, a stratification variable that has only 1 level is ignored.
#' @export
ReplicatedNCAOptions <- function(
  selected_statistics = character(),
  summary_stat_display = get_stat_display_default(),
  summary_stat_signif_digits = 3L,
  strata = get_default_strata()
) {
  if (is.null(strata)) {
    strata = get_default_strata()
  }
  return(new(
    "replicated_nca_options",
    selected_statistics = selected_statistics,
    summary_stat_display = summary_stat_display,
    summary_stat_signif_digits = summary_stat_signif_digits,
    strata = strata
  ))
}

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature=c("replicated_nca_options", "json_element"), definition=function(object, json) {
  return(map_json_properties_to_s4_slots(object=object, json=json))
})
