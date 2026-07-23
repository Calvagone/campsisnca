#'
#' Replicated NCA options class.
#'
#' @export
setClass(
  "replicated_nca_options",
  representation(
    selected_stastistics = "character", # vector
    summary_stat_display = "character", # vector
    summary_stat_signif_digits = "integer" # integer value
  ),
  prototype = prototype(
    selected_stastistics = character(), # empty means no filter (all stats used)
    summary_stat_display = get_stat_display_default(),
    summary_stat_signif_digits = 3L
  )
)

#'
#' Replicated NCA options.
#'
#' @param selected_stastistics NCA metrics statistics to keep (e.g. mean, etc) when summary statistics are computed on replicated output.
#'  Default is the empty character vector (all statistics are computed).
#' @param summary_stat_display display format for replicate statistics, character vector. Default is \verb{'{median} ({p5}–{p95})'}.
#' @param summary_stat_signif_digits number of significant digits to display for replicate statistics, default is 3.
#' @export
ReplicatedNCAOptions <- function(
  selected_stastistics = character(),
  summary_stat_display = get_stat_display_default(),
  summary_stat_signif_digits = 3L
) {
  return(new(
    "replicated_nca_options",
    selected_stastistics = selected_stastistics,
    summary_stat_display = summary_stat_display,
    summary_stat_signif_digits = summary_stat_signif_digits
  ))
}

#_______________________________________________________________________________
#----                          load_from_json                               ----
#_______________________________________________________________________________

setMethod("load_from_json", signature=c("replicated_nca_options", "json_element"), definition=function(object, json) {
  return(map_json_properties_to_s4_slots(object=object, json=json))
})
