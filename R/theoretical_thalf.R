#_______________________________________________________________________________
#----                      theoretical_thalf_metric classes                 ----
#_______________________________________________________________________________

validate_theoretical_thalf_metric <- function(object) {
  return(c(
    expectZeroOrMore(object, "map"),
    expectOne(object, "subtype")
  ))
}

#' 
#' Get default name based on thalf subtype.
#'
#' @param subtype thalf subtype (2cpt.dist, 2cpt.z or 2cpt.eff)
get_default_thalf_name <- function(subtype) {
  if (subtype == "1cpt") {
    return("thalf.z")
  } else if (subtype == "2cpt.dist") {
    return("thalf.dist")
  } else if (subtype == "2cpt.z") {
    return("thalf.z")
  } else if (subtype == "2cpt.eff") {
    return("thalf.eff")
  } else {
    stop(paste0("Unknown subtype ", subtype))
  }
}

#' 
#' Theoretical thalf metric class.
#' 
#' @export
setClass(
  "theoretical_thalf_metric",
  representation(
    map = "character",
    subtype = "character"
  ),
  contains="nca_metric",
  prototype=prototype(subtype=as.character(NA)),
  validity=validate_theoretical_thalf_metric
)

#' 
#' Thalf metric required columns for a 1-compartment model.
#' 
#' @return character vector of required column names
#' @export
thalf.1cpt.required <- function() {
  return("K")
}

#' 
#' Thalf metric required columns for a 2-compartment model.
#' 
#' @return character vector of required column names
#' @export
thalf.2cpt.required <- function() {
  return(c("DOSE", "TAU", "CL", "V2", "Q", "V3", "KA"))
}

check_map <- function(map, thalf.1cpt=TRUE) {
  if (is.null(map)) {
    return(character(0))
  }
  if (thalf.1cpt) {
    thalf.required <- thalf.1cpt.required()
  } else {
    thalf.required <- thalf.2cpt.required()
  }
  assertthat::assert_that(is.character(map),
                          msg=paste0("map must be a character vector and may contain the following keys: ",
                                     paste0(thalf.required, collapse=", ")))
  keys <- names(map)
  check <- keys %in% thalf.required
  assertthat::assert_that(all(check),
                          msg=paste0("Unnecessary keys detected in map vector: ",
                                     paste0(keys[!check], collapse=", ")))
  return(map)
}

#' 
#' Theoretical half life for a 1-compartment model.
#' 
#' @inheritParams metrics_params
#' @param map character vector used for column mapping, only one key is possible: K
#' @export
Thalf.1cpt <- function(map=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  subtype <- "1cpt"
  metric <- nca_constructor(variable=as.character(NA), window=UndefinedTimeWindow(), name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="theoretical_thalf_metric")
  map <- check_map(map, thalf.1cpt=TRUE)
  metric@map <- map
  metric@subtype <- subtype
  return(set_default_name_if_na(metric))
}

#' 
#' Theoretical distribution half life for a 2-compartment model.
#' 
#' @inheritParams metrics_params
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @export
Thalf.2cpt.dist <- function(map=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  subtype <- "2cpt.dist"
  metric <- nca_constructor(variable=as.character(NA), window=UndefinedTimeWindow(), name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="theoretical_thalf_metric")
  map <- check_map(map, thalf.1cpt=FALSE)
  metric@map <- map
  metric@subtype <- subtype
  return(set_default_name_if_na(metric))
}

#' 
#' Theoretical elimination half life for a 2-compartment model.
#' 
#' @inheritParams metrics_params
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @export
Thalf.2cpt.z <- function(map=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  subtype <- "2cpt.z"
  metric <- nca_constructor(variable=as.character(NA), window=UndefinedTimeWindow(), name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="theoretical_thalf_metric")
  map <- check_map(map, thalf.1cpt=FALSE)
  metric@map <- map
  metric@subtype <- subtype
  return(set_default_name_if_na(metric))
}

#' 
#' Theoretical effective half life for a 2-compartment model.
#' 
#' @inheritParams metrics_params
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @export
Thalf.2cpt.eff <- function(map=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  subtype <- "2cpt.eff"
  metric <- nca_constructor(variable=as.character(NA), window=UndefinedTimeWindow(), name=name, unit=unit,
                           stat_display=stat_display, digits=digits,
                           metric_name="theoretical_thalf_metric")
  map <- check_map(map, thalf.1cpt=FALSE)
  metric@map <- map
  metric@subtype <- subtype
  return(set_default_name_if_na(metric))
}

#_______________________________________________________________________________
#----                          get_default_name                             ----
#_______________________________________________________________________________

#' @rdname get_default_name
setMethod("get_default_name", signature=c("theoretical_thalf_metric"), definition=function(object, ...) {
  return(get_default_thalf_name(object@subtype)) 
})

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("theoretical_thalf_metric", "campsis_output", "nca_options"), definition=function(object, x, options, ...) {
  subtype <- object@subtype

  if (subtype == "1cpt") {
    ind <- metrics.1cpt(x, map=object@map)
    ind <- ind %>% dplyr::transmute(id=ID, value=THALF)

  } else if (subtype %>% startsWith("2cpt")) {
    ind <- metrics.2cpt(x, map=object@map)

    if (subtype == "2cpt.dist") {
      ind <- ind %>% dplyr::transmute(id=ID, value=THALF_D)
    } else if (subtype == "2cpt.z") {
      ind <- ind %>% dplyr::transmute(id=ID, value=THALF_Z)
    } else if (subtype == "2cpt.eff") {
      ind <- ind %>% dplyr::transmute(id=ID, value=THALF_EFF)
    }

  } else {
    stop(paste0("Unknown subtype ", subtype))
  }
  args <- list(...)
  strat_vars <- process_extra_arg(args, name="strat_vars", mandatory=FALSE, default=character(0))
  object@individual <- ind
  structuredObj <- compute_nca_metric_summary(object=object, strat_vars=strat_vars, quantile_type=options@quantile_type)
  object@summary <- structuredObj$summary
  object@summary_pretty <- structuredObj$summary_pretty
  return(object)
})

#_______________________________________________________________________________
#----                          get_latex_name                               ----
#_______________________________________________________________________________

#' @rdname get_latex_name
setMethod("get_latex_name", signature=c("theoretical_thalf_metric"), definition = function(x) {
  name <- x %>% get_name()
  subtype <- x@subtype
  if (subtype == "1cpt") {
    retValue <- subscript_occurrence(name, "half\\.z", "\U00BD,z")
  } else if (subtype == "2cpt.dist") {
    retValue <- subscript_occurrence(name, "half\\.dist", "\U00BD,dist")
  } else if (subtype == "2cpt.z") {
    retValue <- subscript_occurrence(name, "half\\.z", "\U00BD,z")
  } else if (subtype == "2cpt.eff") {
    retValue <- subscript_occurrence(name, "half\\.eff", "\U00BD,eff")
  } else {
    retValue <- name
  }
  return(retValue)
})
