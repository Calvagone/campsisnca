#_______________________________________________________________________________
#----                      theoretical_thalf_metric class                   ----
#_______________________________________________________________________________

validateTheoreticalThalfMetric <- function(object) {
  return(c(
    expectZeroOrMore(object, "map"),
    expectOne(object, "subtype")
  ))
}

#' 
#' Get default name based on thalf subtype.
#'
#' @param subtype thalf subtype (2cpt.dist, 2cpt.z or 2cpt.eff)
getDefaultTHalfName <- function(subtype) {
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
  validity=validateTheoreticalThalfMetric
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

checkMap <- function(map, thalf.1cpt=TRUE) {
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
#' @inheritParams metricsParams
#' @param map character vector used for column mapping, only one key is possible: K
#' @export
Thalf.1cpt <- function(x=NULL, map=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  subtype <- "1cpt"
  metric <- ncaConstructor(x=x, variable=as.character(NA), name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="theoretical_thalf_metric", def_name=getDefaultTHalfName(subtype))
  map <- checkMap(map, thalf.1cpt=TRUE)
  metric@map <- map
  metric@subtype <- subtype
  return(metric)
}

#' 
#' Theoretical distribution half life for a 2-compartment model.
#' 
#' @inheritParams metricsParams
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @export
Thalf.2cpt.dist <- function(x=NULL, map=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  subtype <- "2cpt.dist"
  metric <- ncaConstructor(x=x, variable=as.character(NA), name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="theoretical_thalf_metric", def_name=getDefaultTHalfName(subtype))
  map <- checkMap(map, thalf.1cpt=FALSE)
  metric@map <- map
  metric@subtype <- subtype
  return(metric)
}

#' 
#' Theoretical elimination half life for a 2-compartment model.
#' 
#' @inheritParams metricsParams
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @export
Thalf.2cpt.z <- function(x=NULL, map=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  subtype <- "2cpt.z"
  metric <- ncaConstructor(x=x, variable=as.character(NA), name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="theoretical_thalf_metric", def_name=getDefaultTHalfName(subtype))
  map <- checkMap(map, thalf.1cpt=FALSE)
  metric@map <- map
  metric@subtype <- subtype
  return(metric)
}

#' 
#' Theoretical effective half life for a 2-compartment model.
#' 
#' @inheritParams metricsParams
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @export
Thalf.2cpt.eff <- function(x=NULL, map=NULL, name=NULL, unit=NULL, stat_display=NULL, digits=NULL) {
  subtype <- "2cpt.eff"
  metric <- ncaConstructor(x=x, variable=as.character(NA), name=name, unit=unit, stat_display=stat_display, digits=digits,
                           metric_name="theoretical_thalf_metric", def_name=getDefaultTHalfName(subtype))
  map <- checkMap(map, thalf.1cpt=FALSE)
  metric@map <- map
  metric@subtype <- subtype
  return(metric)
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("theoretical_thalf_metric", "numeric"), definition=function(object, level, ...) {
  subtype <- object@subtype

  if (subtype == "1cpt") {
    ind <- metrics.1cpt(object@x, map=object@map)
    ind <- ind %>% dplyr::transmute(id=ID, value=THALF)

  } else if (subtype %>% startsWith("2cpt")) {
    ind <- metrics.2cpt(object@x, map=object@map)

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
  object@individual <- ind
  object@summary <- computeTableSummary(object=object)
  return(object)
})

#_______________________________________________________________________________
#----                           getLaTeXName                                ----
#_______________________________________________________________________________

#' @rdname getLaTeXName
setMethod("getLaTeXName", signature=c("theoretical_thalf_metric"), definition = function(x) {
  name <- x %>% getName()
  subtype <- x@subtype
  if (subtype == "1cpt") {
    retValue <- subscriptOccurrence(name, "half\\.z", "\U00BD,z")
  } else if (subtype == "2cpt.dist") {
    retValue <- subscriptOccurrence(name, "half\\.dist", "\U00BD,dist")
  } else if (subtype == "2cpt.z") {
    retValue <- subscriptOccurrence(name, "half\\.z", "\U00BD,z")
  } else if (subtype == "2cpt.eff") {
    retValue <- subscriptOccurrence(name, "half\\.eff", "\U00BD,eff")
  } else {
    retValue <- name
  }
  return(retValue)
})
