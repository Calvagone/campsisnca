#_______________________________________________________________________________
#----                          thalf_metric class                             ----
#_______________________________________________________________________________

validateThalfMetric <- function(object) {
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
    return("t1/2")
  } else if (subtype == "2cpt.dist") {
    return("t1/2dist")
  } else if (subtype == "2cpt.z") {
    return("t1/2z")
  } else if (subtype == "2cpt.eff") {
    return("t1/2eff")
  } else {
    stop(paste0("Unknown subtype ", subtype))
  }
}

#' 
#' Thalf metric class.
#' 
#' @export
setClass(
  "thalf_metric",
  representation(
    map = "character",
    subtype = "character"
  ),
  contains="nca_metric",
  validity=validateThalfMetric
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
Thalf.1cpt <- function(x=NULL, map=NULL, name=NULL, unit=NULL) {
  x <- processDataframe(x)
  map <- checkMap(map, thalf.1cpt=TRUE)
  subtype <- "1cpt"
  name <- if (is.null(name)) getDefaultTHalfName(subtype) else name
  unit <- processUnit(unit)
  return(new("thalf_metric", x=x, variable=NA, map=map, subtype=subtype, name=name, unit=unit))
}

#' 
#' Theoretical distribution half life for a 2-compartment model.
#' 
#' @inheritParams metricsParams
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @export
Thalf.2cpt.dist <- function(x=NULL, map=NULL, name=NULL, unit=NULL) {
  x <- processDataframe(x)
  map <- checkMap(map, thalf.1cpt=FALSE)
  subtype <- "2cpt.dist"
  name <- if (is.null(name)) getDefaultTHalfName(subtype) else name
  unit <- processUnit(unit)
  return(new("thalf_metric", x=x, variable=as.character(NA), map=map, subtype=subtype, name=name, unit=unit))
}

#' 
#' Theoretical elimination half life for a 2-compartment model.
#' 
#' @inheritParams metricsParams
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @export
Thalf.2cpt.z <- function(x=NULL, map=NULL, name=NULL, unit=NULL) {
  x <- processDataframe(x)
  map <- checkMap(map, thalf.1cpt=FALSE)
  subtype <- "2cpt.z"
  name <- if (is.null(name)) getDefaultTHalfName(subtype) else name
  unit <- processUnit(unit)
  return(new("thalf_metric", x=x, variable=as.character(NA), map=map, subtype=subtype, name=name, unit=unit))
}

#' 
#' Theoretical effective half life for a 2-compartment model.
#' 
#' @inheritParams metricsParams
#' @param map character vector used for column mapping, keys to be chosen among: DOSE, TAU, CL, V2, Q, V3, KA
#' @export
Thalf.2cpt.eff <- function(x=NULL, map=NULL, name=NULL, unit=NULL) {
  x <- processDataframe(x)
  map <- checkMap(map, thalf.1cpt=FALSE)
  subtype <- "2cpt.eff"
  name <- if (is.null(name)) getDefaultTHalfName(subtype) else name
  unit <- processUnit(unit)
  return(new("thalf_metric", x=x, variable=as.character(NA), map=map, subtype=subtype, name=name, unit=unit))
}

#_______________________________________________________________________________
#----                            calculate                                  ----
#_______________________________________________________________________________

#' @rdname calculate
setMethod("calculate", signature=c("thalf_metric", "numeric"), definition=function(object, level, ...) {
  subtype <- object@subtype

  if (subtype == "1cpt") {
    ind <- metrics.1cpt(object@x, map=object@map)
    ind <- ind %>% dplyr::transmute(id=id, value=THALF)
 
   } else if (subtype %>% startsWith("2cpt")) {
    ind <- metrics.2cpt(object@x, map=object@map)
    
    if (subtype == "2cpt.dist") {
      ind <- ind %>% dplyr::transmute(id=id, value=THALF_D)
    } else if (subtype == "2cpt.z") {
      ind <- ind %>% dplyr::transmute(id=id, value=THALF_Z)
    } else if (subtype == "2cpt.eff") {
      ind <- ind %>% dplyr::transmute(id=id, value=THALF_EFF)
    }
  } else {
    stop(paste0("Unknown subtype ", subtype))
  }
  object@individual <- ind
  return(object %>% summariseIndividualData(level=level))    
})
