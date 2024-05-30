
# setwd("C:/prj/campsisnca/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsisnca/tests/")
# testFolder <- "C:/prj/campsisnca/tests/testthat/"

overwriteNonRegressionFiles <- FALSE
testFolder <- ""

generateData1 <- function() {
  rich_sampling <- c(0,1,2,4,6,8,12,16,24)
  day1 <- rich_sampling
  day2day6 <- c(2,3,4,5,6)*24
  day7 <- rich_sampling + 6*24
  day8day10 <- c(8,9,10)*24
  
  ds <- Dataset(200) %>%
    add(Bolus(time=(0:6)*24, amount=1000)) %>%
    add(Observations(times=c(day1, day2day6, day7, day8day10))) %>%
    add(Covariate("BW", UniformDistribution(50,100)))
  
  model <- model_suite$testing$nonmem$advan4_trans4
  cl <- model %>% campsismod::find(Equation("CL"))
  model <- model %>% campsismod::replace(Equation("CL", paste0(cl@rhs, "*pow(BW/70, 0.75)")))
  
  # It should not matter if mrgsolve is used instead of rxode2
  # Residual variability is now generated with base R on line 32
  campsis <- model %>% simulate(dataset=ds, dest="mrgsolve", seed=1, outvars=c("BW", "CL", "V2", "Q", "V3", "KA"))
  
  # Generate residual variability with base R (see issue #52)
  set.seed(1)
  campsis$OBS_CP <- campsis$CP*(rnorm(n=nrow(campsis), mean=0, sd=sqrt(0.025)) + 1)
  campsis$Y <- campsis$OBS_CP
  
  # eps <- campsis$OBS_CP / campsis$CP - 1
  # eps <- eps[is.finite(eps)]
  # var(eps)
  
  return(campsis)
}

convertMethod <- function(method) {
  if (method==1) {
    method_ <- "linear"
  } else if(method==2) {
    method_ <- "linearup-logdown"
  } else {
    stop("Either 1 or 2")
  }
  return(method_)
}

standardiseOutput <- function(ncaOutput, metric) {
  ncaOutput <- ncaOutput %>% dplyr::mutate(ID=as.numeric(ID)) %>% dplyr::arrange(ID)
  if (!is.null(metric)) {
    ncaOutput <- ncaOutput %>% dplyr::select(ID, dplyr::all_of(metric))
    ncaOutput <- ncaOutput %>% rename(id=ID)
    ncaOutput <- ncaOutput %>% rename_at(.vars=metric, .funs=~"value")
  }
  return(ncaOutput %>% tibble::as_tibble())
}

# THis is needed because of Cavg, Cmin funtions etc
# that make the ncappc package not to work properly
# Test it with the global variable: Cavg=0
detachCampsisNCA <- function() {
  if("campsisnca" %in% (.packages())){
    detach("package:campsisnca", unload=TRUE) 
  }
}

ncappcOutput <- function(nmDataset, metric=NULL, method=1, doseType="ns", doseTime=NULL, Tau=NULL, extrapolate=FALSE) {
  detachCampsisNCA()
  out <- ncappc::ncappc(
    obsFile=nmDataset,
    method=convertMethod(method),
    doseType=doseType,
    doseTime=doseTime,
    Tau=Tau,
    onlyNCA=T, # To avoid note: Simulated data file, nca_simulation.1.npctab.dta.zip, is not found in the working directory.
    extrapolate=extrapolate,
    printOut=F,
    noPlot=T
  )
  return(standardiseOutput(out$ncaOutput, metric))
}

# calvaNCAOutput <- function(nmDataset, metric=NULL, method=1, doseType="ns", doseTime=NULL, Tau=NULL, AUCTimeRange=NULL) {
#   detachCampsisNCA()
#   out <- CalvaNCA::CalvaNCA_plasma(
#     obsFile=nmDataset, 
#     method=convertMethod(method),
#     doseType=doseType,
#     doseTime=doseTime,
#     Tau=Tau,
#     AUCTimeRange=AUCTimeRange)
#   return(standardiseOutput(out$ncaOutput, metric))
# }

exportToNMDataset <- function(results, dataset, model, seed=1) {
  # Retrieve ETA names from model
  etas <- (model@parameters %>% campsismod::select("omega"))@list %>% purrr::map_chr(~paste0("ETA_", .x@name))
  
  # Export CAMPSIS dataset to dataframe, remove all ETAS
  # Note: seed can be random as ETAS are removed
  nmDataset <- dataset %>% export(dest="mrgsolve", model=model, seed=seed) %>% dplyr::select(-all_of(etas), -ARM, -DOSENO)
  
  # Merge CAMPSIS results and dataset to have kind of full dataset (dosing info + observations)
  nmDataset <- nmDataset %>% dplyr::left_join(results %>% dplyr::transmute(ID=ID, TIME=TIME, EVID=0, DV=Y))
  
  # Restrict DV to 6 decimals
  return(nmDataset)
}

#' Test there is no regression in the simulated output.
#' 
#' @param data newly generated data
#' @param output variables to compare, if NULL, all column names are compared
#' @param filename reference file
#' @param times filter reference results on specific times, NULL by default
#' @importFrom tibble as_tibble
outputRegressionTest <- function(data, output=NULL, filename) {
  if (is.null(output)) {
    output <- colnames(data)
  }
  
  results1 <- data %>%
    dplyr::select(dplyr::all_of(output)) %>%
    dplyr::mutate_if(is.numeric, round, digits=2)
  
  file <- paste0(testFolder, "non_regression/", paste0(filename, ".csv"))
  
  if (overwriteNonRegressionFiles) {
    write.table(results1, file=file, sep=",", row.names=FALSE)
  }
  
  results2 <- read.csv(file=file) %>% tibble::as_tibble()
  if ("discrete_value" %in% colnames(results2)) {
    # Otherwise, read as logical if only 2 states
    results2 <- results2 %>%
      dplyr::mutate(discrete_value=as.character(discrete_value))
  }
  expect_equal(results1, results2)
}

gtTableRegressionTest <- function(gttable, filename) {
  
  file <- paste0(testFolder, "non_regression/", paste0(filename, ".html"))
  
  gttable %>%
    gt::gtsave(filename=file)
}

dataset1 <- function(seed=1, reload=TRUE) {
  if (reload) {
    library(campsisnca)
  }
  model <- model_suite$testing$nonmem$advan3_trans4
  model <- model %>% add(InfusionDuration(1, rhs="5"))
  
  # 20 subjects
  dataset <- Dataset(20)
  
  # 2-weeks infusions
  dataset <- dataset %>% add(Infusion(time=(0:13)*24, amount=100, compartment=1))

  # Rich PK sampling on day 1 and day 7
  sampling <- c(0,1,2,3,4,8,10,12,16,20,24)
  dataset <- dataset %>% add(Observations(times=c(sampling, sampling + ((7-1)*24))))
  
  # Simulate with CAMPSIS
  results <- model %>% simulate(dataset, dest="mrgsolve", seed=seed)
  
  #spaghettiPlot(results, "CP")
  
  # Return both the CAMPSIS output and the NONMEM dataset
  return(list(campsis=results, nonmem=results %>% exportToNMDataset(dataset=dataset, model=model, seed=seed)))
}
