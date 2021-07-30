
# setwd("C:/prj/campsisnca/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsisnca/tests/")
# testFolder <<- "C:/prj/campsisnca/tests/testthat/"

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

calvaNCAOutput <- function(nmDataset, metric=NULL, method=1, doseType="ns", doseTime=NULL, Tau=NULL, AUCTimeRange=NULL) {
  detachCampsisNCA()
  out <- CalvaNCA::CalvaNCA_plasma(
    obsFile=nmDataset, 
    method=convertMethod(method),
    doseType=doseType,
    doseTime=doseTime,
    Tau=Tau,
    AUCTimeRange=AUCTimeRange)
  return(standardiseOutput(out$ncaOutput, metric))
}

exportToNMDataset <- function(results, dataset, model, seed=1) {
  # Retrieve ETA names from model
  etas <- (model@parameters %>% campsismod::select("omega"))@list %>% purrr::map_chr(~paste0("ETA_", .x@name))
  
  # Export CAMPSIS dataset to dataframe, remove all ETAS
  # Note: seed can be random as ETAS are removed
  nmDataset <- dataset %>% export(dest="mrgsolve", model=model, seed=seed) %>% dplyr::select(-all_of(etas), -ARM, -DOSENO)
  
  # Merge CAMPSIS results and dataset to have kind of full dataset (dosing info + observations)
  nmDataset <- nmDataset %>% dplyr::left_join(results %>% dplyr::transmute(ID=id, TIME=time, EVID=0, DV=Y))
  
  # Restrict DV to 6 decimals
  return(nmDataset)
}

dataset1 <- function(seed=1, reload=TRUE) {
  if (reload) {
    library(campsisnca)
  }
  model <- getNONMEMModelTemplate(3,4)
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