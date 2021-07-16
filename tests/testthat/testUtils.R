
# setwd("C:/prj/campsisnca/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsisnca/tests/")
# testFolder <<- "C:/prj/campsisnca/tests/testthat/"

validateNCA <- function(nmDataset, metric, method=1, AUCTimeRange=NULL) {
  if (method==1) {
    method_ <- "linear"
  } else if(method==2) {
    method_ <- "linearup-logdown"
  } else {
    stop("Either 1 or 2")
  }
  out <- ncappc::ncappc(
    obsFile=nmDataset,
    method=method_,
    onlyNCA=T, # To avoid note: Simulated data file, nca_simulation.1.npctab.dta.zip, is not found in the working directory.
    extrapolate=F,
    printOut=F,
    noPlot=T,
    AUCTimeRange=AUCTimeRange
  )
  retValue <- out$ncaOutput
  #cat(paste0(colnames(retValue), collapse=","))
  retValue <- retValue %>% dplyr::mutate(ID=as.numeric(ID)) %>% dplyr::arrange(ID) %>% dplyr::select(ID, dplyr::all_of(metric))
  return(retValue %>% tibble::as_tibble())
}

fixedSeed <- function() {
  return(1)
}

exportToNMDataset <- function(results, dataset, model) {
  # Retrieve ETA names from model
  etas <- (model@parameters %>% campsismod::select("omega"))@list %>% purrr::map_chr(~paste0("ETA_", .x@name))
  
  # Export CAMPSIS dataset to dataframe, remove all ETAS
  # Note: seed can be random as ETAS are removed
  nmDataset <- dataset %>% export(dest="mrgsolve", model=model, seed=fixedSeed()) %>% dplyr::select(-all_of(etas), -ARM, -DOSENO)
  
  # Merge CAMPSIS results and dataset to have kind of full dataset (dosing info + observations)
  nmDataset <- nmDataset %>% dplyr::left_join(results %>% dplyr::transmute(ID=id, TIME=time, EVID=0, DV=Y))
  
  # Restrict DV to 6 decimals
  return(nmDataset)
}

dataset1 <- function() {
  model <- getNONMEMModelTemplate(3,4)
  model <- model %>% add(InfusionDuration(1, rhs="5"))
  
  # 20 subjects
  dataset <- Dataset(20)
  
  # 4 infusions
  dataset <- dataset %>% add(Infusion(0, 100, 1))
  dataset <- dataset %>% add(Infusion(24, 100, 1))
  dataset <- dataset %>% add(Infusion(48, 100, 1))
  dataset <- dataset %>% add(Infusion(72, 100, 1))
  
  # Rich PK sampling on day 1 and day 4
  sampling <- c(0,1,2,3, 2,4,8,10,12,16,20,24)
  dataset <- dataset %>% add(Observations(times=c(sampling, sampling + 72)))
  
  # Simulate with CAMPSIS
  results <- model %>% simulate(dataset, dest="mrgsolve", seed=fixedSeed())
  
  # Return both the CAMPSIS output and the NONMEM dataset
  return(list(campsis=results, nonmem=results %>% exportToNMDataset(dataset=dataset, model=model)))
}