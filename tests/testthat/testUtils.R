
# setwd("C:/prj/campsisnca/")
# roxygen2::roxygenise()
# setwd("C:/prj/campsisnca/tests/")
# testFolder <<- "C:/prj/campsisnca/tests/testthat/"

validateNCA <- function(nmDataset, metric=NULL, method=1, doseType="ns", doseTime=NULL, Tau=NULL, extrapolate=FALSE) {
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
    doseType=doseType,
    doseTime=doseTime,
    Tau=Tau,
    onlyNCA=T, # To avoid note: Simulated data file, nca_simulation.1.npctab.dta.zip, is not found in the working directory.
    extrapolate=extrapolate,
    printOut=F,
    noPlot=T
  )
  retValue <- out$ncaOutput
  retValue <- retValue %>% dplyr::mutate(ID=as.numeric(ID)) %>% dplyr::arrange(ID)
  if (!is.null(metric)) {
    retValue <- retValue %>% dplyr::select(ID, dplyr::all_of(metric))
    retValue <- retValue %>% rename(id=ID)
    retValue <- retValue %>% rename_at(.vars=metric, .funs=function(.x) {
      if (.x == "AUClast") {
        return("auc")
      } else if (.x == "Cmax") {
        return("cmax")
      } else if (.x == "Tmax") {
        return("tmax")
      } else if (.x == "Cmin") {
        return("cmin")
      } else if (.x == "Tmin") {
        return("tmin")
      } else if (.x == "Cavg") {
        return("cavg")
      } else {
        stop(paste0("Metric ", x, " not encoded"))
      }
    })
  }
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
  
  # 2-weeks infusions
  dataset <- dataset %>% add(Infusion(time=(0:13)*24, amount=100, compartment=1))

  # Rich PK sampling on day 1 and day 7
  sampling <- c(0,1,2,3, 2,4,8,10,12,16,20,24)
  dataset <- dataset %>% add(Observations(times=c(sampling, sampling + ((7-1)*24))))
  
  # Simulate with CAMPSIS
  results <- model %>% simulate(dataset, dest="mrgsolve", seed=fixedSeed())
  
  #spaghettiPlot(results, "CP")
  
  # Return both the CAMPSIS output and the NONMEM dataset
  return(list(campsis=results, nonmem=results %>% exportToNMDataset(dataset=dataset, model=model)))
}