
library(testthat)
library(dplyr)
library(campsis)
context("Compute theoritical NCA metrics from PK parameters")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Run from modelling", {
  x <- data.frame(id=1, time=0, TAU=12, DOSE=10000, CL=48, V2=208, Q=18, V3=684, KA=3.3)
  metrics <- metrics.2cpt(x)
  
  tol <- 1e-6
  
  expect_equal(metrics$K20, 0.230769231, tolerance=tol)
  expect_equal(metrics$K23, 0.086538462, tolerance=tol)
  expect_equal(metrics$K32, 0.026315789, tolerance=tol)
  
  expect_equal(metrics$A, 1, tolerance=tol)
  expect_equal(metrics$B, 0.343623482, tolerance=tol)
  expect_equal(metrics$C, 0.006072874, tolerance=tol)
  expect_equal(metrics$DETER, 0.093785599, tolerance=tol)
  
  expect_equal(metrics$ALPHA, 0.324933914, tolerance=tol)
  expect_equal(metrics$BETA, 0.018689568, tolerance=tol)
  
  expect_equal(metrics$COEFF_A, 51.99984773, tolerance=tol)
  expect_equal(metrics$COEFF_B, 1.204050264, tolerance=tol)
  expect_equal(metrics$COEFF_C, -53.20389799, tolerance=tol)
  
  expect_equal(metrics$AUC, 208.3333, tolerance=tol)
  expect_equal(metrics$AUC_Z_1, 160.0321, tolerance=tol)
  expect_equal(metrics$AUC_Z, 64.42365, tolerance=tol)
  expect_equal(metrics$AUC_KA, -16.12239, tolerance=tol)
  
  expect_equal(metrics$AUC_Z_1_pc, 71.29783, tolerance=tol)
  expect_equal(metrics$AUC_Z_pc, 28.70217, tolerance=tol)
  
  expect_equal(metrics$THALF, 2.133194, tolerance=tol)
  expect_equal(metrics$THALF_Z, 37.08738, tolerance=tol)
  expect_equal(metrics$THALF_EFF, 12.1658, tolerance=tol)
  
})

test_that("Get half-life parameters from CAMPSIS 2-cpt model", {
  model <- getNONMEMModelTemplate(4,4)
  model <- model %>% campsismod::replace(Theta(name="Q", value=5))
  model <- model %>% campsismod::replace(Theta(name="V3", value=100))
  halfLifeRequiredVars <- c("CL", "V2", "Q", "V3", "KA")
  
  dataset <- Dataset(1)
  dataset <- dataset %>% add(Bolus(time=(0:13)*24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,21*24)))
  
  # Simulate with CAMPSIS
  results <- model %>% disable("IIV") %>% simulate(dataset, dest="mrgsolve", seed=1, outvars=c("CP", halfLifeRequiredVars))
  
  shadedPlot(results, "CP") + ggplot2::scale_y_log10()
  
  metrics <- metrics.2cpt(results %>% mutate(DOSE=1000, TAU=24))
  metrics$THALF_EFF
  expect_equal(metrics$THALF_EFF, 23.97703, tolerance=1e-5)
  nca <- ncappcOutput(exportToNMDataset(results, dataset, model), metric=NULL, doseType="ss", doseTime=24*14, Tau=24, extrapolate=TRUE)
})

test_that("Get half-life parameters from CAMPSIS 1-cpt model", {
  model <- getNONMEMModelTemplate(2,1)

  dataset <- Dataset(1)
  dataset <- dataset %>% add(Bolus(time=(0:13)*24, amount=1000, compartment=1))
  dataset <- dataset %>% add(Observations(times=seq(0,21*24)))
  
  # Simulate with CAMPSIS
  results <- model %>% disable("IIV") %>% simulate(dataset, dest="mrgsolve", seed=1, outvars=c("CP", "K"))
  
  shadedPlot(results, "CP") + ggplot2::scale_y_log10()
  
  # K = LAMBDA_Z
  firstRow <- results %>% slice(1)
  firstRow$K
  
  # Validation 1: OK
  linearMod <- lm(log(CP) ~ time, data=results %>% timerange(min=15*24, max=16*24))
  linearMod$coefficients
  expect_equal(-linearMod$coefficients[["time"]], firstRow$K, tolerance=1e-3)
  
  # Validation 2: NOK
  nca <- ncappcOutput(exportToNMDataset(results, dataset, model), metric=NULL, doseType="ss", doseTime=24*14, Tau=24, extrapolate=TRUE)
  nca$Lambda_z
  
  # Validation 3: OK
  calva_nca <- (CalvaNCA::CalvaNCA_plasma(obsFile=exportToNMDataset(results, dataset, model)))$ncaOutput
  expect_equal(calva_nca$Lambda_z, firstRow$K, tolerance=1e-3)

  # Validation 4: OK
  metrics <- NonCompart::sNCA(x=results$time, y=results$CP, dose=1000, adm="Bolus")
  expect_equal(metrics[["LAMZ"]], firstRow$K, tolerance=1e-3)
})
