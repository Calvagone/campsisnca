
library(testthat)
library(dplyr)
library(campsis)
context("Compute theoritical NCA metrics from PK parameters")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Check errors are well detected", {
  x <- data.frame(ID=1, TIME=0, TAU=12, DOSE=10000, CL=48, V2=208, Q=18, V3=684, K=3.3)
  expect_error(metrics.2cpt(x), regexp="Missing columns in x: KA")
  
  x <- data.frame(ID=1, TIME=0, TAU=12, DOSE=10000, CL=48, V2=208, Q=18, V3=684, K=3.3)
  expect_error(metrics.2cpt(x, map=c(KA="K", V4="TAU")), regexp="Unnecessary keys detected in map vector: V4")
})

test_that("Run 017F Pop1", {
  x <- data.frame(ID=1, TIME=0, TAU=12, DOSE=10000, CL=48, V2=208, Q=18, V3=684, KA=3.3)
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
  
  expect_equal(metrics$THALF_D, 2.133194, tolerance=tol)
  expect_equal(metrics$THALF_Z, 37.08738, tolerance=tol)
  expect_equal(metrics$THALF_EFF, 12.1658, tolerance=tol)
  
})

test_that("Run 017F Pop2", {
  x <- data.frame(ID=1, TIME=0, TAU=12, DOSE=10000, CL=48, V2=209, Q=18, V3=650, KA=2.1)
  metrics <- metrics.2cpt(x)
  
  tol <- 1e-6
  
  expect_equal(metrics$K20, 0.229665072, tolerance=tol)
  expect_equal(metrics$K23, 0.086124402, tolerance=tol)
  expect_equal(metrics$K32, 0.027692308, tolerance=tol)
  
  expect_equal(metrics$A, 1, tolerance=tol)
  expect_equal(metrics$B, 0.343481781, tolerance=tol)
  expect_equal(metrics$C, 0.006359956, tolerance=tol)
  expect_equal(metrics$DETER, 0.092539911, tolerance=tol)
  
  expect_equal(metrics$ALPHA, 0.323842757, tolerance=tol)
  expect_equal(metrics$BETA, 0.019639024, tolerance=tol)
  
  expect_equal(metrics$COEFF_A, 55.07309134, tolerance=tol)
  expect_equal(metrics$COEFF_B, 1.278623681, tolerance=tol)
  expect_equal(metrics$COEFF_C, -56.35171502, tolerance=tol)
  
  expect_equal(metrics$AUC, 208.3333, tolerance=tol)
  expect_equal(metrics$AUC_Z_1, 170.061210797, tolerance=tol)
  expect_equal(metrics$AUC_Z, 65.106272548, tolerance=tol)
  expect_equal(metrics$AUC_KA, -26.834150012, tolerance=tol)
  
  expect_equal(metrics$AUC_Z_1_pc, 72.314934181, tolerance=tol)
  expect_equal(metrics$AUC_Z_pc, 27.685065819, tolerance=tol)
  
  expect_equal(metrics$THALF_D, 2.14038192, tolerance=tol)
  expect_equal(metrics$THALF_Z, 35.294379356, tolerance=tol)
  expect_equal(metrics$THALF_EFF, 11.319087929, tolerance=tol)
  
})

test_that("Get half-life parameters from CAMPSIS 2-cpt model", {
  model <- model_suite$testing$nonmem$advan4_trans4
  model <- model %>% campsismod::replace(Theta(name="Q", value=5))
  model <- model %>% campsismod::replace(Theta(name="V3", value=100))
  halfLifeRequiredVars <- thalf.2cpt.required()[!(thalf.2cpt.required() %in% c("DOSE", "TAU"))]
  
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
  model <- model_suite$testing$nonmem$advan2_trans1

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
  linearMod <- lm(log(CP) ~ TIME, data=results %>% campsisnca::timerange(min=15*24, max=16*24))
  linearMod$coefficients
  expect_equal(-linearMod$coefficients[["TIME"]], firstRow$K, tolerance=1e-3)
  
  # Validation 2: NOK
  nca <- ncappcOutput(exportToNMDataset(results, dataset, model), metric=NULL, doseType="ss", doseTime=24*14, Tau=24, extrapolate=TRUE)
  nca$Lambda_z
  
  # Validation 3: OK
  # calva_nca <- (CalvaNCA::CalvaNCA_plasma(obsFile=exportToNMDataset(results, dataset, model)))$ncaOutput
  # expect_equal(calva_nca$Lambda_z, firstRow$K, tolerance=1e-3)

  # Validation 4: OK
  # metrics <- NonCompart::sNCA(x=results$TIME, y=results$CP, dose=1000, adm="Bolus")
  # expect_equal(metrics[["LAMZ"]], firstRow$K, tolerance=1e-3)
})
