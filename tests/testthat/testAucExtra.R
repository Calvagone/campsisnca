library(testthat)
library(dplyr)
library(campsis)

context("Test Auc metric (extra tests)")

test_that("AUC infinite (single dose)", {
  amount <- 1000
  wait_period <- 24*7*5 # 5 weeks
  model <- model_library$advan4_trans4 %>% disable("IIV")
  model <- model %>% campsismod::replace(Theta(name="Q", value=5))
  model <- model %>% campsismod::replace(Theta(name="V3", value=100))
  required <- thalf.2cpt.required()[!(thalf.2cpt.required() %in% c("DOSE", "TAU"))]
  dataset <- Dataset(1)
  dataset <- dataset %>% add(Bolus(time=0, amount=amount))
  dataset <- dataset %>% add(Observations(times=seq(0, wait_period, by=0.1)))
  results <- model %>% simulate(dataset=dataset, dest="RxODE", seed=1, outvars=c("CP", required))
  spaghettiPlot(results, "CP")
  auc <- Auc(results, "CP", method=1) %>% campsisnca::calculate()
  auc@individual$value
  met <- metrics.2cpt(results %>% mutate(DOSE=amount, TAU=24))
  met$AUC # = DOSE / CL
  expect_equal(auc@individual$value, met$AUC, tolerance=1e-2)
})
