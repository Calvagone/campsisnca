library(testthat)
library(dplyr)
library(campsis)

context("Test AUC metric (extra tests)")

test_that("AUC infinite (single dose)", {
  amount <- 1000
  wait_period <- 24 * 7 * 5 # 5 weeks

  model <- model_suite$testing$nonmem$advan4_trans4 %>% disable("IIV") %>%
    replace(Theta(name = "Q", value = 5)) %>%
    replace(Theta(name = "V3", value = 100))

  required <- thalf.2cpt.required()[!(thalf.2cpt.required() %in% c("DOSE", "TAU"))]

  dataset <- Dataset(1) %>%
    add(Bolus(time = 0, amount = amount)) %>%
    add(Observations(times = seq(0, wait_period, by = 0.1)))

  results <-  simulate(model = model, dataset = dataset, seed = 1, outvars = c("CP", required))

  spaghetti_plot(results, "CP")

  auc <- AUC("CP", method = 1) %>% campsisnca::calculate(results)
  auc@individual$value
  met <- metrics.2cpt(results %>% mutate(DOSE = amount, TAU = 24))
  met$AUC # = DOSE / CL
  expect_equal(auc@individual$value, met$AUC, tolerance = 1e-2)
})
