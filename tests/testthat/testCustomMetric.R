
library(testthat)
library(dplyr)
library(campsis)
context("Test custom metric")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

test_that("Auto-replace of known NCA metrics works as expected", {
  custom1 <- CustomMetric(fun=~Cmax > 12, name="Cmax > 12", categorical=TRUE)
  custom1 <- custom1 %>%
    replaceAll(pattern=NCAMetrics(), replacement="auto")
  expect_equal(custom1@custom_function, "rlang::as_function(~iValue(Cmax(),.x,.y) > 12)")
  
  custom2 <- CustomMetric(fun=~Cmax > 12 & Ctrough < 5, name="Cmax > 12 & Ctrough < 5", categorical=TRUE)
  custom2 <- custom2 %>%
    replaceAll(pattern=NCAMetrics(), replacement="auto")
  expect_equal(custom2@custom_function, "rlang::as_function(~iValue(Cmax(),.x,.y) > 12 & iValue(Ctrough(),.x,.y) < 5)")
  
  custom3 <- CustomMetricTbl(fun=~max(.x$Y), name="Cmax > 12", categorical=TRUE)
  custom3 <- custom3 %>%
    replaceAll(pattern=NCAMetrics(), replacement="auto")
  expect_equal(custom3@custom_function, "rlang::as_function(~max(.x$Y))") # I.e. no change
  
  custom4 <- CustomMetric(fun=~AUC > 100, name="AUC > 100", categorical=TRUE)
  custom4 <- custom4 %>%
    replaceAll(pattern=NCAMetrics(), replacement="auto")
  expect_equal(custom4@custom_function, "rlang::as_function(~iValue(AUC(),.x,.y) > 100)")
})


test_that("Dataset 1 - Cmax at day 1 via custom metric", {
  ds <- dataset1()
  
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  
  customFun <- function(time, value) {
    max(value)
  }
  
  cmax1a <- Cmax("Y", TimeWindow(0, 24)) %>% campsisnca::calculate(campsis)
  cmax1b <- CustomMetric("Y", TimeWindow(0, 24), fun=~max(.y)) %>% campsisnca::calculate(campsis) # Lambda
  cmax1c <- CustomMetric("Y", TimeWindow(0, 24), fun=customFun) %>% campsisnca::calculate(campsis) # Normal function
  cmax1d <- CustomMetricTbl(TimeWindow(0, 24), fun=~max(.x$Y)) %>% campsisnca::calculate(campsis) # Lambda using tibble
  
  cmax2 <- ncappcOutput(nonmem %>% timerange(0, 24), metric="Cmax")
  
  expect_equal(cmax1a@individual, cmax2, tolerance=1e-3)
  expect_equal(cmax1b@individual, cmax2, tolerance=1e-3)
  expect_equal(cmax1c@individual, cmax2, tolerance=1e-3)
  expect_equal(cmax1d@individual, cmax2, tolerance=1e-3)
})
