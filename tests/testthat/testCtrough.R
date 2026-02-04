
library(testthat)
library(dplyr)
library(campsis)
context("Test CAt and Ctrough methods")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

test_that("Dataset 1 - Ctrough at steady state", {
  ds <- dataset1()
  ctrough <- Ctrough("Y") %>% campsisnca::calculate(ds$campsis)
  last <- Last("Y") %>% campsisnca::calculate(ds$campsis) # Strictly identical
  concAt <- CAt("Y", time=168) %>% campsisnca::calculate(ds$campsis)
  expect_error(CAt("Y", time=169) %>% campsisnca::calculate(ds$campsis),
               msg="Could not find any sample at t=169")
  
  expected <- ncappcOutput(ds$nonmem, metric="Clast", doseType="ss", doseTime=144, Tau=24)
  expect_equal(ctrough@individual, expected, tolerance=1e-3)
  expect_equal(last@individual, expected, tolerance=1e-3)
  expect_equal(concAt@individual, expected, tolerance=1e-3)
  
  expect_equal(ctrough %>% getName(), "Ctrough")
  expect_equal(last %>% getName(), "Last value")
  expect_equal(concAt %>% getName(), "Conc")
})

