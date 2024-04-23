
library(testthat)
library(dplyr)
library(campsis)
context("Test CAt and Ctrough methods")

testFolder <- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - Ctrough at steady state", {
  ds <- dataset1()
  ctrough <- Ctrough(ds$campsis, "Y") %>% campsisnca::calculate()
  clast <- Clast(ds$campsis, "Y") %>% campsisnca::calculate()
  concAt <- CAt(ds$campsis, "Y", time=168) %>% campsisnca::calculate()
  expect_error(CAt(ds$campsis, "Y", time=169) %>% campsisnca::calculate(), msg="Could not find any sample at t=169")
  
  expected <- ncappcOutput(ds$nonmem, metric="Clast", doseType="ss", doseTime=144, Tau=24)
  expect_equal(ctrough@individual, expected, tolerance=1e-3)
  expect_equal(clast@individual, expected, tolerance=1e-3)
  expect_equal(concAt@individual, expected, tolerance=1e-3)
  
  expect_equal(ctrough %>% getName(), "Ctrough")
  expect_equal(clast %>% getName(), "Clast")
  expect_equal(concAt %>% getName(), "Conc")
})

