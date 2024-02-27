
library(testthat)
library(dplyr)
library(campsis)
context("Test Ctrough method")

testFolder <- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - Ctrough at steady state", {
  ds <- dataset1()
  ctrough1a <- Ctrough(ds$campsis, "Y") %>% campsisnca::calculate()
  ctrough1b <- Ctrough(ds$campsis, "Y", time=168) %>% campsisnca::calculate()
  expect_error(Ctrough(ds$campsis, "Y", time=169) %>% campsisnca::calculate(), msg="Could not find any sample at t=169")
  
  ctrough2 <- ncappcOutput(ds$nonmem, metric="Clast", doseType="ss", doseTime=144, Tau=24)
  expect_equal(ctrough1a@individual, ctrough2, tolerance=1e-3)
  expect_equal(ctrough1b@individual, ctrough2, tolerance=1e-3)
})

