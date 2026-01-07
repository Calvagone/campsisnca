
library(testthat)
library(dplyr)
library(campsis)
context("Test Cmin method")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

test_that("Dataset 1 - cmin at steady state", {
  ds <- dataset1()
  cmin1 <- Cmin(ds$campsis %>% timerange(144, 168), "Y") %>% campsisnca::calculate()
  cmin2 <- ncappcOutput(ds$nonmem, metric="Cmin", doseType="ss", doseTime=144, Tau=24) # SS info needed to have Cmin
  expect_equal(cmin1@individual, cmin2, tolerance=1e-3)
})

