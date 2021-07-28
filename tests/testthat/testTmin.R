
library(testthat)
library(dplyr)
library(campsis)
context("Test tmin method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - cmin at steady state", {
  ds <- dataset1()
  tmin1 <- Tmin(ds$campsis %>% timerange(144, 168), "Y") %>% campsisnca::calculate()
  tmin2 <- ncappcOutput(ds$nonmem, metric="Tmin", doseType="ss", doseTime=144, Tau=24) # SS info needed to have Tmin
  expect_equal(tmin1@individual, tmin2, tolerance=1e-3)
})

