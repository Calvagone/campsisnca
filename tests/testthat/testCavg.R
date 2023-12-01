library(testthat)
library(dplyr)
library(campsis)
context("Test Cavg metric")

testFolder <- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - cavg at steady state", {
  ds <- dataset1()
  cavg1 <- Cavg(ds$campsis %>% timerange(144, 168), "Y") %>% campsisnca::calculate()
  # SS info needed to have Cavg + extrapolate=TRUE (I have no idea why...)
  cavg2 <- ncappcOutput(ds$nonmem, metric="Cavg", doseType="ss", doseTime=144, Tau=24, extrapolate=TRUE)
  expect_equal(cavg1@individual, cavg2, tolerance=1e-3)
})
