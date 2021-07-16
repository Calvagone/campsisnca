library(testthat)
library(dplyr)
library(campsis)
context("Test cavg method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - cavg at steady state", {
  ds <- dataset1()
  cavg1 <- cavg(ds$campsis %>% timerange(144, 168), "Y")
  # SS info needed to have Cavg + extrapolate=TRUE (I have no idea why...)
  cavg2 <- validateNCA(ds$nonmem, metric="Cavg", doseType="ss", doseTime=144, Tau=24, extrapolate=TRUE)
  expect_equal(cavg1, cavg2, tolerance=1e-3)
})
