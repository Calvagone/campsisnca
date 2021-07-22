
library(testthat)
library(dplyr)
library(campsis)
context("Test cmin method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - cmin at steady state", {
  ds <- dataset1()
  cmin1 <- cmin(ds$campsis %>% timerange(144, 168), "Y")
  cmin2 <- ncappcOutput(ds$nonmem, metric="Cmin", doseType="ss", doseTime=144, Tau=24) # SS info needed to have Cmin
  expect_equal(cmin1, cmin2, tolerance=1e-3)
})

