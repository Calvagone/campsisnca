library(testthat)
library(dplyr)
library(campsis)
context("Test Cavg metric")

TEST_FOLDER <-  file.path(getwd(), test_path())
source(file.path(TEST_FOLDER, "test-utils.R"))

test_that("Dataset 1 - cavg at steady state", {
  ds <- dataset1()
  cavg1 <- Cavg("Y", TimeWindow(144, 168)) %>% campsisnca::calculate(ds$campsis)
  # SS info needed to have Cavg + extrapolate=TRUE (I have no idea why...)
  cavg2 <- ncappc_output(ds$nonmem, metric="Cavg", doseType="ss", doseTime=144, Tau=24, extrapolate=TRUE,
    reg_file=ncappc_reg_file("dataset1_cavg_ss.csv"))
  expect_equal(cavg1@individual, cavg2, tolerance=1e-3)
})
