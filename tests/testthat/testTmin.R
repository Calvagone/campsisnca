
library(testthat)
library(dplyr)
library(campsis)
context("Test tmin method")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

test_that("Dataset 1 - cmin at steady state", {
  ds <- dataset1()
  tmin1 <- Tmin("Y", TimeWindow(144, 168), rebase=FALSE) %>% campsisnca::calculate(ds$campsis)
  tmin2 <- ncappc_output(ds$nonmem, metric="Tmin", doseType="ss", doseTime=144, Tau=24,
    reg_file=ncappc_reg_file("dataset1_tmin_ss.csv")) # SS info needed to have Tmin
  expect_equal(tmin1@individual, tmin2, tolerance=1e-3)
})

