library(testthat)
library(dplyr)
library(campsis)
context("Test CAt and Ctrough methods")

TEST_FOLDER <- file.path(getwd(), test_path())
source(file.path(TEST_FOLDER, "test-utils.R"))

test_that("Dataset 1 - Ctrough at steady state", {
  ds <- dataset1()
  ctrough <- Ctrough("Y") %>% campsisnca::calculate(ds$campsis)
  last <- Last("Y") %>% campsisnca::calculate(ds$campsis) # Strictly identical
  concAt <- CAt("Y", time = 168) %>% campsisnca::calculate(ds$campsis)
  expect_error(CAt("Y", time = 169) %>% campsisnca::calculate(ds$campsis), msg = "Could not find any sample at t=169")

  expected <- ncappc_output(
    ds$nonmem,
    metric = "Clast",
    doseType = "ss",
    doseTime = 144,
    Tau = 24,
    reg_file = ncappc_reg_file("dataset1_ctrough_ss.csv")
  )
  expect_equal(ctrough@individual, expected, tolerance = 1e-3)
  expect_equal(last@individual, expected, tolerance = 1e-3)
  expect_equal(concAt@individual, expected, tolerance = 1e-3)

  expect_equal(ctrough %>% get_name(), "Ctrough")
  expect_equal(last %>% get_name(), "Last value")
  expect_equal(concAt %>% get_name(), "Conc")
})
