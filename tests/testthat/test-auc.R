library(testthat)
library(dplyr)
library(campsis)

context("Test Auc metric")

TEST_FOLDER <- file.path(getwd(), test_path())
source(file.path(TEST_FOLDER, "test-utils.R"))


test_that("Dataset 1 - AUC0-24 at day 1 (method 1)", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  auc1 <- AUC("Y", TimeWindow(0, 24)) %>% campsisnca::calculate(campsis)
  auc2 <- ncappc_output(
    nonmem %>% timerange(0, 24),
    metric = "AUClast",
    reg_file = ncappc_reg_file("dataset1_auc0_24_day1_m1.csv")
  )
  expect_equal(auc1@individual, auc2, tolerance = 1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 7 (method 1)", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  auc1 <- AUC("Y", TimeWindow(144, 168)) %>% campsisnca::calculate(campsis)
  auc2 <- ncappc_output(
    nonmem %>% timerange(144, 168),
    metric = "AUClast",
    reg_file = ncappc_reg_file("dataset1_auc0_24_day7_m1.csv")
  )
  expect_equal(auc1@individual, auc2, tolerance = 1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 1 (method 2)", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  auc1 <- AUC("Y", TimeWindow(0, 24), method = 2) %>% campsisnca::calculate(campsis)
  auc2 <- ncappc_output(
    nonmem %>% timerange(0, 24),
    metric = "AUClast",
    method = 2,
    reg_file = ncappc_reg_file("dataset1_auc0_24_day1_m2.csv")
  )
  expect_equal(auc1@individual, auc2, tolerance = 1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 7 (method 2)", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  auc1 <- AUC("Y", TimeWindow(144, 168), method = 2) %>% campsisnca::calculate(campsis)
  auc2 <- ncappc_output(
    nonmem %>% timerange(144, 168),
    metric = "AUClast",
    method = 2,
    reg_file = ncappc_reg_file("dataset1_auc0_24_day7_m2.csv")
  )
  expect_equal(auc1@individual, auc2, tolerance = 1e-3)
})
