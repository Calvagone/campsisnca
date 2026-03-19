
library(testthat)
library(dplyr)
library(campsis)

context("Test Auc metric")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))


test_that("Dataset 1 - AUC0-24 at day 1 (method 1)", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  auc1 <- AUC("Y", TimeWindow(0, 24)) %>% campsisnca::calculate(campsis)
  auc2 <- ncappcOutput(nonmem %>% timerange(0, 24), metric="AUClast")
  expect_equal(auc1@individual, auc2, tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 7 (method 1)", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  auc1 <- AUC("Y", TimeWindow(144, 168)) %>% campsisnca::calculate(campsis)
  auc2 <- ncappcOutput(nonmem %>% timerange(144, 168), metric="AUClast")
  expect_equal(auc1@individual, auc2, tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 1 (method 2)", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  auc1 <- AUC("Y", TimeWindow(0, 24), method=2) %>% campsisnca::calculate(campsis)
  auc2 <- ncappcOutput(nonmem %>% timerange(0, 24), metric="AUClast", method=2)
  expect_equal(auc1@individual, auc2, tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 7 (method 2)", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  auc1 <- AUC("Y", TimeWindow(144, 168), method=2) %>% campsisnca::calculate(campsis)
  auc2 <- ncappcOutput(nonmem %>% timerange(144, 168), metric="AUClast", method=2)
  expect_equal(auc1@individual, auc2, tolerance=1e-3)
})
