
library(testthat)
library(dplyr)
library(campsis)

context("Test Auc metric")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))


test_that("Dataset 1 - AUC0-24 at day 1 (method 1)", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(0, 24)
  nonmem <- ds$nonmem %>% timerange(0, 24)
  auc1 <- Auc(campsis, "Y") %>% campsisnca::calculate()
  auc2 <- ncappcOutput(nonmem, metric="AUClast")
  auc3 <- calvaNCAOutput(nonmem, metric="AUClast")
  
  expect_equal(auc1@individual, auc2, tolerance=1e-3)
  expect_equal(auc1@individual, auc3, tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 7 (method 1)", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(144, 168)
  nonmem <- ds$nonmem %>% timerange(144, 168)
  auc1 <- Auc(campsis, "Y") %>% campsisnca::calculate()
  auc2 <- ncappcOutput(nonmem, metric="AUClast")
  auc3 <- calvaNCAOutput(ds$nonmem, metric="AUC144_168", AUCTimeRange=c(144,168))

  expect_equal(auc1@individual, auc2, tolerance=1e-3)
  expect_equal(auc1@individual, auc3, tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 1 (method 2)", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(0, 24)
  nonmem <- ds$nonmem %>% timerange(0, 24)
  auc1 <- Auc(campsis, "Y", method=2) %>% campsisnca::calculate()
  auc2 <- ncappcOutput(nonmem, metric="AUClast", method=2)
  auc3 <- calvaNCAOutput(nonmem, metric="AUClast", method=2)

  expect_equal(auc1@individual, auc2, tolerance=1e-3)
  expect_equal(auc1@individual, auc3, tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 7 (method 2)", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(144, 168)
  nonmem <- ds$nonmem %>% timerange(144, 168)
  auc1 <- Auc(campsis, "Y", method=2) %>% campsisnca::calculate()
  auc2 <- ncappcOutput(nonmem, metric="AUClast", method=2)
  auc3 <- calvaNCAOutput(ds$nonmem, metric="AUC144_168", AUCTimeRange=c(144,168), method=2)

  expect_equal(auc1@individual, auc2, tolerance=1e-3)
  expect_equal(auc1@individual, auc3, tolerance=1e-3)
})
