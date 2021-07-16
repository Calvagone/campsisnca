
library(testthat)
library(dplyr)
library(campsis)
context("Test auc method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - AUC0-24 at day 1 (method 1)", {
  ds <- dataset1()
  auc1 <- auc(ds$campsis %>% timerange(0, 24), "Y")
  auc2 <- validateNCA(ds$nonmem %>% timerange(0, 24), metric="AUClast")
  expect_equal(auc1, auc2, tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 7 (method 1)", {
  ds <- dataset1()
  auc1 <- auc(ds$campsis %>% timerange(144, 168), "Y")
  auc2 <- validateNCA(ds$nonmem %>% timerange(144, 168), metric="AUClast")
  expect_equal(auc1, auc2, tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 1 (method 2)", {
  ds <- dataset1()
  auc1 <- auc(ds$campsis %>% timerange(0, 24), "Y", method=2)
  auc2 <- validateNCA(ds$nonmem %>% timerange(0, 24), metric="AUClast", method=2)
  expect_equal(auc1, auc2, tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 7 (method 2)", {
  ds <- dataset1()
  auc1 <- auc(ds$campsis %>% timerange(144, 168), "Y", method=2)
  auc2 <- validateNCA(ds$nonmem %>% timerange(144, 168), metric="AUClast", method=2)
  expect_equal(auc1, auc2, tolerance=1e-3)
})
