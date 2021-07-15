
library(testthat)
library(dplyr)
library(campsis)
context("Test the AUC method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - AUC0-24 at day 1", {
  ds <- dataset1()
  auc1 <- auc(ds$campsis %>% filter(time <= 24), "Y")
  auc2 <- validateNCA(ds$nonmem %>% filter(TIME <= 24), metric="AUClast")
  expect_equal(auc1, auc2 %>% rename(id=ID, auc=AUClast), tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 4", {
  ds <- dataset1()
  auc1 <- auc(ds$campsis %>% filter(time >= 72 & time <= 72 + 24), "Y")
  auc2 <- validateNCA(ds$nonmem %>% filter(TIME >= 72 & TIME <= 72 + 24), metric="AUClast")
  expect_equal(auc1, auc2 %>% rename(id=ID, auc=AUClast), tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 1 (method 2)", {
  ds <- dataset1()
  auc1 <- auc(ds$campsis %>% filter(time <= 24), "Y", method=2)
  auc2 <- validateNCA(ds$nonmem %>% filter(TIME <= 24), metric="AUClast", method=2)
  expect_equal(auc1, auc2 %>% rename(id=ID, auc=AUClast), tolerance=1e-3)
})

test_that("Dataset 1 - AUC0-24 at day 4 (method 2)", {
  ds <- dataset1()
  auc1 <- auc(ds$campsis %>% filter(time >= 72 & time <= 72 + 24), "Y", method=2)
  auc2 <- validateNCA(ds$nonmem %>% filter(TIME >= 72 & TIME <= 72 + 24), metric="AUClast", method=2)
  expect_equal(auc1, auc2 %>% rename(id=ID, auc=AUClast), tolerance=1e-3)
})
