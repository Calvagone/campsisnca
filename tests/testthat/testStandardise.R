
library(testthat)
library(dplyr)
library(campsis)
context("Test cmax method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Times must be monotonically increasing", {
  ds <- dataset1()
  campsis <- ds$campsis
  campsis[campsis$time==10 & campsis$id==5, "time"] <- 4
  expect_error(Auc(campsis, "Y") %>% campsisnca::calculate(), regexp="Times must be monotonically increasing")
})

test_that("Times can't be NA's", {
  ds <- dataset1()
  campsis <- ds$campsis
  campsis[campsis$time==10 & campsis$id==5, "time"] <- NA
  expect_error(Auc(campsis, "Y") %>% campsisnca::calculate(), regexp="Sample times cannot be NA")
})

test_that("Observations can't be NA's", {
  ds <- dataset1()
  campsis <- ds$campsis
  campsis[campsis$time==10 & campsis$id==5, "Y"] <- NA
  expect_error(Auc(campsis, "Y") %>% campsisnca::calculate(), regexp="Observations at times '10' are NA")
})

