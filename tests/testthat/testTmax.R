
library(testthat)
library(dplyr)
library(campsis)
context("Test tmax method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - tmax at day 1", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(0, 24)
  nonmem <- ds$nonmem %>% timerange(0, 24)
  tmax1 <- Tmax(campsis, "Y") %>% campsisnca::calculate()
  tmax2 <- ncappcOutput(nonmem, metric="Tmax")
  expect_equal(tmax1@individual, tmax2, tolerance=1e-3)
})

test_that("Dataset 1 - tmax at day 4", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(144, 168)
  nonmem <- ds$nonmem %>% timerange(144, 168)
  tmax1 <- Tmax(campsis, "Y") %>% campsisnca::calculate()
  tmax2 <- ncappcOutput(nonmem, metric="Tmax")
  expect_equal(tmax1@individual, tmax2, tolerance=1e-3)
})
