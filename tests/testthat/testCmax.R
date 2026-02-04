
library(testthat)
library(dplyr)
library(campsis)
context("Test Cmax method")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

test_that("Dataset 1 - Cmax at day 1", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  cmax1 <- Cmax("Y", TimeWindow(0, 24)) %>% campsisnca::calculate(campsis)
  cmax2 <- ncappcOutput(nonmem %>% timerange(0, 24), metric="Cmax")
  expect_equal(cmax1@individual, cmax2, tolerance=1e-3)
})

test_that("Dataset 1 - Cmax at day 7", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  cmax1 <- Cmax("Y", TimeWindow(144, 168)) %>% campsisnca::calculate(campsis)
  cmax2 <- ncappcOutput(nonmem %>% timerange(144, 168), metric="Cmax")
  expect_equal(cmax1@individual, cmax2, tolerance=1e-3)
})
