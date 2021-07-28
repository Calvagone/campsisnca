
library(testthat)
library(dplyr)
library(campsis)
context("Test cmax method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - Cmax at day 1", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(0, 24)
  nonmem <- ds$nonmem %>% timerange(0, 24)
  cmax1 <- Cmax(campsis, "Y") %>% campsisnca::calculate()
  cmax2 <- ncappcOutput(nonmem, metric="Cmax")
  expect_equal(cmax1@individual, cmax2, tolerance=1e-3)
})

test_that("Dataset 1 - Cmax at day 7", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(144, 168)
  nonmem <- ds$nonmem %>% timerange(144, 168)
  cmax1 <- Cmax(campsis, "Y")  %>% campsisnca::calculate()
  cmax2 <- ncappcOutput(nonmem, metric="Cmax")
  expect_equal(cmax1@individual, cmax2, tolerance=1e-3)
})
