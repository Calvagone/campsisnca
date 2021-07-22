
library(testthat)
library(dplyr)
library(campsis)
context("Test cmax method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - Cmax at day 1", {
  ds <- dataset1()
  cmax1 <- cmax(ds$campsis %>% timerange(0, 24), "Y")
  cmax2 <- ncappcOutput(ds$nonmem %>% timerange(0, 24), metric="Cmax")
  expect_equal(cmax1, cmax2, tolerance=1e-3)
})

test_that("Dataset 1 - Cmax at day 7", {
  ds <- dataset1()
  cmax1 <- cmax(ds$campsis %>% timerange(144, 168), "Y")
  cmax2 <- ncappcOutput(ds$nonmem %>% timerange(144, 168), metric="Cmax")
  expect_equal(cmax1, cmax2, tolerance=1e-3)
})
