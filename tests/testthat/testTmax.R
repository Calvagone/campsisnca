
library(testthat)
library(dplyr)
library(campsis)
context("Test tmax method")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

test_that("Dataset 1 - tmax at day 1", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  tmax1 <- Tmax("Y", TimeWindow(0, 24)) %>% campsisnca::calculate(campsis)
  tmax2 <- ncappc_output(nonmem %>% timerange(0, 24), metric="Tmax",
    reg_file=ncappc_reg_file("dataset1_tmax_day1.csv"))
  expect_equal(tmax1@individual, tmax2, tolerance=1e-3)
})

test_that("Dataset 1 - tmax at day 7", {
  ds <- dataset1()
  campsis <- ds$campsis
  nonmem <- ds$nonmem
  tmax1 <- Tmax("Y", TimeWindow(144, 168), rebase=FALSE) %>% campsisnca::calculate(campsis)
  tmax2 <- ncappc_output(nonmem %>% timerange(144, 168), metric="Tmax",
    reg_file=ncappc_reg_file("dataset1_tmax_day7.csv"))
  expect_equal(tmax1@individual, tmax2, tolerance=1e-3)
})
