
library(testthat)
library(dplyr)
library(campsis)
context("Test tmax method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - tmax at day 1", {
  ds <- dataset1()
  tmax1 <- tmax(ds$campsis %>% timerange(0, 24), "Y")
  tmax2 <- validateNCA(ds$nonmem %>% timerange(0, 24), metric="Tmax")
  expect_equal(tmax1, tmax2 %>% rename(id=ID, tmax=Tmax), tolerance=1e-3)
})

test_that("Dataset 1 - tmax at day 4", {
  ds <- dataset1()
  tmax1 <- tmax(ds$campsis %>% timerange(72, 96), "Y")
  tmax2 <- validateNCA(ds$nonmem %>% timerange(72, 96), metric="Tmax")
  expect_equal(tmax1, tmax2 %>% rename(id=ID, tmax=Tmax), tolerance=1e-3)
})
