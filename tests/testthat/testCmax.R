
library(testthat)
library(dplyr)
library(campsis)
context("Test cmax method")

testFolder <<- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - Cmax at day 1", {
  ds <- dataset1()
  cmax1 <- cmax(ds$campsis %>% timerange(0, 24), "Y")
  cmax2 <- validateNCA(ds$nonmem %>% timerange(0, 24), metric="Cmax")
  expect_equal(cmax1, cmax2 %>% rename(id=ID, cmax=Cmax), tolerance=1e-3)
})

test_that("Dataset 1 - Cmax at day 4", {
  ds <- dataset1()
  cmax1 <- cmax(ds$campsis %>% timerange(72, 96), "Y")
  cmax2 <- validateNCA(ds$nonmem %>% timerange(72, 96), metric="Cmax")
  expect_equal(cmax1, cmax2 %>% rename(id=ID, cmax=Cmax), tolerance=1e-3)
})
