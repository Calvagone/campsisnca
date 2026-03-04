library(testthat)
library(dplyr)
library(campsis)

context("Test script filter_utils.R")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))


test_that("Method timerange works as expected", {
  ds <- dataset1()
  nonmem <- ds$nonmem
  
  day7a <- nonmem %>% timerange(144, 168)
  expect_true(all(day7a$TIME >= 144 & day7a$TIME <= 168))
  
  day7b <- nonmem %>% timerange(144, 168, exclmin=TRUE, exclmax=TRUE)
  expect_true(all(day7b$TIME > 144 & day7b$TIME < 168))
  expect_true(!144 %in% day7b$TIME)
  expect_true(!168 %in% day7b$TIME)
  
  # Additional tests
  expect_error(nonmem %>% timerange(144, "168"), regexp="min and max must be numeric")
  expect_error(nonmem %>% timerange("144", 168), regexp="min and max must be numeric")
  expect_error(nonmem %>% mutate(TIME=as.character(TIME)) %>% timerange(144, 168),
               regexp="TIME must be numeric")
})