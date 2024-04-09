
library(testthat)
library(dplyr)
library(campsis)
context("Test cmax method")

source(paste0("", "testUtils.R"))

test_that("Dataset 1 - Cmax at day 1 via custom metric", {
  ds <- dataset1()
  
  campsis <- ds$campsis %>% timerange(0, 24)
  nonmem <- ds$nonmem %>% timerange(0, 24)
  
  customFun <- function(data) {
    max(data$Y)
  }
  
  cmax1a <- Cmax(campsis, "Y") %>% campsisnca::calculate()
  cmax1b <- CustomMetric(campsis, "Y", fun=~max(.x$Y)) %>% campsisnca::calculate() # Lambda
  cmax1c <- CustomMetric(campsis, "Y", fun=customFun) %>% campsisnca::calculate() # Normal function
  
  cmax2 <- ncappcOutput(nonmem, metric="Cmax")
  
  expect_equal(cmax1a@individual, cmax2, tolerance=1e-3)
  expect_equal(cmax1b@individual, cmax2, tolerance=1e-3)
  expect_equal(cmax1c@individual, cmax2, tolerance=1e-3)
})
