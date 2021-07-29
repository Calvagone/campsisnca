
library(testthat)
library(dplyr)
library(campsis)

context("Test NCA metrics object")

testFolder <<- "C:/prj/campsisnca/tests/testthat/"
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - day 1 & day 7", {
  ds <- dataset1()
  
  metrics <- NCAMetrics(x=ds$campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1"))
  metrics <- metrics %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=24)))
  metrics <- metrics %>% calculate()
    
})

