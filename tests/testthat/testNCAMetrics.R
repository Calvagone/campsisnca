
library(testthat)
library(dplyr)
library(campsis)

context("Test NCA metrics and NCA metrics table object")

testFolder <<- "C:/prj/campsisnca/tests/testthat/"
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - day 1 & day 7", {
  ds <- dataset1()
  
  metrics_d1 <- NCAMetrics(x=ds$campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1"))
  metrics_d1 <- metrics_d1 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=24)))
  metrics_d1 <- metrics_d1 %>% calculate()
  
  metrics_d7 <- NCAMetrics(x=ds$campsis %>% timerange(144, 168), variable="Y", scenario=c(day="Day 7"))
  metrics_d7 <- metrics_d7 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=168)))
  metrics_d7 <- metrics_d7 %>% calculate()
  
  table <- NCAMetricsTable()  
  table <- table %>% add(metrics_d1, metrics_d7)
})

