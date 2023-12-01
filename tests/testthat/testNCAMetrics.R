
library(testthat)
library(dplyr)
library(campsis)

context("Test NCA metrics and NCA metrics table object")

testFolder <- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - day 1 & day 7", {
  ds <- dataset1()
  
  nca_d1 <- NCAMetrics(x=ds$campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1"))
  nca_d1 <- nca_d1 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=24)))
  nca_d1 <- nca_d1 %>% campsisnca::calculate()
  
  expect_equal(nca_d1 %>% getNames(), c("AUC", "Cmax", "tmax", "Ctrough"))
  
  nca_d7 <- NCAMetrics(x=ds$campsis %>% timerange(144, 168), variable="Y", scenario=c(day="Day 7"))
  nca_d7 <- nca_d7 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=168)))
  nca_d7 <- nca_d7 %>% campsisnca::calculate()
  
  expect_equal(nca_d7 %>% getNames(), c("AUC", "Cmax", "tmax", "Ctrough"))
  
  table <- NCAMetricsTable()  
  table <- table %>% add(c(nca_d1, nca_d7))
  
  # Export to dataframe
  df <- table %>% export(dest="dataframe")
  
  # Export to HTML table
  kable <- table %>% export(dest="kable")
  kable
})

test_that("Dataset 1 - day 1 & day 7 - seed 1 & seed 2", {
  ds1 <- dataset1(seed=1)
  ds2 <- dataset1(seed=2)
  
  nca_d1_seed1 <- NCAMetrics(x=ds1$campsis %>% timerange(0, 24), variable="Y", scenario=c(seed="Seed 1", day="Day 1"))
  nca_d1_seed1 <- nca_d1_seed1 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=24)))
  nca_d1_seed1 <- nca_d1_seed1 %>% campsisnca::calculate()
  
  nca_d7_seed1 <- NCAMetrics(x=ds1$campsis %>% timerange(144, 168), variable="Y", scenario=c(seed="Seed 1", day="Day 7"))
  nca_d7_seed1 <- nca_d7_seed1 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=168)))
  nca_d7_seed1 <- nca_d7_seed1 %>% campsisnca::calculate()
  
  nca_d1_seed2 <- NCAMetrics(x=ds2$campsis %>% timerange(0, 24), variable="Y", scenario=c(seed="Seed 2", day="Day 1"))
  nca_d1_seed2 <- nca_d1_seed2 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=24)))
  nca_d1_seed2 <- nca_d1_seed2 %>% campsisnca::calculate()
  
  nca_d7_seed2 <- NCAMetrics(x=ds2$campsis %>% timerange(144, 168), variable="Y", scenario=c(seed="Seed 2", day="Day 7"))
  nca_d7_seed2 <- nca_d7_seed2 %>% add(c(Auc(), Cmax(), Tmax(), Ctrough(time=168)))
  nca_d7_seed2 <- nca_d7_seed2 %>% campsisnca::calculate()
  
  table <- NCAMetricsTable()  
  table <- table %>% add(c(nca_d1_seed1, nca_d7_seed1, nca_d1_seed2, nca_d7_seed2))
  
  df <- table %>% export(dest="dataframe")
  kable <- table %>% export(dest="kable")
  kable
})

