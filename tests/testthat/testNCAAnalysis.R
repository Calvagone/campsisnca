
library(testthat)
library(dplyr)
library(campsis)
library(gt)
library(gtsummary)

context("Test NCA metrics and NCA metrics table object")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

test_that("Dataset 1 - day 1 & day 7", {
  ds <- dataset1()
  
  nca_d1 <- NCAAnalysis(name="Day 1", window=TimeWindow(0, 24), variable="Y") %>%
    add(c(AUC(), Cmax(), Tmax(), Ctrough())) %>%
    campsisnca::calculate(ds$campsis)
  
  expect_equal(nca_d1@metrics %>% getNames(), c("AUC", "Cmax", "tmax", "Ctrough"))
  
  nca_d7 <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="Y") %>%
    add(c(AUC(), Cmax(), Tmax(), Ctrough())) %>%
    campsisnca::calculate(ds$campsis)
  
  expect_equal(nca_d7@metrics %>% getNames(), c("AUC", "Cmax", "tmax", "Ctrough"))
  
  table <- NCAMetricsTable()  
  table <- table %>% add(c(nca_d1, nca_d7))
  
  # Export to dataframe
  df <- table %>% export(dest="dataframe")
  
  # Export to HTML table
  gtTable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTable
})

test_that("Dataset 1 - day 1 & day 7 - seed 1 & seed 2", {
  ds <- rbind(dataset1(seed=1)$campsis %>% mutate(Scenario="Seed 1"),
              dataset1(seed=2)$campsis %>% mutate(Scenario="Seed 2"))
  
  nca_d1 <- NCAAnalysis(name="Day 1", window=TimeWindow(0, 24), variable="Y", strat_vars=c("Scenario")) %>%
    add(c(AUC(), Cmax(), Tmax(), Ctrough()))
  
  nca_d2 <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="Y", strat_vars=c("Scenario")) %>%
    add(c(AUC(), Cmax(), Tmax(), Ctrough()))
  
  table <- NCAMetricsTable() %>%
    add(nca_d1) %>%
    add(nca_d2) %>%
    campsisnca::calculate(ds)
  
  # Export to dataframe
  df <- table %>%
    export(dest="dataframe")
  
  # A small test to show values will be slightly different based on the seed value
  medAucSeed1 <- df %>%
    filter(metric=="AUC", Scenario=="Seed 1", stat=="median", analysis=="Day 1") %>%
    pull(value)
  expect_equal(round(medAucSeed1), 13)
  
  medAucSeed2 <- df %>%
    filter(metric=="AUC", Scenario=="Seed 2", stat=="median", analysis=="Day 1") %>%
    pull(value)
  expect_equal(round(medAucSeed2), 13)
  
  expect_false(round(medAucSeed1, 3) == round(medAucSeed2, 3))
  
  # Export to HTML table
  gtTable <- table %>% export(dest="gt")
  gtTable
})

