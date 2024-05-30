library(testthat)
library(campsisnca)
library(campsis)
library(dplyr)
library(gtsummary)
library(gt)

context("Test extra features")

source(paste0("", "testUtils.R"))

campsis <- generateData1()

test_that("Column names can be non-standard", {
  
  nca <- NCAMetrics(x=campsis, variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", name="Area Under Curve"), Cavg(unit="ng/mL*h"))) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable()
  table <- table %>%
    add(nca)
  
  summary <- table %>%
    export(dest="dataframe")
  
  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, "non_standard_column_name")
  
})

test_that("Table can be reduced to 2 dimensions on demand", {
  
  nca <- NCAMetrics(x=campsis, variable="Y", scenario=c(a="1", b="1", c="1")) %>%
    add(AUC()) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable()  
  table <- table %>%
    add(nca)
  
  table <- table %>% campsisnca::reduceTo2Dimensions()
  
  scenario <- table@list[[1]]@scenario
  expect_equal(scenario, c(a_b="1 / 1", c="1"))

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, "reduction_to_2dim")
})

test_that("Method statDisplayString on categorical data should work", {
  
  custom1a <- CustomMetric(x=campsis %>% timerange(0,24), variable="Y",
                         fun=~(Cmax() %>% iValue(.x, .y)) > 12.5,
                         name="Cmax > 12", unit="%", categorical=TRUE)

  custom1a <- custom1a %>% campsisnca::calculate()
  expect_equal(custom1a %>% campsisnca::statDisplayString(), "FALSE: 183 / 200 (92%), TRUE: 17 / 200 (8.5%)")
  
  # Vice-versa
  custom1b <- CustomMetric(x=campsis %>% timerange(0,24), variable="Y",
                         fun=~(Cmax() %>% iValue(.x, .y)) <= 12.5,
                         name="Cmax > 12", unit="%", categorical=TRUE)
  
  custom1b <- custom1b %>% campsisnca::calculate()
  expect_equal(custom1b %>% campsisnca::statDisplayString(), "FALSE: 17 / 200 (8.5%), TRUE: 183 / 200 (92%)")
  
})

test_that("Method statDisplayString works as expected when digits is provided", {
  
  cmax1 <- Cmax(x=campsis %>% timerange(0,24), variable="Y")
  cmax1 <- cmax1 %>% campsisnca::calculate()
  expect_equal(cmax1 %>% campsisnca::statDisplayString(), "10.21 (7.86–13.13)")
  
  cmax2 <- Cmax(x=campsis %>% timerange(0,24), variable="Y", digits=~style_sigfig(.x))
  cmax2 <- cmax2 %>% campsisnca::calculate()
  expect_equal(cmax2 %>% campsisnca::statDisplayString(), "10 (7.9–13)")
  
  cmax3 <- Cmax(x=campsis %>% timerange(0,24), variable="Y", digits=~style_number(.x))
  cmax3 <- cmax3 %>% campsisnca::calculate()
  expect_equal(cmax3 %>% campsisnca::statDisplayString(), "10 (8–13)")
  
})

test_that("Summary stats on categorical data only should work as expected", {
  
  getCategory <- ~dplyr::case_when(Cmax < 10 ~ "(1) < 10 ng/mL", Cmax >= 10 & Cmax <= 15 ~ "(2) 10-15 ng/mL", Cmax > 15 ~ "(3) > 15 ng/mL")
  
  # Day 1
  ncaD1 <- NCAMetrics(x=campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1")) %>%
    add(c(CustomMetric(fun=getCategory, name="Cmax categories", unit="%", categorical=TRUE, stat_display="{p}%"))) %>%
    campsisnca::calculate()
  
  # Day 7 
  ncaD7 <- NCAMetrics(x=campsis %>% timerange(144, 168, rebase=TRUE), variable="Y", scenario=c(day="Day 7")) %>%
    add(c(CustomMetric(fun=getCategory, name="Cmax categories", unit="%", categorical=TRUE, stat_display="{p}%"))) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable()  
  table <- table %>%
    add(c(ncaD1, ncaD7))
  
  summary <- table %>%
    export(dest="dataframe")
  
  expect_equal(nrow(summary), 6) # 2 days * 3 categories * 1 stat
  
  individual <- table %>%
    export(dest="dataframe", type="individual_wide")
  
  subjects <- length(unique(campsis$ID))
  expect_equal(nrow(individual), subjects*2) # subjects * 2 categories
  expect_equal(length(unique(individual$`Cmax categories`)), 3)
})

test_that("Order of metrics when 'individual_wide' is requested should be respected", {
  # Day 7 
  ncaD7 <- NCAMetrics(x=campsis %>% timerange(144, 168, rebase=TRUE), variable="Y", scenario=c(day="Day 7")) %>%
    add(Cmax()) %>%
    add(c(CustomMetric(fun=~Cmax > 10, name="Cmax > 10", unit="%", categorical=TRUE, stat_display="{p}%"))) %>%
    add(AUC()) %>%
    add(c(CustomMetric(fun=~Cmax > 15, name="Cmax > 15", unit="%", categorical=TRUE, stat_display="{p}%"))) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable()  
  table <- table %>%
    add(c(ncaD7))
  
  individual <- table %>%
    export(dest="dataframe", type="individual_wide")
  
  colnames <- colnames(individual)
  expect_equal(colnames, c("id", "day", "Cmax", "Cmax > 10", "AUC", "Cmax > 15"))
})
