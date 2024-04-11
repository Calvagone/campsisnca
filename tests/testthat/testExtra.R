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
    add(c(Auc(unit="ng/mL*h", name="Area Under Curve"), Cavg(unit="ng/mL*h"))) %>%
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
    add(Auc()) %>%
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
  
  custom <- CustomMetric(x=campsis %>% timerange(0,24), variable="Y",
                         fun=~(Cmax() %>% iValue(.x, .y)) > 12.5,
                         name="Cmax > 12", unit="%", categorical=TRUE)

  custom <- custom %>% campsisnca::calculate()
  expect_equal(custom %>% campsisnca::statDisplayString(), "FALSE: 181 / 200 (91%), TRUE: 19 / 200 (9.5%)")
})

test_that("Method statDisplayString works as expected when digits is provided", {
  
  cmax1 <- Cmax(x=campsis %>% timerange(0,24), variable="Y")
  cmax1 <- cmax1 %>% campsisnca::calculate()
  expect_equal(cmax1 %>% campsisnca::statDisplayString(), "10.13 [7.37-13.38]")
  
  cmax2 <- Cmax(x=campsis %>% timerange(0,24), variable="Y", digits=~style_sigfig(.x))
  cmax2 <- cmax2 %>% campsisnca::calculate()
  expect_equal(cmax2 %>% campsisnca::statDisplayString(), "10 [7.4-13]")
  
  cmax3 <- Cmax(x=campsis %>% timerange(0,24), variable="Y", digits=~style_number(.x))
  cmax3 <- cmax3 %>% campsisnca::calculate()
  expect_equal(cmax3 %>% campsisnca::statDisplayString(), "10 [7-13]")
  
})
