library(testthat)
library(campsisnca)
library(campsis)
library(dplyr)
library(gtsummary)
library(gt)

context("Test extra features")

source(paste0("C:/prj/campsisnca/tests/testthat/", "testUtils.R"))

campsis <- generateData1()

test_that("Dynamic computation of quantiles works as expected (type=2)", {
  
  nca <- NCAMetrics(x=campsis, variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", stat_display="{p5}-{p95}", digits=1))) %>%
    campsisnca::calculate()
  
  prettyString <- NCAMetricsTable() %>%
    add(nca) %>%
    export(dest="dataframe", type="summary_pretty") %>%
    .$summary_stats
  
  expect_equal(prettyString, "511.5-1493.1")
  
  nca <- NCAMetrics(x=campsis, variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", stat_display="{p2.5}-{p97.5}", digits=1))) %>%
    campsisnca::calculate()
  
  prettyString <- NCAMetricsTable() %>%
    add(nca) %>%
    export(dest="dataframe", type="summary_pretty") %>%
    .$summary_stats
  
  expect_equal(prettyString, "455.7-1589.1")
})

test_that("Dynamic computation of quantiles works as expected (type=7)", {
  
  nca <- NCAMetrics(x=campsis, variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", stat_display="{p5}-{p95}", digits=1))) %>%
    campsisnca::calculate(quantile_type=7)
  
  prettyString <- NCAMetricsTable() %>%
    add(nca) %>%
    export(dest="dataframe", type="summary_pretty") %>%
    .$summary_stats
  
  expect_equal(prettyString, "516.1-1490.7")
  
  nca <- NCAMetrics(x=campsis, variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", stat_display="{p2.5}-{p97.5}", digits=1))) %>%
    campsisnca::calculate(quantile_type=7)
  
  prettyString <- NCAMetricsTable() %>%
    add(nca) %>%
    export(dest="dataframe", type="summary_pretty") %>%
    .$summary_stats
  
  expect_equal(prettyString, "463.8-1570.4")
})