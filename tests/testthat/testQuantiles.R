library(testthat)
library(campsisnca)
library(campsis)
library(dplyr)
library(gtsummary)
library(gt)

context("Test extra features")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

campsis <- generateTestData()

test_that("Dynamic computation of quantiles works as expected (type=2)", {
  
  nca <- NCAAnalysis(variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", stat_display="{p5}-{p95}", digits=1))) %>%
    campsisnca::calculate(campsis)
  
  prettyString <- NCATable() %>%
    add(nca) %>%
    export(dest="dataframe", type="summary_pretty") %>%
    .$summary_stats
  
  expect_equal(prettyString, "511.5-1493.1")
  
  nca <- NCAAnalysis(variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", stat_display="{p2.5}-{p97.5}", digits=1))) %>%
    campsisnca::calculate(campsis)
  
  prettyString <- NCATable() %>%
    add(nca) %>%
    export(dest="dataframe", type="summary_pretty") %>%
    .$summary_stats
  
  expect_equal(prettyString, "455.7-1589.1")
})

test_that("Dynamic computation of quantiles works as expected (type=7)", {
  
  nca <- NCAAnalysis(variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", stat_display="{p5}-{p95}", digits=1))) %>%
    campsisnca::calculate(campsis, quantile_type=7)
  
  prettyString <- NCATable() %>%
    add(nca) %>%
    export(dest="dataframe", type="summary_pretty") %>%
    .$summary_stats
  
  expect_equal(prettyString, "516.1-1490.7")
  
  nca <- NCAAnalysis(variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", stat_display="{p2.5}-{p97.5}", digits=1))) %>%
    campsisnca::calculate(campsis, quantile_type=7)
  
  prettyString <- NCATable() %>%
    add(nca) %>%
    export(dest="dataframe", type="summary_pretty") %>%
    .$summary_stats
  
  expect_equal(prettyString, "463.8-1570.4")
})
