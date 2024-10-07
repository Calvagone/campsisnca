
library(testthat)
library(dplyr)
library(campsis)
context("Test the 'TimeAboveLimit' and 'TimeBelowLimit' methods")

testFolder <- ""
source(paste0(testFolder, "testUtils.R"))

test_that("Dataset 1 - time above 0.75 at day 1", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(0, 24)
  spaghettiPlot(campsis, "CP")
  
  # ind1 <- campsis %>% filter(ID %in% c(1,2,3,4,5))
  # spaghettiPlot(ind1, "CP") +
  #   ggplot2::geom_hline(yintercept=0.75, linetype="dashed")
  
  results1 <- TimeAboveLimit(x=campsis, variable="CP", limit=0.75) %>% calculate()
  results1@individual
  
  outputRegressionTest(results1@individual, filename="dataset1_time_above_0_75.csv")
})

test_that("Dataset 1 - time below 0.75 at day 1 (check reciprocity)", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(0, 24)
  spaghettiPlot(campsis, "CP")

  # Make sure results are reciprocal with first test
  results2 <- TimeBelowLimit(x=campsis, variable="CP", limit=0.75) %>% calculate()
  results2@individual$value <- 24 - results2@individual$value
  results2@individual
  
  outputRegressionTest(results2@individual, filename="dataset1_time_above_0_75.csv")
})
