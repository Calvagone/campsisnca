
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
  
  results1 <- TimeAboveLimit(x=campsis, variable="CP", limit=0.75) %>% campsisnca::calculate()
  results1@individual
  
  outputRegressionTest(results1@individual, filename="dataset1_time_above_0_75.csv")
})

test_that("Dataset 1 - time below 0.75 at day 1 (check reciprocity)", {
  ds <- dataset1()
  campsis <- ds$campsis %>% timerange(0, 24)
  spaghettiPlot(campsis, "CP")

  # Make sure results are reciprocal with first test
  results2 <- TimeBelowLimit(x=campsis, variable="CP", limit=0.75) %>% campsisnca::calculate()
  results2@individual$value <- 24 - results2@individual$value
  results2@individual
  
  outputRegressionTest(results2@individual, filename="dataset1_time_above_0_75.csv")
})

test_that("Method 'computeTimeAboveBelow' deals well with special cases", {
  
  # Horizontal line, strictly higher than 0
  expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=0, y2=0, above=TRUE, strictly=TRUE), 0)
  
  # Horizontal line, higher or equal to 0
  expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=0, y2=0, above=TRUE, strictly=FALSE), 3)
  
  # Horizontal line, strictly lower than 0
  expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=0, y2=0, above=FALSE, strictly=TRUE), 0)
  
  # Horizontal line, lower or equal to 0
  expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=0, y2=0, above=FALSE, strictly=FALSE), 3)
  
  for (strictly in c(FALSE, TRUE)) {
    # First value is 0, second value is 1
    expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=0, y2=1, above=TRUE, strictly=strictly), 3)
    expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=0, y2=1, above=FALSE, strictly=strictly), 0)
    
    # First value is 0, second value is -1
    expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=0, y2=-1, above=TRUE, strictly=strictly), 0)
    expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=0, y2=-1, above=FALSE, strictly=strictly), 3)
    
    # First value is 1, second value is 0
    expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=1, y2=0, above=TRUE, strictly=strictly), 3)
    expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=1, y2=0, above=FALSE, strictly=strictly), 0)
    
    # First value is -1, second value is 0
    expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=-1, y2=0, above=TRUE, strictly=strictly), 0)
    expect_equal(computeTimeAboveBelow(x1=0, x2=3, y1=-1, y2=0, above=FALSE, strictly=strictly), 3)
  }
  
  # Vertical line should raise an error
  expect_error(computeTimeAboveBelow(x1=0, x2=0, y1=0, y2=1, above=TRUE, strictly=TRUE), msg="Sample times must be unique")

})
