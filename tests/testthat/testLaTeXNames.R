
library(testthat)

context("Test method getLaTeXName")

test_that("LaTex names derived from script cat.R are correct", {
  expect_equal(Ctrough() %>% campsisnca::getLaTeXName(), "C_{trough}")
  expect_equal(Clast() %>% campsisnca::getLaTeXName(), "C_{last}")
  expect_equal(CAt() %>% campsisnca::getLaTeXName(), "Conc")
  expect_equal(Last() %>% campsisnca::getLaTeXName(), "Last value")
  expect_equal(ValueAt() %>% campsisnca::getLaTeXName(), "Value")
})

test_that("LaTex names derived from script cavg.R are correct", {
  expect_equal(Cavg() %>% campsisnca::getLaTeXName(), "C_{avg}")
  expect_equal(Avg() %>% campsisnca::getLaTeXName(), "Avg") # No subscript used
})
