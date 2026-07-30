
library(testthat)

context("Test method get_latex_name")

test_that("LaTex names derived from script cat.R are correct", {
  expect_equal(Ctrough() %>% campsisnca::get_latex_name(), "C_{trough}")
  expect_equal(CAt() %>% campsisnca::get_latex_name(), "Conc")
  expect_equal(Last() %>% campsisnca::get_latex_name(), "Last value")
  expect_equal(ValueAt() %>% campsisnca::get_latex_name(), "Value")
})

test_that("LaTex names derived from script cavg.R are correct", {
  expect_equal(Cavg() %>% campsisnca::get_latex_name(), "C_{avg}")
  expect_equal(Avg() %>% campsisnca::get_latex_name(), "Avg") # No subscript used
})
