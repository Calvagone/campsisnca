
library(testthat)

context("Test method getLaTeXName")

test_that("LaTex names derived from script cat.R are correct", {
  expect_equal(Ctrough() %>% getLaTeXName(), "C_{trough}")
  expect_equal(Clast() %>% getLaTeXName(), "C_{last}")
  expect_equal(CAt() %>% getLaTeXName(), "Conc")
  expect_equal(Last() %>% getLaTeXName(), "Last value")
  expect_equal(ValueAt() %>% getLaTeXName(), "Value")
})

test_that("LaTex names derived from script cavg.R are correct", {
  expect_equal(Cavg() %>% getLaTeXName(), "C_{avg}")
  expect_equal(Avg() %>% getLaTeXName(), "Avg") # No subscript used
})
