
library(testthat)
library(dplyr)
library(campsis)

context("Test the 'Change From Baseline' metric")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))


# A small fictitious dataset with 3 subjects and 4 time points each.
# Subject 3 has a baseline of 0 (edge case for percent/ratio/log).
cfb_dataset <- function() {
  data.frame(
    ID   = rep(1:3, each=4),
    TIME = rep(0:3, times=3),
    Y    = c(100, 120,  90,  80,   # subject 1: baseline 100
              50,  55,  45,  40,   # subject 2: baseline 50
               0,  10,  20,   5)   # subject 3: baseline 0
  )
}

#_______________________________________________________________________________
#----                       i_value: absolute method                        ----
#_______________________________________________________________________________

test_that("i_value (absolute) returns value - baseline", {
  metric <- ChangeFromBaseline(method="absolute")
  expect_equal(metric %>% i_value(time=c(0, 24), value=c(100, 80)), -20)
  expect_equal(metric %>% i_value(time=c(0, 24), value=c(110, 80)), -30)
})

#_______________________________________________________________________________
#----                       i_value: percent method                         ----
#_______________________________________________________________________________

test_that("i_value (percent) returns 100 * (value - baseline) / baseline", {
  metric <- ChangeFromBaseline(method="percent")
  expect_equal(metric %>% i_value(time=c(0, 24), value=c(100, 80)), -20)
  expect_equal(metric %>% i_value(time=c(0, 24), value=c(110, 80)), (80-110)/110*100)
})

test_that("i_value (percent) returns NA when baseline is 0", {
  metric <- ChangeFromBaseline(method="percent")
  expect_true(is.na(metric %>% i_value(time=c(0, 24), value=c(0, -10))))
})

#_______________________________________________________________________________
#----                        i_value: ratio method                          ----
#_______________________________________________________________________________

test_that("i_value (ratio) returns value / baseline", {
  metric <- ChangeFromBaseline(method="ratio")
  expect_equal(metric %>% i_value(time=c(0, 24), value=c(100, 80)), 80/100)
  expect_equal(metric %>% i_value(time=c(0, 24), value=c(110, 80)), 80/110)
})

test_that("i_value (ratio) returns NA when baseline is 0", {
  metric <- ChangeFromBaseline(method="ratio")
  expect_true(is.na(metric %>% i_value(time=c(0, 24), value=c(0, -10))))
})

#_______________________________________________________________________________
#----                         i_value: log method                           ----
#_______________________________________________________________________________

test_that("i_value (log) returns log(value) - log(baseline)", {
  metric <- ChangeFromBaseline(method="log")
  expect_equal(metric %>% i_value(time=c(0, 24), value=c(100, 80)), log(80) - log(100))
  expect_equal(metric %>% i_value(time=c(0, 24), value=c(110, 80)), log(80) - log(110))
})

test_that("i_value (log) returns NA when baseline is <= 0", {
  metric <- ChangeFromBaseline(method="log")
  expect_true(is.na(metric %>% i_value(time=c(0, 24), value=c(0, 10))))
})

test_that("i_value (log) returns NA when any value is <= 0", {
  metric <- ChangeFromBaseline(method="log")
  expect_true(is.na(metric %>% i_value(time=c(0, 24), value=c(50, -10))))
})

#_______________________________________________________________________________
#----                       i_value: missing baseline                       ----
#_______________________________________________________________________________

test_that("i_value errors for all 4 methods when there is no observation", {
  # The generic i_value() dispatcher enforces length(value) > 0 up front,
  # so baseline_metric's own NA-handling for an empty vector is unreachable.
  for (method in c("absolute", "percent", "ratio", "log")) {
    metric <- ChangeFromBaseline(method=method)
    expect_error(i_value(metric, numeric(0), numeric(0)), "value should contain at least 1 value")
  }
})

#_______________________________________________________________________________
#----                    get_default_name / get_latex_name                  ----
#_______________________________________________________________________________

test_that("get_default_name returns the expected acronym for each method", {
  expect_equal(ChangeFromBaseline(method="absolute") %>% get_default_name(), "CFB")
  expect_equal(ChangeFromBaseline(method="percent") %>% get_default_name(), "PCFB")
  expect_equal(ChangeFromBaseline(method="ratio") %>% get_default_name(), "Ratio")
  expect_equal(ChangeFromBaseline(method="log") %>% get_default_name(), "CFBlog")
})

test_that("get_latex_name only adds a subscript for the log method", {
  metric_log <- ChangeFromBaseline(method="log")
  expect_equal(metric_log %>% get_latex_name(), "CFB_{log}")

  metric_abs <- ChangeFromBaseline(method="absolute")
  expect_equal(metric_abs %>% get_latex_name(), "CFB")
})

#_______________________________________________________________________________
#----                 ChangeFromBaseline() / CFB() constructor              ----
#_______________________________________________________________________________

test_that("ChangeFromBaseline()/CFB() build a valid metric with correct default name", {
  expect_equal(ChangeFromBaseline(method="absolute")@name, "CFB")
  expect_equal(ChangeFromBaseline(method="percent")@name, "PCFB")
  expect_equal(ChangeFromBaseline(method="ratio")@name, "Ratio")
  expect_equal(ChangeFromBaseline(method="log")@name, "CFBlog")
  expect_equal(CFB(method="absolute")@method, "absolute")
})

test_that("ChangeFromBaseline()/CFB() rejects an invalid method", {
  expect_error(ChangeFromBaseline(method="unknown"))
})

#_______________________________________________________________________________
#----               calculate() end-to-end on a fictitious dataset          ----
#_______________________________________________________________________________

test_that("calculate() computes individual CFB values for all 4 methods", {
  df <- cfb_dataset()

  expected <- list(
    absolute = c(-20, -10, 5),
    percent = c(-20, -20, NA_real_),
    ratio = c(0.8, 0.8, NA_real_),
    log = c(log(0.8), log(0.8), NA_real_) # log(0.8) evaluates to ~ -0.2231436
  )

  for (method in names(expected)) {
    metric <- ChangeFromBaseline("Y", method=method) %>%
      campsisnca::calculate(df)
    expect_equal(metric@individual$value, expected[[method]], tolerance=1e-6)
  }
})
