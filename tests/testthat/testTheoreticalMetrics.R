
library(testthat)
context("Compute theoritical NCA metrics from PK parameters")

test_that("Run from modelling", {
  x <- data.frame(id=1, time=0, TAU=12, DOSE=10000, CL=48, V2=208, Q=18, V3=684, KA=3.3)
  metrics <- metrics.2cpt(x)
  
  tol <- 1e-6
  
  expect_equal(metrics$K20, 0.230769231, tolerance=tol)
  expect_equal(metrics$K23, 0.086538462, tolerance=tol)
  expect_equal(metrics$K32, 0.026315789, tolerance=tol)
  
  expect_equal(metrics$A, 1, tolerance=tol)
  expect_equal(metrics$B, 0.343623482, tolerance=tol)
  expect_equal(metrics$C, 0.006072874, tolerance=tol)
  expect_equal(metrics$DETER, 0.093785599, tolerance=tol)
  
  expect_equal(metrics$ALPHA, 0.324933914, tolerance=tol)
  expect_equal(metrics$BETA, 0.018689568, tolerance=tol)
  
  expect_equal(metrics$COEFF_A, 51.99984773, tolerance=tol)
  expect_equal(metrics$COEFF_B, 1.204050264, tolerance=tol)
  expect_equal(metrics$COEFF_C, -53.20389799, tolerance=tol)
  
  expect_equal(metrics$AUC, 208.3333, tolerance=tol)
  expect_equal(metrics$AUC_Z_1, 160.0321, tolerance=tol)
  expect_equal(metrics$AUC_Z, 64.42365, tolerance=tol)
  expect_equal(metrics$AUC_KA, -16.12239, tolerance=tol)
  
  expect_equal(metrics$AUC_Z_1_pc, 71.29783, tolerance=tol)
  expect_equal(metrics$AUC_Z_pc, 28.70217, tolerance=tol)
  
  expect_equal(metrics$THALF, 2.133194, tolerance=tol)
  expect_equal(metrics$THALF_Z, 37.08738, tolerance=tol)
  expect_equal(metrics$THALF_EFF, 12.1658, tolerance=tol)
  
})