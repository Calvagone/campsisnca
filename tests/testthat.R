Sys.setenv("R_TESTS" = "")
library(testthat)
library(campsisnca)
test_check("campsisnca")
