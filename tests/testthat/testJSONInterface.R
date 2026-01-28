library(testthat)

context("Test the JSON interface")

testFolder <-  file.path(getwd(), test_path())

test_that("Import NCA config 1", {
  
  # Import NCA configuration from JSON
  nca_config <- loadFromJSON(new("nca_configuration"), file.path(testFolder, "json_examples", "nca_config_1.json"))
  # 
  # expect_equal(expectedModel, model)
})
