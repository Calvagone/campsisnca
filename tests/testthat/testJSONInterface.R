library(testthat)

context("Test the JSON interface")

testFolder <-  file.path(getwd(), test_path())

test_that("Import time range from JSON", {
  
  json <- jsonlite::read_json(file.path(testFolder, "json_examples", "nca_config_1.json"))
  
  time_range <- loadFromJSON(NCATimeRange(), JSONElement(json$nca_analyses[[1]]$time_range))
  expect_equal(time_range, NCATimeRange(0, 24))
  
  time_range <- loadFromJSON(NCATimeRange(), JSONElement(json$nca_analyses[[2]]$time_range))
  expect_equal(time_range, NCATimeRange(144, 168))
})

test_that("Import NCA config 1", {
  
  # Import NCA configuration from JSON
  nca_config <- loadFromJSON(NCAConfiguration(), file.path(testFolder, "json_examples", "nca_config_1.json"))
  # 
  # expect_equal(expectedModel, model)
})
