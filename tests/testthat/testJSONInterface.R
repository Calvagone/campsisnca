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
  imported_config <- loadFromJSON(NCAConfiguration(), file.path(testFolder, "json_examples", "nca_config_1.json"))

  analysis1 <- NCAAnalysis(name="Day 1", time_range=NCATimeRange(0, 24), variable="Y") %>%
    add(AUC()) %>%
    add(Cmax()) %>%
    add(Cmin()) %>%
    add(Cavg()) %>%
    add(Max()) %>%
    add(Min()) %>%
    add(Avg()) %>%
    add(Last()) %>%
    add(Ctrough()) %>%
    add(Tmin()) %>%
    add(Tmax()) %>%
    add(TimeAboveLimit(limit=10)) %>%
    add(TimeBelowLimit(limit=10))
  
  analysis2 <- NCAAnalysis(name="Day 7", time_range=NCATimeRange(144, 168), variable="Y") %>%
    add(AUC()) %>%
    add(Cmax()) %>%
    add(Cmin()) %>%
    add(Cavg()) %>%
    add(Max()) %>%
    add(Min()) %>%
    add(Avg()) %>%
    add(Last()) %>%
    add(Ctrough()) %>%
    add(Tmin()) %>%
    add(Tmax()) %>%
    add(TimeAboveLimit(limit=20)) %>%
    add(TimeBelowLimit(limit=20))
  
  expected_config <- NCAConfiguration() %>%
    add(analysis1) %>%
    add(analysis2)
  
  expect_equal(imported_config, expected_config)
})
