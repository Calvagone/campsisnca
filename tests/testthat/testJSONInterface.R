library(testthat)

context("Test the JSON interface")

testFolder <-  file.path(getwd(), test_path())

test_that("Import time range from JSON", {
  
  json <- jsonlite::read_json(file.path(testFolder, "json_examples", "nca_table_1.json"))
  
  time_range <- loadFromJSON(TimeWindow(), JSONElement(json$nca_analyses[[1]]$window))
  expect_equal(time_range, TimeWindow(0, 24))
  
  time_range <- loadFromJSON(TimeWindow(), JSONElement(json$nca_analyses[[2]]$window))
  expect_equal(time_range, TimeWindow(144, 168))
})

test_that("Import NCA config 1", {
  
  # Import NCA configuration from JSON
  imported_table <- loadFromJSON(NCAMetricsTable(), file.path(testFolder, "json_examples", "nca_table_1.json"))

  analysis1 <- NCAAnalysis(name="Day 1", window=TimeWindow(0, 24), variable="Y", strata=c(ARM="Specific arm")) %>%
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
    add(TimeBelowLimit(limit=10)) %>%
    add(Thalf())
  
  analysis2 <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="Y", strata=c(ARM="Specific arm")) %>%
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
    add(TimeBelowLimit(limit=20)) %>%
    add(Thalf())
  
  expected_table <- NCAMetricsTable() %>%
    add(analysis1) %>%
    add(analysis2)
  
  expect_equal(imported_table, expected_table)
})
