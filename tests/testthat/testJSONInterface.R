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

test_that("Import NCA table 1 (2 analyses, all metrics covered)", {
  
  # Import NCA configuration from JSON
  imported_table <- NCATable(json=file.path(testFolder, "json_examples", "nca_table_1.json"))

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
  
  expected_table <- NCATable() %>%
    add(analysis1) %>%
    add(analysis2)
  
  expect_equal(imported_table, expected_table)
})

test_that("Import NCA table 2 (default analysis, AUC only)", {
  
  # Import NCA configuration from JSON
  imported_table <- NCATable(json=file.path(testFolder, "json_examples", "nca_table_2.json"))
  
  analysis <- NCAAnalysis(name="Default", variable="CONC") %>%
    add(AUC(stat_display="{median} ({p5}–{p95})"))
  
  expected_table <- NCATable() %>%
    add(analysis)
  
  expect_equal(imported_table, expected_table)
})

test_that("Import NCA table 3 (default analysis, AUC and AUC custom)", {
  
  # Import NCA configuration from JSON
  imported_table <- NCATable(json=file.path(testFolder, "json_examples", "nca_table_3.json"))
  
  analysis <- NCAAnalysis(name="Default", variable="CONC") %>%
    add(AUC(stat_display="{median} ({p5}–{p95})")) %>%
    add(AUC(window=TimeWindow(0, 12), name="AUC custom", stat_display="{median} ({p5}–{p95})"))
  
  expected_table <- NCATable() %>%
    add(analysis)
  
  expect_equal(imported_table, expected_table)
})

test_that("Import NCA table 4 (default analysis, custom metric)", {
  
  # Import NCA configuration from JSON
  imported_table <- NCATable(json=file.path(testFolder, "json_examples", "nca_table_4.json"))
  
  analysis <- NCAAnalysis(name="Default", variable="CONC") %>%
    add(CustomMetric(fun=~AUC > 100, stat_display="{p}%", categorical=TRUE))
  
  expected_table <- NCATable() %>%
    add(analysis)
  
  expect_equal(imported_table, expected_table)

})

test_that("Import NCA table 5 (default analysis, Cmax with rounding options)", {
  
  # Import NCA configuration from JSON
  imported_table <- NCATable(json=file.path(testFolder, "json_examples", "nca_table_5.json"))
  
  analysis <- NCAAnalysis(name="Default", variable="CONC") %>%
    add(Cmax(name="Cmax 1", stat_display="{median} ({p5}–{p95})", digits=~style_sigfig(.x, 3))) %>%
    add(Cmax(name="Cmax 2", stat_display="{median} ({p5}–{p95})", digits=1))
  
  expected_table <- NCATable(title="Table 5 title", subtitle="Table 5 subtitle", tab_options=list(table.font.size="14px")) %>%
    add(analysis)
  
  expect_equal(imported_table, expected_table)
  
})

test_that("Import NCA table with proper time units", {
  
  # Import NCA configuration from JSON
  imported_table <- NCATable(json=file.path(testFolder, "json_examples", "nca_table_6.json"))
  
  analysis <- NCAAnalysis(name="Day 1", window=TimeWindow(0, 24), variable="Y") %>%
    add(AUC())
  
  expected_table <- NCATable(nca_options=NCAOptions(data_time_unit="hour", table_time_unit="day")) %>%
    add(analysis)
  
  expect_equal(imported_table, expected_table)
})
