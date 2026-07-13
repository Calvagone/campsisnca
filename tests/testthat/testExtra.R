library(testthat)
library(campsis)
library(dplyr)
library(gtsummary)
library(gt)

context("Test extra features")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

campsis <- campsisnca::generate_test_data()

getRefFile <- function(filename) {
  return(file.path(testFolder, "non_regression", filename))
}

test_that("Column names can be non-standard", {
  
  nca <- NCAAnalysis(variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", name="Area Under Curve"), Cavg(unit="ng/mL*h"))) %>%
    campsisnca::calculate(campsis)
  
  table <- NCATable(subscripts=TRUE)
  table <- table %>%
    add(nca)
  
  table %>%
    export(dest="dataframe", type="individual_wide")
  
  summary <- table %>%
    export(dest="dataframe")
  
  expect_true(all(c("Area Under Curve", "Cavg") %in% summary$metric))
  
  gttable <- table %>% export(dest="gt")
  gtTableRegressionTest(gttable, getRefFile("non_standard_column_name.html"))
  
})

test_that("Statistics can contain line breaks", {
  
  nca <- NCAAnalysis(variable="Y") %>%
    add(c(AUC(stat_display="{geomean}<BR>({geocv}%)"), Cavg(stat_display="{geomean}<BR>({geocv}%)"))) %>%
    campsisnca::calculate(campsis)
  
  table <- NCATable(subscripts=TRUE)
  table <- table %>%
    add(nca)
  
  summary <- table %>%
    export(dest="dataframe", type="summary_pretty")
  
  expect_equal(summary$metric, c("AUC", "Cavg"))
  expect_equal(summary$summary_stats, c("909<BR>(35.2%)", "3.79<BR>(35.2%)")) # No conversion yet at this stage
  
  gttable <- table %>% export(dest="gt", fmt_markdown=TRUE)
  gtTableRegressionTest(gttable, getRefFile("linebreaks_in_stats.html"))
  
})

test_that("Method stat_display_string on categorical data should work", {
  
  custom1a <- CustomMetric("Y", TimeWindow(0, 24),
                         fun=~(Cmax() %>% i_value(.x, .y)) > 12.5,
                         name="Cmax > 12", unit="%", categorical=TRUE)

  custom1a <- custom1a %>% campsisnca::calculate(campsis)
  expect_equal(custom1a %>% campsisnca::stat_display_string(), c("FALSE: 183 / 200 (91.5%)", "TRUE: 17 / 200 (8.50%)"))
  
  # Vice-versa
  custom1b <- CustomMetric("Y", TimeWindow(0, 24),
                         fun=~(Cmax() %>% i_value(.x, .y)) <= 12.5,
                         name="Cmax > 12", unit="%", categorical=TRUE)
  
  custom1b <- custom1b %>% campsisnca::calculate(campsis)
  expect_equal(custom1b %>% campsisnca::stat_display_string(), c("FALSE: 17 / 200 (8.50%)", "TRUE: 183 / 200 (91.5%)"))
  
})

test_that("Method stat_display_string works as expected on continuous data when digits is provided", {
  
  cmax1 <- Cmax("Y", TimeWindow(0, 24))
  cmax1 <- cmax1 %>% campsisnca::calculate(campsis)
  expect_equal(cmax1 %>% campsisnca::stat_display_string(), "10.2 (7.85–13.1)")
  
  cmax2 <- Cmax("Y", TimeWindow(0, 24), digits=~style_sigfig(.x))
  cmax2 <- cmax2 %>% campsisnca::calculate(campsis)
  expect_equal(cmax2 %>% campsisnca::stat_display_string(), "10 (7.8–13)")
  
  cmax3 <- Cmax("Y", TimeWindow(0, 24), digits=~style_number(.x))
  cmax3 <- cmax3 %>% campsisnca::calculate(campsis)
  expect_equal(cmax3 %>% campsisnca::stat_display_string(), "10 (8–13)")
  
})

test_that("Summary stats on categorical data only should work as expected", {
  
  getCategory <- ~dplyr::case_when(Cmax < 10 ~ "(1) < 10 ng/mL", Cmax >= 10 & Cmax <= 15 ~ "(2) 10-15 ng/mL", Cmax > 15 ~ "(3) > 15 ng/mL")
  
  # Day 1
  ncaD1 <- NCAAnalysis(name="Day 1", window=TimeWindow(0, 24), variable="Y") %>%
    add(c(CustomMetric(fun=getCategory, name="Cmax categories", unit="%", categorical=TRUE, stat_display="{p}% ({n}/{N})"))) %>%
    campsisnca::calculate(campsis)
  
  # Day 7 
  ncaD7 <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="Y") %>%
    add(c(CustomMetric(fun=getCategory, name="Cmax categories", unit="%", categorical=TRUE, stat_display="{p}% ({n}/{N})"))) %>%
    campsisnca::calculate(campsis)
  
  table <- NCATable()  
  table <- table %>%
    add(c(ncaD1, ncaD7))
  
  summary <- table %>%
    export(dest="dataframe")
  
  expect_equal(nrow(summary), 2*3*3) # 2 days * 3 categories * 3 stat
  outputRegressionTest(data=summary, file=getRefFile("categorical_data_summary.csv"))
  
  summary_wide <- table %>%
    export(dest="dataframe", type="summary_wide")
  outputRegressionTest(data=summary_wide, file=getRefFile("categorical_data_summary_wide.csv"))
  
  summary_pretty <- table %>%
    export(dest="dataframe", type="summary_pretty")
  outputRegressionTest(data=summary_pretty, file=getRefFile("categorical_data_summary_pretty.csv"))
  
  individual <- table %>%
    export(dest="dataframe", type="individual_wide")
  outputRegressionTest(data=individual[1:20,] %>% dplyr::rename(Categories=`Cmax categories`), file=getRefFile("categorical_data_individual.csv"))

  subjects <- length(unique(campsis$ID))
  expect_equal(nrow(individual), subjects*2) # subjects * 2 categories
  expect_equal(length(unique(individual$`Cmax categories`)), 3)
})

test_that("Order of metrics when 'individual_wide' is requested should be respected", {
  # Day 7 
  ncaD7 <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="Y") %>%
    add(Cmax()) %>%
    add(c(CustomMetric(fun=~Cmax > 10, name="Cmax > 10", unit="%", categorical=TRUE, stat_display="{p}%"))) %>%
    add(AUC()) %>%
    add(c(CustomMetric(fun=~Cmax > 15, name="Cmax > 15", unit="%", categorical=TRUE, stat_display="{p}%"))) %>%
    campsisnca::calculate(campsis)
  
  table <- NCATable()  
  table <- table %>%
    add(c(ncaD7))
  
  individual <- table %>%
    export(dest="dataframe", type="individual_wide")
  
  colnames <- colnames(individual)
  expect_equal(colnames, c("id", "Cmax", "Cmax > 10", "AUC", "Cmax > 15"))
})

test_that("Method stat_display_string works as expected on categorical data when digits is provided", {
  # Remove last individual, this way, the dataset will contain 199 subjects, an odd number 
  campsis_ <- campsis %>%
    filter(ID != 200)
  
  # Default behaviour
  custom <- CustomMetric("Y", TimeWindow(0, 24), fun=~Cmax > 10,
                         stat_display="{p}%", digits=NULL, categorical=TRUE)
  custom <- custom %>% campsisnca::calculate(campsis_)
  expect_equal(custom %>% campsisnca::stat_display_string(), c("FALSE: 43.7%", "TRUE: 56.3%"))
  
  # 1 digit using style_percent (same as default)
  custom <- CustomMetric("Y", TimeWindow(0, 24), fun=~Cmax > 10,
                         stat_display="{p}", digits=~style_percent(.x, digits=1, suffix='%'), categorical=TRUE)
  custom <- custom %>% campsisnca::calculate(campsis_)
  expect_equal(custom %>% campsisnca::stat_display_string(), c("FALSE: 43.7%", "TRUE: 56.3%"))

  # 2 digits using style_percent
  custom <- CustomMetric("Y", TimeWindow(0, 24), fun=~Cmax > 10,
                         stat_display="{p}", digits=~style_percent(.x, digits=2, suffix='%'), categorical=TRUE)
  custom <- custom %>% campsisnca::calculate(campsis_)
  expect_equal(custom %>% campsisnca::stat_display_string(), c("FALSE: 43.72%", "TRUE: 56.28%"))

  # digits=2
  custom <- CustomMetric("Y", TimeWindow(0, 24), , fun=~Cmax > 10,
                         stat_display="{p}%", digits=2, categorical=TRUE)
  custom <- custom %>% campsisnca::calculate(campsis_)
  expect_equal(custom %>% campsisnca::stat_display_string(), c("FALSE: 43.72%", "TRUE: 56.28%"))
  
  # digits=0
  custom <- CustomMetric("Y", TimeWindow(0, 24), , fun=~Cmax > 10,
                         stat_display="{p}%", digits=0, categorical=TRUE)
  custom <- custom %>% campsisnca::calculate(campsis_)
  expect_equal(custom %>% campsisnca::stat_display_string(), c("FALSE: 44%", "TRUE: 56%"))
  
  # Extra test, only stat 'p' was computed
  expect_equal(unique(custom@summary$stat), "p")
})

test_that("Time unit of AUC can be customised", {
  
  # First example with AUC
  nca <- NCAAnalysis(variable="Y") %>%
    add(AUC(stat_display="{median}", digits=1))
  
  median_hour <- NCATable() %>%
    add(nca) %>%
    campsisnca::calculate(campsis) %>%
    export(dest="dataframe", type="summary_pretty")
  
  median_day <- NCATable(nca_options=NCAOptions(table_time_unit="day")) %>%
    add(nca) %>%
    campsisnca::calculate(campsis) %>%
    export(dest="dataframe", type="summary_pretty")
  
  expect_equal(median_hour$summary_stats, "921.5")
  expect_equal(median_day$summary_stats, "38.4")
  
  # Second example with real simulated data
  table <- NCATable(json=file.path(testFolder, "json_examples", "nca_table_8.json"))
  
})

test_that("Time unit of tmax and tmin can be customised as well", {
  
  # Second example with real simulated data
  table <- NCATable(json=file.path(testFolder, "json_examples", "nca_table_8.json"))
  
  model <- model_suite$pk$`2cpt_fo`
  
  dataset <- Dataset(100) %>%
    add(Bolus(time=0, amount=1000, compartment="ABS", ii=24, addl=6)) %>%
    add(Observations(times=TimeSequence(0, 24, 0.1), rep=DosingSchedule()))
  
  x <- simulate(model=model, dataset=dataset, dest="mrgsolve", seed=1)
  spaghettiPlot(x, "CONC")
  
  stats <- table %>%
    campsisnca::calculate(x=x) %>%
    export(dest="dataframe", type="summary_pretty")
  
  expect_equal(stats$metric, c("Cmax", "tmax", "Cmax", "tmax"))
  expect_equal(stats$analysis, c("Day 1", "Day 1", "Day 7", "Day 7"))
  expect_equal(stats$summary_stats, c("24.7 (17.9–35.1)", "48.0 (30.0–78.0)", "31.3 (23.0–40.3)", "48.0 (30.0–72.0)"))
})

test_that("Method NCATableOutfun can be used in campsisnca", {
  
  table <- NCATable(json=file.path(testFolder, "json_examples", "nca_table_9.json"))
  subjects <- 100
  model <- model_suite$pk$`2cpt_fo`
  
  dataset <- Dataset(100) %>%
    add(Bolus(time=0, amount=1000, compartment="ABS", ii=24, addl=6)) %>%
    add(Observations(times=TimeSequence(0, 24, 0.1), rep=DosingSchedule()))

  outfun <- NCATableOutfun(table=table, export_type="individual")
  
  stats <- simulate(model=model, dataset=dataset, dest="mrgsolve", seed=1, outfun=outfun)
  expect_true("individual_campsisnca_tbl" %in% class(stats))
  expect_equal(colnames(stats), c("metric", "id", "value", "discrete_value"))
  expect_equal(stats$metric, c(rep("Cmax", subjects), rep("AUC", subjects)))
  expect_equal(stats$id, c(1:subjects, 1:subjects))
  

  stats <- simulate(model=model, dataset=dataset, dest="mrgsolve", seed=1, outfun=outfun, replicates=5)
  expect_equal(unique(stats$replicate), 1:5)
  expect_true("individual_campsisnca_tbl" %in% class(stats))
  stats_rep1 <- stats %>% dplyr::filter(replicate==1)
  expect_equal(colnames(stats_rep1), c("replicate", "metric", "id", "value", "discrete_value"))
  expect_equal(stats_rep1$metric, c(rep("Cmax", subjects), rep("AUC", subjects)))
  expect_equal(stats_rep1$id, c(1:subjects, 1:subjects))
})

test_that("Method summarise_replicates can be used to summarise Campsisnca output across replicates", {
  # Same as NCA table 9 but with a categorical metric added + specific options
  table <- NCATable(
    json = file.path(testFolder, "json_examples", "nca_table_10.json")
  )
  subjects <- 100
  model <- model_suite$pk$`2cpt_fo`

  dataset <- Dataset(100) %>%
    add(Bolus(time=0, amount=1000, compartment="ABS", ii=24, addl=6)) %>%
    add(Observations(times=TimeSequence(0, 24, 0.1), rep=DosingSchedule()))

  scenarios <- Scenarios() %>%
    add(Scenario(name="Base scenario")) %>%
    add(Scenario(name="Lower CL") %>%
      add(ReplaceAction(Theta(name="CL", value=1.5))))

  outfun <- NCATableOutfun(table = table, export_type = "summary")

  x <- simulate(model=model, dataset=dataset, dest="mrgsolve", seed=1, outfun=outfun, scenarios=scenarios, replicates=5)

  summary <- table %>%
    summarise_replicates(x = x)
  expect_equal(
    colnames(summary),
    c(
      "SCENARIO",
      "Cmax (geomean)",
      "Cmax (geocv)",
      "AUC (geomean)",
      "AUC (geocv)",
      "AUC > 300 (n_TRUE)",
      "AUC > 300 (N_TRUE)",
      "AUC > 300 (p_TRUE)"
    )
  )

  # Stat type is 'continuous2' because several statistical strings
  gttable <- table %>%
    summarise_replicates(x = x, dest = "gt")

  gtTableRegressionTest(gttable, getRefFile("summarised_replicated_table10a.html"))

  # Changing to 1 string stat display
  # Stat type should now be 'continuous'
  table@nca_options@rep_stat_display <- c("{median} ({p5}–{p95})")
  gttable <- table %>%
    summarise_replicates(x = x, dest = "gt")

  gtTableRegressionTest(gttable, getRefFile("summarised_replicated_table10b.html"))


  # Testing method 'translate_stat_string'
  expect_equal(translate_stat_string("{median} ({p5}–{p95})"), "Median (5th–95th percentile)")
  expect_equal(translate_stat_string("{mean} ± {sd}"), "Mean ± SD") 
  expect_equal(translate_stat_string("{mean} ({min}–{max})"), "Mean (Minimum–Maximum)") 
})
