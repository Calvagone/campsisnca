
library(testthat)
library(campsisnca)
library(campsis)
library(dplyr)
library(gtsummary)
library(gt)

context("Test all functionalities presented in the README")

testFolder <-  file.path(getwd(), test_path())
source(file.path(testFolder, "testUtils.R"))

campsis <- generateTestData()

getRefFile <- function(filename) {
  return(file.path(testFolder, "non_regression", filename))
}

test_that("PK metrics at Day 1 and Day 7 (example 1) can be reproduced", {

  # Day 1
  ncaD1 <- NCAAnalysis(name="Day 1", window=TimeWindow(0, 24), variable="Y") %>%
    add(c(AUC(unit="ng/mL*h"), Cmax(unit="ng/mL"), Tmax(unit="h"), Ctrough(unit="ng/mL")))

  # Day 7
  ncaD7 <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="Y") %>%
    add(c(AUC(), Cmax(), Tmax(), Ctrough()))

  table <- NCAMetricsTable()
  table <- table %>%
    add(c(ncaD1, ncaD7)) %>%
    campsisnca::calculate(campsis)

  summary <- table %>%
    export(dest="dataframe")

  summary_wide <- table %>%
    export(dest="dataframe", type="summary_wide")

  summary_pretty <- table %>%
    export(dest="dataframe", type="summary_pretty")

  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    select(-discrete_value) %>%
    filter(id %in% c(1,2,3)) # Keep first 3

  outputRegressionTest(data=summary, file=getRefFile("example1_summary.csv"))
  outputRegressionTest(data=summary_wide, file=getRefFile("example1_summary_wide.csv"))
  outputRegressionTest(data=summary_pretty, file=getRefFile("example1_summary_pretty.csv"))
  outputRegressionTest(data=individual, file=getRefFile("example1_individual.csv"))

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example1.html"))
})

test_that("PK metrics at Day 1 and Day 7 for different body weight ranges (example 2) can be reproduced", {

  campsis_ <- campsis %>%
    mutate(Scenario=ifelse(BW >= 75, ">=75kg patients", "<75kg patients"))

  day1 <- NCAAnalysis(name="Day 1", window=TimeWindow(0, 24), variable="Y") %>%
    add(c(AUC(unit="ng/mL*h"), Cmax(unit="ng/mL"), Tmax(unit="h"), Ctrough(unit="ng/mL")))

  day7 <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="Y") %>%
    add(c(AUC(), Cmax(), Tmax(), Ctrough()))

  table <- NCAMetricsTable() %>%
    add(c(day1, day7)) %>%
    campsisnca::calculate(campsis_, strat_vars="Scenario")

  summary <- table %>%
    export(dest="dataframe")

  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    select(-discrete_value) %>%
    filter(id %in% c(1,2,3)) # Keep first 3

  outputRegressionTest(data=summary %>% dplyr::arrange(Scenario), file=getRefFile("example2_summary.csv"))
  outputRegressionTest(data=individual, file=getRefFile("example2_individual.csv"))

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example2.html"))
})

test_that("campsisnca::calculate 2-compartment half-life metrics (example 3) can be reproduced", {

  nca <- NCAAnalysis(variable="Y") %>%
    add(c(Thalf.2cpt.dist(), Thalf.2cpt.eff(), Thalf.2cpt.z()))

  table <- NCAMetricsTable() %>%
    add(nca) %>%
    campsisnca::calculate(campsis %>% mutate(DOSE=1000, TAU=24))

  summary <- table %>%
    export(dest="dataframe")

  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    select(-discrete_value) %>%
    filter(id %in% c(1,2,3)) # Keep first 3

  outputRegressionTest(data=summary, file=getRefFile("example3_summary.csv"))
  outputRegressionTest(data=individual, file=getRefFile("example3_individual.csv"))

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example3.html"))
})

test_that("Compute terminal half-live based on data (example 4) can be reproduced", {

  nca <- NCAAnalysis(variable="Y") %>%
    add(c(Thalf(window=TimeWindow(7*24, 10*24))))

  table <- NCAMetricsTable() %>%
    add(nca) %>%
    campsisnca::calculate(campsis)

  summary <- table %>%
    export(dest="dataframe")

  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    select(-discrete_value) %>%
    filter(id %in% c(1,2,3)) # Keep first 3

  outputRegressionTest(data=summary, file=getRefFile("example4_summary.csv"))
  outputRegressionTest(data=individual, file=getRefFile("example4_individual.csv"))

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example4.html"))
})

test_that("Round your PK metrics (example 5)", {

  # Day 1
  ncaD1 <- NCAAnalysis(name="Day 1", window=TimeWindow(0, 24), variable="Y") %>%
    add(AUC(digits=~style_sigfig(.x, 2), name="AUC1")) %>% # At least 2 significant figures (default in gtsummary)
    add(AUC(digits=c(1,2,2), name="AUC2")) %>% # Respectively 1/2/2 digit(s) after decimal for med, p5 and p95
    add(AUC(digits=~signif(.x, 2), name="AUC3")) %>% # 2 significant digits only
    add(AUC(digits=list(~round(.x/5)*5, ~round(.x, 1) , ~style_number(.x)), name="AUC4")) # 1 specific function for med, p5 and p95

  # Day 7
  ncaD7 <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="Y") %>%
    add(AUC(name="AUC1")) %>%
    add(AUC(name="AUC2")) %>%
    add(AUC(name="AUC3")) %>%
    add(AUC(name="AUC4"))

  table <- NCAMetricsTable()
  table <- table %>%
    add(c(ncaD1, ncaD7)) %>%
    campsisnca::calculate(campsis)

  summary <- table %>%
    export(dest="dataframe", type="summary_pretty")

  auc1 <- summary %>%
    filter(metric=="AUC1", analysis=="Day 1") %>%
    pull(summary_stats)

  auc2 <- summary %>%
    filter(metric=="AUC2", analysis=="Day 1") %>%
    pull(summary_stats)

  auc3 <- summary %>%
    filter(metric=="AUC3", analysis=="Day 1") %>%
    pull(summary_stats)

  auc4 <- summary %>%
    filter(metric=="AUC4", analysis=="Day 1") %>%
    pull(summary_stats)

  expect_equal(auc1, "134 (102–168)") # At least 2 significant figures
  expect_equal(auc2, "134 (102.03–167.51)") # 1/2/2 digits
  expect_equal(auc3, "130 (100–170)") # 2 first significant digits
  expect_equal(auc4, "135 (102–168)") # Specific functions, see above

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example5.html"))
})


test_that("Export custom metrics (example 6)", {

  custom1 <- CustomMetric(fun=~Cmax() %>% iValue(.x, .y), name="C_{max} custom", unit="ng/mL")
  custom2 <- CustomMetric(fun=~(Cmax() %>% iValue(.x, .y)) > 12, name="C_{max} > 12", unit="%", categorical=TRUE)

  # Day 1
  ncaD1 <- NCAAnalysis(name="Day 1", window=TimeWindow(0, 24), variable="Y") %>%
    add(c(Cmax(unit="ng/mL"), Tmax(unit="h"), custom1, custom2))

  # Day 7
  ncaD7 <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="Y") %>%
    add(c(Cmax(), Tmax(), custom1, custom2))

  table <- NCAMetricsTable()
  table <- table %>%
    add(c(ncaD1, ncaD7)) %>%
    campsisnca::calculate(campsis)

  summary <- table %>%
    export(dest="dataframe")

  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    filter(id %in% c(1,2,3)) # Keep first 3

  outputRegressionTest(data=summary, file=getRefFile("example6_summary.csv"))
  outputRegressionTest(data=individual, file=getRefFile("example6_individual.csv"))

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example6.html"))

  gttable <- table %>% export(dest="gt", subscripts=TRUE, all_dichotomous_levels=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example6_all_levels.html"))
})

test_that("Geometric Mean / Geometric CV (example 7)", {

  nca <- NCAAnalysis(variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", stat_display="{geomean} ({geocv}%)"), Cavg(unit="ng/mL", stat_display="{geomean} ({geocv}%)")))

  table <- NCAMetricsTable() %>%
    add(nca) %>%
    campsisnca::calculate(campsis)

  summary <- table %>%
    export(dest="dataframe")

  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    select(-discrete_value) %>%
    filter(id %in% c(1,2,3)) # Keep first 3

  outputRegressionTest(data=summary, file=getRefFile("example7_summary.csv"))
  outputRegressionTest(data=individual, file=getRefFile("example7_individual.csv"))

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example7.html"))
})

test_that("Stats on categorical data with more than 2 levels (example 8)", {

  getCategory <- function(.x, .y) {
    values <- Cmax() %>% iValue(.x, .y)
    retValue <- dplyr::case_when(
      values < 10 ~ "(1) < 10 ng/mL",
      values >= 10 & values <= 15 ~ "(2) 10-15 ng/mL",
      values > 15 ~ "(3) > 15 ng/mL",
    )
    return(retValue)
  }

  # Day 1
  ncaD1 <- NCAAnalysis(name="Day 1", window=TimeWindow(0, 24), variable="Y") %>%
    add(c(Cmax(unit="ng/mL"), CustomMetric(fun=getCategory, name="C_{max} categories", unit="%", categorical=TRUE)))

  # Day 7
  ncaD7 <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="Y") %>%
    add(c(Cmax(), CustomMetric(fun=getCategory, name="C_{max} categories", unit="%", categorical=TRUE)))

  table <- NCAMetricsTable()
  table <- table %>%
    add(c(ncaD1, ncaD7)) %>%
    campsisnca::calculate(campsis)

  summary <- table %>%
    export(dest="dataframe")

  individual <- table %>%
    export(dest="dataframe", type="individual_wide") %>%
    filter(id %in% seq_len(10)) # Keep first 10

  outputRegressionTest(data=summary, file=getRefFile("example8_summary.csv"))
  outputRegressionTest(data=individual %>% rename(Categories=`C_{max} categories`), file=getRefFile("example8_individual.csv"))

  # Because there are 3 levels (and not 2), both table below are exactly similar
  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example8.html"))

  gttable <- table %>% export(dest="gt", subscripts=TRUE, all_dichotomous_levels=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example8_all_levels.html"))

})

test_that("Summary statistics across simulation arms and scenarios (example 10)", {
  
  arm1 <- Arm(subjects=24, label="1g QD") %>%
    add(Bolus(time=0, amount=1000, compartment="ABS", ii=24, addl=6)) %>%
    add(Observations(seq(0,24,0.1), rep=DosingSchedule()))
  
  arm2 <- Arm(subjects=24, label="2g QD") %>%
    add(Bolus(time=0, amount=2000, compartment="ABS", ii=24, addl=6)) %>%
    add(Observations(seq(0,24,0.1), rep=DosingSchedule()))
  
  dataset <- Dataset() %>%
    add(c(arm1, arm2))
  
  scenario1 <- Scenario(name="Base scenario", model=~.x, dataset=~.x)
  scenario2 <- Scenario(name="Lower clearance", model=~.x %>%
                          replace(Theta(name="CL", value=2)), dataset=~.x)
  scenarios <- Scenarios() %>% add(c(scenario1, scenario2))
  results <- simulate(model=model_suite$pk$`2cpt_fo`, dataset=dataset, seed=1, dest="mrgsolve", scenarios=scenarios)
  
  shadedPlot(results, "CONC", colour="ARM", strat_extra="SCENARIO") +
    ggplot2::facet_wrap(~SCENARIO) +
    ggplot2::xlab("Time (h)") +
    ggplot2::ylab("Concentrations (ng/mL)") +
    ggplot2::labs(colour="Arm", fill="Arm")
  
  nca <- NCAAnalysis(name="Day 7", window=TimeWindow(144, 168), variable="CONC") %>%
    add(AUC(unit="ng/mL*h")) %>%
    add(Cmax(unit="ng/mL")) %>%
    add(CustomMetric(fun=~(Cmax() %>% iValue(.x, .y)) > 30, name="C_{max} > 30", unit="%", categorical=TRUE)) %>%
    add(Tmax(unit="h", digits=2)) %>%
    add(Ctrough(unit="ng/mL"))
  
  table <- NCAMetricsTable() %>%
    add(nca) %>%
    campsisnca::calculate(results, strat_vars=c("ARM", "SCENARIO"))
  
  summary <- table %>%
    export(dest="dataframe")
  
  individual <- table %>%
    export(dest="dataframe", type="summary_pretty")
  
  outputRegressionTest(data=summary, file=getRefFile("example10_summary.csv"))
  
  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, getRefFile("readme_example10.html"))
})
