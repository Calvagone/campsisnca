
library(testthat)
library(campsisnca)
library(campsis)
library(dplyr)
library(gtsummary)
library(gt)

context("Test all functionalities presented in the README")

source(paste0("C:/prj/campsisnca/tests/testthat/", "testUtils.R"))

campsis <- generateData1()

test_that("PK metrics at Day 1 and Day 7 (example 1) can be reproduced", {
  
  # Day 1
  ncaD1 <- NCAMetrics(x=campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1")) %>%
    add(c(AUC(unit="ng/mL*h"), Cmax(unit="ng/mL"), Tmax(unit="h"), Ctrough(unit="ng/mL"))) %>%
    campsisnca::calculate()
  
  # Day 7 
  ncaD7 <- NCAMetrics(x=campsis %>% timerange(144, 168, rebase=TRUE), variable="Y", scenario=c(day="Day 7")) %>%
    add(c(AUC(), Cmax(), Tmax(), Ctrough())) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable()  
  table <- table %>%
    add(c(ncaD1, ncaD7))
  
  summary <- table %>%
    export(dest="dataframe")
  
  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    select(-discrete_value) %>%
    filter(id %in% c(1,2,3)) # Keep first 3
  
  outputRegressionTest(data=summary, filename="example1_summary")
  outputRegressionTest(data=individual, filename="example1_individual")

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, "readme_example1")
})

test_that("PK metrics at Day 1 and Day 7 for different body weight ranges (example 2) can be reproduced", {
  
  campsis_bw_50_75 <- campsis %>% filter(BW > 50 & BW < 75)
  campsis_bw_75_100 <- campsis %>% filter(BW >= 75 & BW < 100)
  
  scenarioD1_a <- c(day="Day 1", bw_range="BW range: 50-75")
  ncaD1_a <- NCAMetrics(x=campsis_bw_50_75 %>% timerange(0, 24), variable="Y", scenario=scenarioD1_a) %>% 
    add(c(AUC(unit="ng/mL*h"), Cmax(unit="ng/mL"), Tmax(unit="h"), Ctrough(unit="ng/mL"))) %>%
    campsisnca::calculate()
  
  scenarioD7_a <- c(day="Day 7", bw_range="BW range: 50-75")
  ncaD7_a <- NCAMetrics(x=campsis_bw_50_75 %>% timerange(144, 168, rebase=T), variable="Y", scenario=scenarioD7_a) %>%
    add(c(AUC(), Cmax(), Tmax(), Ctrough())) %>%
    campsisnca::calculate()
  
  scenarioD1_b <- c(day="Day 1", bw_range="BW range: 75-100")
  ncaD1_b <- NCAMetrics(x=campsis_bw_75_100 %>% timerange(0, 24), variable="Y", scenario=scenarioD1_b) %>%
    add(c(AUC(), Cmax(), Tmax(), Ctrough())) %>%
    campsisnca::calculate()
  
  scenarioD7_b <- c(day="Day 7", bw_range="BW range: 75-100")
  ncaD7_b <- NCAMetrics(x=campsis_bw_75_100 %>% timerange(144, 168, rebase=T), variable="Y", scenario=scenarioD7_b) %>%
    add(c(AUC(), Cmax(), Tmax(), Ctrough())) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable() %>%
    add(c(ncaD1_a, ncaD7_a, ncaD1_b, ncaD7_b))
  
  summary <- table %>%
    export(dest="dataframe")
  
  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    select(-discrete_value) %>%
    filter(id %in% c(1,2,3)) # Keep first 3
  
  outputRegressionTest(data=summary, filename="example2_summary")
  outputRegressionTest(data=individual, filename="example2_individual")

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, "readme_example2")
})

test_that("campsisnca::calculate 2-compartment half-life metrics (example 3) can be reproduced", {
  
  nca <- NCAMetrics(x=campsis %>% mutate(DOSE=1000, TAU=24), variable="Y") %>%
    add(c(Thalf.2cpt.dist(), Thalf.2cpt.eff(), Thalf.2cpt.z())) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable() %>%
    add(nca)

  summary <- table %>%
    export(dest="dataframe")
  
  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    select(-discrete_value) %>%
    filter(id %in% c(1,2,3)) # Keep first 3
  
  outputRegressionTest(data=summary, filename="example3_summary")
  outputRegressionTest(data=individual, filename="example3_individual")
  
  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, "readme_example3")
})

test_that("Compute terminal half-live based on data (example 4) can be reproduced", {
  
  nca <- NCAMetrics(x=campsis, variable="Y") %>%
    add(c(Thalf(x=campsis %>% timerange(7*24, 10*24)))) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable() %>%
    add(nca)
  
  summary <- table %>%
    export(dest="dataframe")
  
  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    select(-discrete_value) %>%
    filter(id %in% c(1,2,3)) # Keep first 3
  
  outputRegressionTest(data=summary, filename="example4_summary")
  outputRegressionTest(data=individual, filename="example4_individual")
  
  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, "readme_example4")
})

test_that("Round your PK metrics (example 5)", {
  
  # Day 1
  ncaD1 <- NCAMetrics(x=campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1")) %>%
    add(AUC(digits=~style_sigfig(.x, 2), name="AUC1")) %>% # At least 2 significant figures (default in gtsummary)
    add(AUC(digits=c(1,2,2), name="AUC2")) %>% # Respectively 1/2/2 digit(s) after decimal for med, p5 and p95
    add(AUC(digits=~signif(.x, 2), name="AUC3")) %>% # 2 significant digits only
    add(AUC(digits=list(~round(.x/5)*5, ~round(.x, 1) , ~style_number(.x)), name="AUC4")) %>% # 1 specific function for med, p5 and p95
    campsisnca::calculate()
  
  # Day 7 
  ncaD7 <- NCAMetrics(x=campsis %>% timerange(144, 168, rebase=TRUE), variable="Y", scenario=c(day="Day 7")) %>%
    add(AUC(name="AUC1")) %>%
    add(AUC(name="AUC2")) %>%
    add(AUC(name="AUC3")) %>%
    add(AUC(name="AUC4")) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable()  
  table <- table %>%
    add(c(ncaD1, ncaD7))

  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, "readme_example5")
})


test_that("Export custom metrics (example 6)", {
  
  custom1 <- CustomMetric(fun=~Cmax() %>% iValue(.x, .y), name="Cmax custom", unit="ng/mL")
  custom2 <- CustomMetric(fun=~(Cmax() %>% iValue(.x, .y)) > 12, name="Cmax > 12", unit="%", categorical=TRUE)
  
  # Day 1
  ncaD1 <- NCAMetrics(x=campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1")) %>%
    add(c(Cmax(unit="ng/mL"), Tmax(unit="h"), custom1, custom2)) %>%
    campsisnca::calculate()
  
  # Day 7 
  ncaD7 <- NCAMetrics(x=campsis %>% timerange(144, 168, rebase=TRUE), variable="Y", scenario=c(day="Day 7")) %>%
    add(c(Cmax(), Tmax(), custom1, custom2)) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable()  
  table <- table %>%
    add(c(ncaD1, ncaD7))
  
  summary <- table %>%
    export(dest="dataframe")
  
  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    filter(id %in% c(1,2,3)) # Keep first 3
  
  outputRegressionTest(data=summary, filename="example6_summary")
  outputRegressionTest(data=individual, filename="example6_individual")
  
  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, "readme_example6")
  
  gttable <- table %>% export(dest="gt", subscripts=TRUE, all_dichotomous_levels=TRUE)
  gtTableRegressionTest(gttable, "readme_example6_all_levels")
})

test_that("Geometric Mean / Geometric CV (example 7)", {
  
  nca <- NCAMetrics(x=campsis, variable="Y") %>%
    add(c(AUC(unit="ng/mL*h", stat_display="{geomean} ({geocv}%)"), Cavg(unit="ng/mL", stat_display="{geomean} ({geocv}%)"))) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable() %>%
    add(nca)
  
  summary <- table %>%
    export(dest="dataframe")
  
  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    select(-discrete_value) %>%
    filter(id %in% c(1,2,3)) # Keep first 3
  
  outputRegressionTest(data=summary, filename="example7_summary")
  outputRegressionTest(data=individual, filename="example7_individual")
  
  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, "readme_example7")
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
  ncaD1 <- NCAMetrics(x=campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1")) %>%
    add(c(Cmax(unit="ng/mL"), CustomMetric(fun=getCategory, name="Cmax categories", unit="%", categorical=TRUE))) %>%
    campsisnca::calculate()
  
  # Day 7 
  ncaD7 <- NCAMetrics(x=campsis %>% timerange(144, 168, rebase=TRUE), variable="Y", scenario=c(day="Day 7")) %>%
    add(c(Cmax(), CustomMetric(fun=getCategory, name="Cmax categories", unit="%", categorical=TRUE))) %>%
    campsisnca::calculate()
  
  table <- NCAMetricsTable()  
  table <- table %>%
    add(c(ncaD1, ncaD7))
  
  summary <- table %>%
    export(dest="dataframe")
  
  individual <- table %>%
    export(dest="dataframe", type="individual_wide") %>%
    filter(id %in% seq_len(10)) # Keep first 10
  
  outputRegressionTest(data=summary, filename="example8_summary")
  outputRegressionTest(data=individual %>% rename(Categories=`Cmax categories`), filename="example8_individual")
  
  # Because there are 3 levels (and not 2), both table below are exactly similar
  gttable <- table %>% export(dest="gt", subscripts=TRUE)
  gtTableRegressionTest(gttable, "readme_example8")
  
  gttable <- table %>% export(dest="gt", subscripts=TRUE, all_dichotomous_levels=TRUE)
  gtTableRegressionTest(gttable, "readme_example8_all_levels")
  
})
