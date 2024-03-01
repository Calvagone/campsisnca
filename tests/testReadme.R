
library(testthat)
library(campsisnca)
library(campsis)
library(dplyr)
context("Test all functionalities presented in the README")

testFolder <- ""
source(paste0(testFolder, "testUtils.R"))

generateData <- function() {
  rich_sampling <- c(0,1,2,4,6,8,12,16,24)
  day1 <- rich_sampling
  day2day6 <- c(2,3,4,5,6)*24
  day7 <- rich_sampling + 6*24
  day8day10 <- c(8,9,10)*24
  
  ds <- Dataset(200) %>%
    add(Bolus(time=(0:6)*24, amount=1000)) %>%
    add(Observations(times=c(day1, day2day6, day7, day8day10))) %>%
    add(Covariate("BW", UniformDistribution(50,100)))
  
  model <- model_suite$testing$nonmem$advan4_trans4
  cl <- model %>% find(Equation("CL"))
  model <- model %>% replace(Equation("CL", paste0(cl@rhs, "*pow(BW/70, 0.75)")))
  
  campsis <- model %>% simulate(dataset=ds, dest="rxode2", seed=1, outvars="BW")
  return(campsis)
}

campsis <- generateData()

test_that("PK metrics at Day 1 and Day 7 (example 1) can be reproduced", {
  
  # Day 1
  ncaD1 <- NCAMetrics(x=campsis %>% timerange(0, 24), variable="Y", scenario=c(day="Day 1")) %>%
    add(c(Auc(unit="ng/mL*h"), Cmax(unit="ng/mL"), Tmax(unit="h"), Ctrough(unit="ng/mL"))) %>%
    calculate()
  
  # Day 7 
  ncaD7 <- NCAMetrics(x=campsis %>% timerange(144, 168, rebase=TRUE), variable="Y", scenario=c(day="Day 7")) %>%
    add(c(Auc(), Cmax(), Tmax(), Ctrough())) %>%
    calculate()
  
  table <- NCAMetricsTable(unitLineBreak=TRUE)  
  table <- table %>%
    add(c(ncaD1, ncaD7))
  
  summary <- table %>%
    export(dest="dataframe") %>%
    mutate(value=as.numeric(value)) # Remove names on values
  
  individual <- table %>%
    export(dest="dataframe", type="individual") %>%
    filter(id %in% c(1,2,3)) # Keep first 3
  
  outputRegressionTest(data=summary, output=c("metric", "stat", "value", "day"), filename="example1_summary")
  outputRegressionTest(data=individual, output=c("metric", "id", "value", "day"), filename="example1_individual")
})
