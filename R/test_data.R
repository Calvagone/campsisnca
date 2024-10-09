
#' Generate fictitious simulation data for campsisnca README and tests.
#' 
#' @return simulation results
#' @importFrom campsis Dataset Bolus Observations Covariate simulate
#' @importFrom campsismod find replace 
#' @export
generateTestData <- function() {
  rich_sampling <- c(0,1,2,4,6,8,12,16,24)
  day1 <- rich_sampling
  day2day6 <- c(2,3,4,5,6)*24
  day7 <- rich_sampling + 6*24
  day8day10 <- c(8,9,10)*24
  
  ds <- campsis::Dataset(200) %>%
    add(campsis::Bolus(time=(0:6)*24, amount=1000)) %>%
    add(campsis::Observations(times=c(day1, day2day6, day7, day8day10))) %>%
    add(campsis::Covariate("BW", campsis::UniformDistribution(50,100)))
  
  model <- campsismod::model_suite$testing$nonmem$advan4_trans4
  cl <- model %>% campsismod::find(Equation("CL"))
  model <- model %>% campsismod::replace(Equation("CL", paste0(cl@rhs, "*pow(BW/70, 0.75)")))
  
  # It should not matter if mrgsolve is used instead of rxode2
  # Residual variability is now generated with base R on line 32
  campsis <- model %>% campsis::simulate(dataset=ds, dest="mrgsolve", seed=1, outvars=c("BW", "CL", "V2", "Q", "V3", "KA"))
  
  # Generate residual variability with base R (see issue #52)
  set.seed(1)
  campsis$OBS_CP <- campsis$CP*(rnorm(n=nrow(campsis), mean=0, sd=sqrt(0.025)) + 1)
  campsis$Y <- campsis$OBS_CP
  
  # eps <- campsis$OBS_CP / campsis$CP - 1
  # eps <- eps[is.finite(eps)]
  # var(eps)
  
  return(campsis)
}