library(testthat)
library(dplyr)
library(campsis)
context("Check computed half lives are OK when plotted on graph")

test_that("Test half life parameters on 1-compartment model ", {
  amount <- 1000
  model <- model_suite$testing$nonmem$advan2_trans1
  required <- thalf.1cpt.required()[!(thalf.1cpt.required() %in% c("DOSE", "TAU"))]
  
  times <- seq(0,144,by=0.1)
  dataset <- Dataset(1)
  dataset <- dataset %>% add(Bolus(time=0, amount=amount))
  dataset <- dataset %>% add(Observations(times=times))
  
  results <- model %>% disable("IIV") %>% simulate(dataset, dest="mrgsolve", seed=1, outvars=c("CP", required))
  
  nca <- NCAMetrics(x=results %>% mutate(DOSE=amount, TAU=24), variable="CP", scenario=c(xx="Half-lives"))
  nca <- nca %>% add(c(Thalf.1cpt(), Thalf(x=results %>% timerange(50,150)))) 
  nca <- nca %>% campsisnca::calculate()
  
  thalf.z <- nca@list[[1]]@individual$value
  expect_equal(thalf.z, 10.6638, tolerance=1e-4)
  
  # Thalf computed based on data
  thalf <- nca@list[[2]]@individual$value
  expect_equal(thalf.z, thalf, tolerance=1e-3)
  
  slopes <- data.frame(x=times, y=13*exp(-log(2)/thalf.z*times), type="z")
  
  shadedPlot(results, "CP") +
    ggplot2::geom_line(mapping=ggplot2::aes(x=x, y=y, colour=type, group=type), data=slopes) +
    ggplot2::scale_y_log10() +
    ggplot2::coord_cartesian(ylim=c(0.1,20))
})

test_that("Test half life parameters on 2-compartment model ", {
  amount <- 1000
  model <- model_suite$testing$nonmem$advan4_trans4
  model <- model %>% campsismod::replace(Theta(name="Q", value=5))
  model <- model %>% campsismod::replace(Theta(name="V3", value=100))
  required <- thalf.2cpt.required()[!(thalf.2cpt.required() %in% c("DOSE", "TAU"))]
  
  times <- seq(0,144,by=0.1)
  dataset <- Dataset(1)
  dataset <- dataset %>% add(Bolus(time=0, amount=amount))
  dataset <- dataset %>% add(Observations(times=times))
  
  results <- model %>% disable("IIV") %>% simulate(dataset, dest="mrgsolve", seed=1, outvars=c("CP", required))
  
  nca <- NCAMetrics(x=results %>% mutate(DOSE=amount, TAU=24), variable="CP", scenario=c(xx="Half-lives"))
  nca <- nca %>% add(c(Thalf.2cpt.dist(), Thalf.2cpt.eff(), Thalf.2cpt.z(), Thalf(x=results %>% timerange(50,150))))
  nca <- nca %>% campsisnca::calculate()
  
  thalf.dist <- nca@list[[1]]@individual$value
  expect_equal(thalf.dist, 4.477274, tolerance=1e-4)
  
  thalf.eff <- nca@list[[2]]@individual$value
  expect_equal(thalf.eff, 23.97703, tolerance=1e-4)
  
  thalf.z <- nca@list[[3]]@individual$value
  expect_equal(thalf.z, 34.33897, tolerance=1e-4)
  
  # Thalf computed based on data
  thalf <- nca@list[[4]]@individual$value
  expect_equal(thalf.z, thalf, tolerance=1e-2)
  
  slopes <- bind_rows(data.frame(x=times, y=20*exp(-log(2)/thalf.dist*times), type="dist"),
                     data.frame(x=times, y=4*exp(-log(2)/thalf.eff*times), type="eff"),
                     data.frame(x=times, y=2.8*exp(-log(2)/thalf.z*times), type="z"))
  
  shadedPlot(results, "CP") +
    ggplot2::geom_line(mapping=ggplot2::aes(x=x, y=y, colour=type, group=type), data=slopes) +
    ggplot2::scale_y_log10() +
    ggplot2::coord_cartesian(ylim=c(0.1,20))
})