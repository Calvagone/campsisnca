library(campsis)

rich_sampling <- c(0, 1, 2, 4, 6, 8, 12, 16, 24)
day1 <- rich_sampling
day2day6 <- c(2, 3, 4, 5, 6) * 24
day7 <- rich_sampling + 6 * 24
day8day10 <- c(8, 9, 10) * 24

ds <- Dataset(200) %>%
  add(Bolus(time = (0:6) * 24, amount = 1000)) %>%
  add(Observations(times = c(day1, day2day6, day7, day8day10))) %>%
  add(Covariate("BW", UniformDistribution(50, 100)))

model <- model_suite$testing$nonmem$advan4_trans4

cl <- model %>%
  find(Equation("CL"))
model <- model %>%
  replace(Equation("CL", paste0(cl@rhs, "*pow(BW/70, 0.75)")))

# It should not matter if mrgsolve is used instead of rxode2
# Residual variability is now generated with base R on line 32
pk_bolus_md <- simulate(model = model, dataset = ds, seed = 1, outvars = c("BW", "CL", "V2", "Q", "V3", "KA"))

# Generate residual variability with base R (see issue #52)
set.seed(1)
pk_bolus_md$OBS_CP <- pk_bolus_md$CP * (rnorm(n = nrow(pk_bolus_md), mean = 0, sd = sqrt(0.025)) + 1)
pk_bolus_md$Y <- pk_bolus_md$OBS_CP

usethis::use_data(pk_bolus_md, overwrite = TRUE)
