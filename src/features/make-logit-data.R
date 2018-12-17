

library(tidyverse)

source('src/features/engineer-features.R')
between_vec <- Vectorize(dplyr::between)

load('data/interim/coxph_simulations.RData')
load('data/interim/correctness.RData')

set.seed(1234)
correctness <- correctness %>% 
  group_by(correct) %>%
  sample_n(5, replace=FALSE) %>%
  ungroup() 


logit_dat <- coxph_simulations %>%
  drop_na() %>%
  arrange(simulationId) %>%
  inner_join(
    correctness,
    by = c(
      "baselineHazardRate",
      "treatmentHazardRatio",
      "maxPeriods",
      "maxCohorts",
      "cohortSize"
    )
  ) %>%
  mutate(
    ePeriodsBaseline = log(0.5) / log(1 - baselineHazardRate),
    expectedLifetimes = (maxPeriods / ePeriodsBaseline),
    nClosedEnrollmentPeriods = maxPeriods - maxCohorts,
    nOpenEnrollmentPeriods = maxPeriods - nClosedEnrollmentPeriods,
    pctOpenEnrollmentPeriods = nOpenEnrollmentPeriods / maxPeriods,
    lConfInt = exp(logHr - qnorm(0.975) * logSe),
    uConfInt = exp(logHr + qnorm(0.975) * logSe),
    rejectH0 = !between_vec(1, lConfInt, uConfInt),
    isCorrect = rejectH0 & uConfInt < 1
  ) %>%
  select(
    isCorrect,
    expectedLifetimes,
    pctOpenEnrollmentPeriods,
    cohortSize,
    baselineHazardRate,
    treatmentHazardRatio
    # maxPeriods,
    # maxCohorts,
    # randomState
  ) %>% 
  calculate_model_params()

save(logit_dat, file = 'data/processed/logit_dat.Rdata')
