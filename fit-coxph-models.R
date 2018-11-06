library(dplyr)
library(purrr)
library(ggplot2)
library(survival)

source('./generate-simulations.R')

# Generate simulations ----------------------------------------------------

x <- 2**seq(6)

sims <- generate_simulations(
  bhrs = 1 / x,
  thrs = 1 - (1 / x),
  mps  = x,
  mcs  = x,
  css  = x,
  rss  = seq(100)
)

params <- sims$params %>% 
  rename(
    baselineHazardRate = baseline_hazard_rate,
    treatmentHazardRatio = treatment_hazard_ratio,
    maxPeriods = max_periods,
    maxCohorts = max_cohorts,
    cohortSize = cohort_size,
    randomState = random_state,
    simulationId = simulation_id
  )
sims_df <- sims$sims_df


# Fit Models --------------------------------------------------------------
# This should take a few minutes to run. 
models <- sims_df %>%
  split(.$simulationId) %>% # from base R
  map(~ (coxph(Surv(nPeriods, isDead) ~ isTreated, data=.)))

model_results <- models %>% 
  map(~ coef(summary(.))) %>%
  lapply(as_data_frame) %>% 
  data.table::rbindlist() %>% 
  as_data_frame() %>% 
  mutate(simulationId = seq(nrow(params))) %>% 
  rename(
    logHr = `coef`,
    hr = `exp(coef)`,
    logSe = `se(coef)`,
    p = `Pr(>|z|)`
  )

coxph_results <- params %>% 
  inner_join(model_results, by='simulationId') %>% 
  select(simulationId, everything())

save(coxph_results, file = './data/coxph_results.Rdata')
