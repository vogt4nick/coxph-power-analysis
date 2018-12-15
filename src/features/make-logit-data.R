
library(tidyverse)


load('./data/interim/coxph_simulations.RData')
load('./data/interim/correctness.RData')
between_vec <- Vectorize(dplyr::between)


logit_dat <- coxph_simulations %>% 
  drop_na() %>% 
  arrange(simulationId) %>% 
  inner_join(
    correctness,
    by = c("baselineHazardRate", "treatmentHazardRatio", "maxPeriods", "maxCohorts", "cohortSize")
  )

save(logit_dat, file='data/processed/logit_dat.Rdata')
