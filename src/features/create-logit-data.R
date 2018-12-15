
library(tidyverse)

load('./data/interim/coxph_simulations.RData')
between_vec <- Vectorize(dplyr::between)


coxph_simulations %>% 
  filter(!is.na(p)) %>%
  mutate(
    ePeriodsBaseline = log(0.5) / log(1-baselineHazardRate),
    lConfInt = exp(logHr-qnorm(0.975)*logSe),
    uConfInt = exp(logHr+qnorm(0.975)*logSe),
    rejectH0 = !between_vec(1, lConfInt, uConfInt),
    isCorrect = ifelse(rejectH0 & uConfInt < 1, 'correct', 'notCorrect')
  ) %>% 
  group_by(baselineHazardRate, treatmentHazardRatio, maxPeriods, maxCohorts, cohortSize, isCorrect) %>%
  summarize(
    count = n()
  ) %>%
  tidyr::spread(isCorrect, count, fill=0) %>% 
  mutate(
    power = correct / (correct + notCorrect)
  ) %>% 
  group_by(correct) %>%
  sample_n(5, replace=TRUE) %>%
  ungroup()