

library(tidyverse)

source('src/features/engineer-features.R')

load('./data/interim/coxph_simulations.RData')
between_vec <- Vectorize(dplyr::between)


correctness <- coxph_simulations %>% 
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
  ) 

gg <- ggplot(correctness) +
  geom_histogram(aes(power), breaks = seq(-0.025, 1.025, 0.05), color='white') +
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, .1),
    minor_breaks = seq(0, 1, .05)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    minor_breaks = seq(0, 100, 10)
  ) +
  theme_minimal() +
  labs(
    x = 'Observed Power',
    y = 'Bin Frequency',
    title = 'Observed Power of Simulated Cox-PH Models'
  )

ggsave(
    filename = 'reports/figures/simulation-histogram.png',
    plot = gg, 
    units = 'mm',
    width = 400,
    height = 300,
    scale = 0.4
  )
