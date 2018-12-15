
library(tidyverse)
library(broom)
library(dotwhisker)


load('models/explore-effects.Rdata')
load('data/interim/correctness.RData')

#' Engineer Features from dataset parameters
engineer_features <- function(dat) {
  dat %>% 
    mutate(
      # ePeriodsBaseline = log(0.5) / log(1 - baselineHazardRate),
      # expectedLifetimes = (maxPeriods / ePeriodsBaseline),
      # nClosedEnrollmentPeriods = maxPeriods - maxCohorts,
      # nOpenEnrollmentPeriods = maxPeriods - nClosedEnrollmentPeriods,
      # pctOpenEnrollmentPeriods = nOpenEnrollmentPeriods / maxPeriods,
      baselineHazardRate2 = baselineHazardRate ** 2,
      baselineHazardRate3 = baselineHazardRate ** 3,
      logBaselineHazardRate = log(baselineHazardRate),
      treatmentHazardRatio2 = treatmentHazardRatio **2,
      treatmentHazardRatio3 = treatmentHazardRatio ** 3,
      logTreatmentHazardRatio = log(treatmentHazardRatio),
      cohortSize2 = cohortSize ** 2,
      cohortSize3 = cohortSize ** 3,
      logCohortSize = log(cohortSize),
      expectedLifetimes2 = expectedLifetimes ** 2,
      expectedLifetimes3 = expectedLifetimes ** 3,
      logExpectedLifetimes = log(expectedLifetimes),
      pctOpenEnrollmentPeriods2 = pctOpenEnrollmentPeriods ** 2,
      pctOpenEnrollmentPeriods3 = pctOpenEnrollmentPeriods ** 3,
      logPctOpenEnrollmentPeriods = log(pctOpenEnrollmentPeriods)
    )
}

# datapoints <- correctness %>% engineer_features()

# Dot-Whisker -------------------------------------------------------------

m1 <- tidy(linear_effects) %>% 
  filter(term != "(Intercept)") %>% 
  mutate(model = "Linear")

dwplot(
    m1,
    vline = geom_vline(xintercept = 0, colour = "grey60", linetype = 2)
  ) %>%
  relabel_predictors(c(
     'baselineHazardRate' = 'Baseline Hazard',
     'treatementHazardRatio' = 'Treatment HR',
     'cohortSize' = 'Cohort Size',
     'expectedLifetimes' = 'Expected Lifetimes',
     'pctOpenEnrollmentPeriods' = '% Open Enrollment'
  )) + 
  theme_minimal()




# Baseline Hazard ---------------------------------------------------------

plotdat <- expand.grid(
  baselineHazardRate=seq(0.0001, 0.05, 0.0001),
  treatmentHazardRatio=0.5,
  expectedLifetimes=0.5,
  pctOpenEnrollmentPeriods=0.5,
  cohortSize=8
) %>% 
  engineer_features() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Log = predict.glm(log_effects, newdata=., type='response')
  ) %>% 
  select(baselineHazardRate, Linear, Quadratic, Cubic, Log) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Log
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Log'))
  )


gg <- ggplot(plotdat) +
  geom_line(aes(baselineHazardRate, y = modelPrediction, group=model, color=model), size=1) +
  theme_minimal() + 
  theme(
    legend.position="top",
    legend.background = element_rect(
      size=0.5,
      linetype="solid",
      colour ="black"
    )
  ) +
  scale_color_discrete(name="Model") +
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0, .1, .01),
    minor_breaks = seq(0 , .1, .005)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, .2),
    minor_breaks = seq(0 , 1, .1),
    limits = c(0, 1)) +
  labs(
    x = 'Baseline Hazard Rate',
    y = 'Predicted Power',
    title = 'Effect of Baseline Hazard Rate on Predicted Power',
    caption = "Evaluated at Treatment HR = 0.5; Expected Lifetimes = 0.5; % Open Enrollment = 50%; Cohort Size = 8"
  )

ggsave(
    filename = 'reports/figures/power-baseline-hazard.png',
    plot = gg, 
    units = 'mm',
    width = 400,
    height = 300,
    scale = 0.4
  )



# Treatment Hazard Ratio ---------------------------------------------------------

plotdat <- expand.grid(
  baselineHazardRate=0.05,
  treatmentHazardRatio=seq(0.0001, 0.9999, 0.0001),
  expectedLifetimes=2,
  pctOpenEnrollmentPeriods=0.5,
  cohortSize=8
) %>% 
  engineer_features() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Log = predict.glm(log_effects, newdata=., type='response')
  ) %>% 
  select(treatmentHazardRatio, Linear, Quadratic, Cubic, Log) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Log
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Log'))
  )


gg <- ggplot(plotdat) +
  geom_line(aes(treatmentHazardRatio, y = modelPrediction, group=model, color=model), size=1) +
  theme_minimal() + 
  theme(
    legend.position="top",
    legend.background = element_rect(
      size=0.5,
      linetype="solid",
      colour ="black"
    )
  ) +
  scale_color_discrete(name="Model") +
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, .2),
    minor_breaks = seq(0 , 1, .1)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, .2),
    minor_breaks = seq(0 , 1, .1),
    limits = c(0, 1)) +
  labs(
    x = 'Treatment Hazard Ratio',
    y = 'Predicted Power',
    title = 'Effect of Treatment Effect Size on Predicted Power',
    caption = "Evaluated at Basline Hazard = 0.05; Expected Lifetimes = 0.5; % Open Enrollment = 50%; Cohort Size = 8"
  )

ggsave(
  filename = 'reports/figures/power-treatment-hr.png',
  plot = gg, 
  units = 'mm',
  width = 400,
  height = 300,
  scale = 0.4
)


# Expected Lifetimes ------------------------------------------------------

plotdat <- expand.grid(
  baselineHazardRate=0.5,
  treatmentHazardRatio=0.5,
  expectedLifetimes=seq(0, 3, 0.001),
  pctOpenEnrollmentPeriods=0.5,
  cohortSize=8
) %>% 
  engineer_features() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Log = predict.glm(log_effects, newdata=., type='response')
  ) %>% 
  select(expectedLifetimes, Linear, Quadratic, Cubic, Log) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Log
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Log'))
  )


gg <- ggplot(plotdat) +
  geom_line(aes(expectedLifetimes, y = modelPrediction, group=model, color=model), size=1) +
  theme_minimal() + 
  theme(
    legend.position="top",
    legend.background = element_rect(
      size=0.5,
      linetype="solid",
      colour ="black"
    )
  ) +
  scale_color_discrete(name="Model") +
  scale_x_continuous(
    # labels = scales::percent,
    breaks = seq(0, 3, 0.5),
    minor_breaks = seq(0 , 3, 0.25)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, .2),
    minor_breaks = seq(0 , 1, .1),
    limits = c(0, 1)) +
  labs(
    x = 'Expected Lifetimes',
    y = 'Predicted Power',
    title = 'Effect of Study Length on Predicted Power',
    caption = "Evaluated at Baseline Hazard = 0.05; Treatment HR = 0.5; % Open Enrollment = 50%; Cohort Size = 8"
  )

ggsave(
  filename = 'reports/figures/power-expected-lifetimes.png',
  plot = gg, 
  units = 'mm',
  width = 400,
  height = 300,
  scale = 0.4
)


# Cohort Size -------------------------------------------------------------

plotdat <- expand.grid(
  baselineHazardRate=0.05,
  treatmentHazardRatio=0.5,
  expectedLifetimes=0.5,
  pctOpenEnrollmentPeriods=0.5,
  cohortSize=seq(32)
) %>% 
  engineer_features() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Log = predict.glm(log_effects, newdata=., type='response')
  ) %>% 
  select(cohortSize, Linear, Quadratic, Cubic, Log) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Log
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Log'))
  )


gg <- ggplot(plotdat) +
  geom_line(aes(cohortSize, y = modelPrediction, group=model, color=model), size=1) +
  theme_minimal() + 
  theme(
    legend.position="top",
    legend.background = element_rect(
      size=0.5,
      linetype="solid",
      colour ="black"
    )
  ) +
  scale_color_discrete(name="Model") +
  scale_x_continuous(
    # labels = scales::percent,
    breaks = seq(0, 32, 4),
    minor_breaks = seq(0, 32, 2)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, .2),
    minor_breaks = seq(0 , 1, .1),
    limits = c(0, 1)) +
  labs(
    x = 'Cohort Size',
    y = 'Predicted Power',
    title = 'Effect of Cohort Size on Predicted Power',
    caption = "Evaluated at Baseline Hazard = 0.05; Treatment HR = 0.5; Expected Lifetimes = 1; % Open Enrollment = 50%"
  )

ggsave(
  filename = 'reports/figures/power-cohort-size.png',
  plot = gg, 
  units = 'mm',
  width = 400,
  height = 300,
  scale = 0.4
)

# % Open Enrollment Periods -------------------------------------------------------------

plotdat <- expand.grid(
  baselineHazardRate=0.01,
  treatmentHazardRatio=0.5,
  expectedLifetimes=0.5,
  pctOpenEnrollmentPeriods=seq(0.001, 1, 0.001),
  cohortSize=16
) %>% 
  engineer_features() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Log = predict.glm(log_effects, newdata=., type='response')
  ) %>% 
  select(pctOpenEnrollmentPeriods, Linear, Quadratic, Cubic, Log) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Log
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Log'))
  )


gg <- ggplot(plotdat) +
  geom_line(aes(pctOpenEnrollmentPeriods, y = modelPrediction, group=model, color=model), size=1) +
  theme_minimal() + 
  theme(
    legend.position="top",
    legend.background = element_rect(
      size=0.5,
      linetype="solid",
      colour ="black"
    )
  ) +
  scale_color_discrete(name="Model") +
  scale_x_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, 0.2),
    minor_breaks = seq(0, 1, 0.1)) +
  scale_y_continuous(
    labels = scales::percent,
    breaks = seq(0, 1, .2),
    minor_breaks = seq(0 , 1, .1),
    limits = c(0, 1)) +
  labs(
    x = '% Open Enrollment Periods',
    y = 'Predicted Power',
    title = 'Effect of New Cohorts on Predicted Power',
    caption = "Evaluated at Baseline Hazard = 0.01; Treatment HR = 0.5; Expected Lifetimes = 0.5; Cohort Size = 16"
  )

ggsave(
  filename = 'reports/figures/power-pct-open-enrollment.png',
  plot = gg, 
  units = 'mm',
  width = 400,
  height = 300,
  scale = 0.4
)










