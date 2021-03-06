
#' Visualize single effects of each model
#' The idea here to decide which effects we'll use in the mixed model

library(tidyverse)


load('models/explore-effects.Rdata')
load('data/interim/correctness.RData')
source('src/features/engineer-features.R')

between_vec <- Vectorize(dplyr::between)

# correctness %>% engineer_features()

# Baseline Hazard ---------------------------------------------------------

plotdat <- expand.grid(
  baselineHazardRate=seq(0.0001, 0.5, 0.0001),
  treatmentHazardRatio=0.5,
  expectedLifetimes=0.5,
  pctOpenEnrollmentPeriods=0.5,
  cohortSize=8
) %>% 
  calculate_model_params() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Diminishing = predict.glm(diminishing_effects, newdata=., type='response')
  ) %>% 
  select(baselineHazardRate, Linear, Quadratic, Cubic, Diminishing) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Diminishing
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Diminishing'))
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
    breaks = seq(0, .5, .1),
    minor_breaks = seq(0, .5, .05)) +
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
    filename = 'reports/figures/single-effects/power-baseline-hazard.png',
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
  expectedLifetimes=1,
  pctOpenEnrollmentPeriods=0.5,
  cohortSize=8
) %>% 
  calculate_model_params() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Diminishing = predict.glm(diminishing_effects, newdata=., type='response')
  ) %>% 
  select(treatmentHazardRatio, Linear, Quadratic, Cubic, Diminishing) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Diminishing
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Diminishing'))
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
    caption = "Evaluated at Basline Hazard = 0.05; Expected Lifetimes = 1; % Open Enrollment = 50%; Cohort Size = 8"
  )

ggsave(
  filename = 'reports/figures/single-effects/power-treatment-hr.png',
  plot = gg, 
  units = 'mm',
  width = 400,
  height = 300,
  scale = 0.4
)


# Expected Lifetimes ------------------------------------------------------

plotdat <- expand.grid(
  baselineHazardRate=0.05,
  treatmentHazardRatio=0.5,
  expectedLifetimes=seq(0, 3, 0.001),
  pctOpenEnrollmentPeriods=0.5,
  cohortSize=8
) %>% 
  calculate_model_params() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Diminishing = predict.glm(diminishing_effects, newdata=., type='response')
  ) %>% 
  select(expectedLifetimes, Linear, Quadratic, Cubic, Diminishing) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Diminishing
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Diminishing'))
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
  filename = 'reports/figures/single-effects/power-expected-lifetimes.png',
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
  calculate_model_params() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Diminishing = predict.glm(diminishing_effects, newdata=., type='response')
  ) %>% 
  select(cohortSize, Linear, Quadratic, Cubic, Diminishing) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Diminishing
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Diminishing'))
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
    caption = "Evaluated at Baseline Hazard = 0.05; Treatment HR = 0.5; Expected Lifetimes = 0.5; % Open Enrollment = 50%"
  )

ggsave(
  filename = 'reports/figures/single-effects/power-cohort-size.png',
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
  cohortSize=32
) %>% 
  calculate_model_params() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Diminishing = predict.glm(diminishing_effects, newdata=., type='response')
  ) %>% 
  select(pctOpenEnrollmentPeriods, Linear, Quadratic, Cubic, Diminishing) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Diminishing
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Diminishing'))
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
  filename = 'reports/figures/single-effects/power-pct-open-enrollment.png',
  plot = gg, 
  units = 'mm',
  width = 400,
  height = 300,
  scale = 0.4
)



# mixed open enrollment ---------------------------------------------------

plotdat <- expand.grid(
  baselineHazardRate=0.01,
  treatmentHazardRatio=0.5,
  expectedLifetimes=c(0.5, 1, 2),
  pctOpenEnrollmentPeriods=seq(0.001, 1, 0.001),
  cohortSize=32
) %>% 
  calculate_model_params() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Diminishing = predict.glm(diminishing_effects, newdata=., type='response'),
    Mixed = predict.glm(mixed_effects, newdata=., type='response')
  ) %>% 
  select(pctOpenEnrollmentPeriods, expectedLifetimes, Linear, Quadratic, Cubic, Diminishing, Mixed) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Diminishing, Mixed
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Diminishing', 'Mixed'))
  ) %>% 
  filter(model %in% c('Quadratic', 'Diminishing', 'Mixed'))


ggplot(plotdat) +
  geom_line(aes(pctOpenEnrollmentPeriods, y = modelPrediction, group=expectedLifetimes, color=expectedLifetimes), size=1) +
  facet_wrap(~model) + 
  theme_minimal() + 
  theme(
    legend.position="top",
    legend.background = element_rect(
      size=0.5,
      linetype="solid",
      colour ="black"
    )
  ) +
  scale_color_continuous(
    name="Expected Lifetimes",
    breaks = c(0.5, 1, 2)
  ) +
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
