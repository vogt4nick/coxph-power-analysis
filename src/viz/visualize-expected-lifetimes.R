
library(tidyverse)

load('models/explore-effects.Rdata')
load('data/interim/correctness.RData')
source('src/features/engineer-features.R')

between_vec <- Vectorize(dplyr::between)


plotdat <- expand.grid(
  baselineHazardRate=0.05,
  treatmentHazardRatio=0.5,
  expectedLifetimes=seq(0, 3, 0.001),
  pctOpenEnrollmentPeriods=0.5,
  cohortSize=2**seq(2, 5)
) %>% 
  calculate_model_params() %>% 
  mutate( # we're breaking camel-case convention b/c they'll be labels later
    Linear = predict.glm(linear_effects, newdata=., type='response'),
    Quadratic = predict.glm(quadratic_effects, newdata=., type='response'),
    Cubic = predict.glm(cubic_effects, newdata=., type='response'),
    Diminishing = predict.glm(diminishing_effects, newdata=., type='response')
  ) %>% 
  select(expectedLifetimes, cohortSize, Linear, Quadratic, Cubic, Diminishing) %>% 
  gather(
    key = 'model', value = 'modelPrediction', Linear, Quadratic, Cubic, Diminishing
  ) %>% 
  mutate(
    model = forcats::fct_relevel(model, c('Linear', 'Quadratic', 'Cubic', 'Diminishing'))
  ) %>% 
  filter(model %in% c('Diminishing', 'Quadratic'))


cc <- scales::seq_gradient_pal("blue", "Lab")(seq(0,1,length.out=4))

gg <- ggplot(plotdat) +
  geom_line(aes(expectedLifetimes, y = modelPrediction, group=cohortSize, color=as.factor(cohortSize)), size=1) +
  theme_minimal() +
  facet_grid(~model) +
  theme(
    legend.position="top",
    legend.background = element_rect(
      size=0.5,
      linetype="solid",
      colour ="black"
    )
  ) +
  scale_color_manual(name="Cohort Size", values=cc) +
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
    title = 'Effect of Study Length, Cohort Size on Predicted Power',
    subtitle = 'Quadratic- and Diminishing-Effects Models',
    caption = "Evaluated at Baseline Hazard = 0.05; Treatment HR = 0.5; % Open Enrollment = 50%; Cohort Size = 8"
  )

ggsave(
  filename = 'reports/figures/multiple-effects/power-expected-lifetimes-cohort-size.png',
  plot = gg,
  units = 'mm',
  width = 400,
  height = 300,
  scale = 0.4
)
