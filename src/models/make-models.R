
library(tidyverse)

load('data/processed/logit_dat.Rdata')


# linear_effects ----------------------------------------------------------

linear_effects <- 
  glm(
    formula = isCorrect ~ 
      baselineHazardRate + treatmentHazardRatio + cohortSize +
      expectedLifetimes + pctOpenEnrollmentPeriods, 
    data = logit_dat, 
    family = binomial(link = "logit"))



# quadratic effects -------------------------------------------------------

quadratic_effects <- 
  glm(
    formula = isCorrect ~ 
      baselineHazardRate + baselineHazardRate2 +
      treatmentHazardRatio + treatmentHazardRatio2 + 
      cohortSize + cohortSize2 +
      expectedLifetimes + expectedLifetimes2 +
      pctOpenEnrollmentPeriods + pctOpenEnrollmentPeriods2, 
    data = logit_dat, 
    family = binomial(link = "logit"))


# Cubic effects -----------------------------------------------------------

cubic_effects <- 
  glm(
    formula = isCorrect ~ 
      baselineHazardRate + baselineHazardRate2 + baselineHazardRate3 +
      treatmentHazardRatio + treatmentHazardRatio2 + treatmentHazardRatio3 +
      cohortSize + cohortSize2 + cohortSize3 +
      expectedLifetimes + expectedLifetimes2 + expectedLifetimes3 +
      pctOpenEnrollmentPeriods + pctOpenEnrollmentPeriods2 + pctOpenEnrollmentPeriods3, 
    data = logit_dat, 
    family = binomial(link = "logit"))


# log effects -------------------------------------------------------------

diminishing_effects <- 
  glm(
    formula = isCorrect ~ 
      exp(baselineHazardRate) +
      exp(treatmentHazardRatio) + 
      logCohortSize +
      logExpectedLifetimes + 
      logPctOpenEnrollmentPeriods, 
    data = logit_dat, 
    family = binomial(link = "logit"))


save(linear_effects, quadratic_effects, cubic_effects, diminishing_effects, file = 'models/explore-effects.Rdata')


# mixed effects -----------------------------------------------------------

mixed_effects <- glm(
  formula = isCorrect ~ 
    logBaselineHazardRate +
    logTreatmentHazardRatio + 
    cohortSize +
    logExpectedLifetimes + 
    pctOpenEnrollmentPeriods + pctOpenEnrollmentPeriods2, 
  data = logit_dat, 
  family = binomial(link = "logit"))

AIC(linear_effects)
AIC(quadratic_effects)
AIC(cubic_effects)
AIC(diminishing_effects)
AIC(mixed_effects)
