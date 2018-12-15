
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
  logit_dat %>% 
  mutate(
    baselineHazardRate2 = baselineHazardRate ** 2,
    treatmentHazardRatio2 = treatmentHazardRatio **2,
    cohortSize2 = cohortSize ** 2,
    expectedLifetimes2 = expectedLifetimes ** 2,
    pctOpenEnrollmentPeriods2 = pctOpenEnrollmentPeriods ** 2
  ) %>% 
  glm(
    formula = isCorrect ~ 
      baselineHazardRate + baselineHazardRate2 +
      treatmentHazardRatio + treatmentHazardRatio2 + 
      cohortSize + cohortSize2 +
      expectedLifetimes + expectedLifetimes2 +
      pctOpenEnrollmentPeriods + pctOpenEnrollmentPeriods2, 
    data = ., 
    family = binomial(link = "logit"))


# Cubic effects -----------------------------------------------------------

cubic_effects <- 
  logit_dat %>% 
  mutate(
    baselineHazardRate2 = baselineHazardRate ** 2,
    treatmentHazardRatio2 = treatmentHazardRatio **2,
    cohortSize2 = cohortSize ** 2,
    expectedLifetimes2 = expectedLifetimes ** 2,
    pctOpenEnrollmentPeriods2 = pctOpenEnrollmentPeriods ** 2,
    baselineHazardRate3 = baselineHazardRate ** 3,
    treatmentHazardRatio3 = treatmentHazardRatio ** 3,
    cohortSize3 = cohortSize ** 3,
    expectedLifetimes3 = expectedLifetimes ** 3,
    pctOpenEnrollmentPeriods3 = pctOpenEnrollmentPeriods ** 3
  ) %>% 
  glm(
    formula = isCorrect ~ 
      baselineHazardRate + baselineHazardRate2 + baselineHazardRate3 +
      treatmentHazardRatio + treatmentHazardRatio2 + treatmentHazardRatio3 +
      cohortSize + cohortSize2 + cohortSize3 +
      expectedLifetimes + expectedLifetimes2 + expectedLifetimes3 +
      pctOpenEnrollmentPeriods + pctOpenEnrollmentPeriods2 + pctOpenEnrollmentPeriods3, 
    data = ., 
    family = binomial(link = "logit"))


# log effects -------------------------------------------------------------

log_effects <- 
  logit_dat %>% 
  mutate(
    logBaselineHazardRate = log(baselineHazardRate),
    logTreatmentHazardRatio = log(treatmentHazardRatio),
    logCohortSize = log(cohortSize),
    logExpectedLifetimes = log(expectedLifetimes),
    logPctOpenEnrollmentPeriods = log(pctOpenEnrollmentPeriods)
  ) %>% 
  glm(
    formula = isCorrect ~ 
      logBaselineHazardRate +
      logTreatmentHazardRatio + 
      logCohortSize +
      logExpectedLifetimes + 
      logPctOpenEnrollmentPeriods, 
    data = ., 
    family = binomial(link = "logit"))

save(linear_effects, quadratic_effects, cubic_effects, log_effects, file = 'models/explore-effects.Rdata')
