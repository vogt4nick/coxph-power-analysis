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