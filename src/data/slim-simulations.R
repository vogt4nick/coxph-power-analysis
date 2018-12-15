library(dplyr)
library(purrr)
library(ggplot2)
library(survival)

#' Generate simulation, but faster this time
#' 
#' Uses the negative binomial distribution instead of a for loop.
generate_simulation2 <- function(
  baseline_hazard_rate, treatment_hazard_ratio, 
  max_periods, max_cohorts, cohort_size,
  simulation_id=1, random_state=1
) {
  N <- cohort_size * max_cohorts
  is_treated <- seq(N) %% 2
  
  dat <- data_frame(
    simulationId = simulation_id,
    id = seq(N),
    cohortId = ((seq(N) - 1) %/% cohort_size) + 1,
    isTreated = is_treated,
    trueHr = baseline_hazard_rate * exp(log(treatment_hazard_ratio) * is_treated)
  ) %>% 
    mutate(
      nPeriods = rnbinom(N, 1, trueHr) + 1,
      isDead = ifelse(nPeriods <= max_periods, 1, 0),
      nPeriods = ifelse(nPeriods > max_periods, max_periods, nPeriods)
    )
  return(dat)
}

slim_simulations <- function(
  bhrs, thrs, mps, mcs, css, rss
) {
  all_params <- expand.grid(
    baseline_hazard_rate=bhrs, 
    treatment_hazard_ratio=thrs, 
    max_periods=mps, 
    max_cohorts=mcs, 
    cohort_size=css, 
    random_state=rss
  ) %>% 
    filter(max_cohorts <= max_periods) %>% 
    # filter(log(0.5) / log(1-baseline_hazard_rate) <= max_periods) %>% 
    filter(max_periods <= 3 * log(0.5) / log(1-baseline_hazard_rate)) %>% 
    as_data_frame()
  
  # Progress bar & stats
  message("Simulating ", nrow(all_params), " datasets...", "\n----|----|----|----|----|")
  pb <- utils::txtProgressBar(min=1, max=nrow(all_params), width=25)
  
  
  all_params$simulation_id <- seq(nrow(all_params))
  
  results <- list()
  for (i in seq(nrow(all_params))) {
    utils::setTxtProgressBar(pb, i)
    params = all_params[i, ]
    if (params$max_cohorts * params$cohort_size < 1e6) {
      # print(params)
      sims_df <- do.call(generate_simulation2, as.list(params))
      results[[i]] <- coxph(Surv(nPeriods, isDead) ~ isTreated, data=sims_df) %>% 
        summary() %>%
        coef() %>% 
        as_data_frame() %>% 
        mutate(
          simulationId = i
        )
      rm(sims_df)
    }
  }
  
  all_params <- all_params %>% 
    rename(
      baselineHazardRate = baseline_hazard_rate,
      treatmentHazardRatio = treatment_hazard_ratio,
      maxPeriods = max_periods,
      maxCohorts = max_cohorts,
      cohortSize = cohort_size,
      randomState = random_state,
      simulationId = simulation_id
    )
  
  # return results as data_frame
  data.table::rbindlist(results) %>% 
    as_data_frame() %>% 
    rename(
      logHr = `coef`,
      hr = `exp(coef)`,
      logSe = `se(coef)`,
      p = `Pr(>|z|)`
    ) %>% 
    inner_join(all_params, by='simulationId') %>% 
    select(simulationId, everything())
}
