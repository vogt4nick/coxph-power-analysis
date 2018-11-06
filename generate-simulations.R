
library(dplyr)

#' Generate simulation
generate_simulation <- function(
  baseline_hazard_rate, treatment_hazard_ratio, 
  max_periods, max_cohorts, cohort_size,
  simulation_id, random_state=1
) {
  stopifnot(cohort_size %% 2 == 0)
  stopifnot(max_cohorts <= max_periods)
  set.seed(random_state)
  
  N <- cohort_size * max_cohorts
  is_treated <- seq(N) %% 2
  
  dat <- data.frame(
    simulationId = simulation_id,
    id = seq(N),
    cohortId = ((seq(N) - 1) %/% cohort_size) + 1,
    isTreated = is_treated,
    isDead = rep(0, N),
    nPeriods = rep(0, N),
    trueHr = baseline_hazard_rate * exp(log(treatment_hazard_ratio) * is_treated)
  )
  
  for (i in seq(max_periods)) {
    is_alive <- dat$isDead == 0
    in_study <- dat$cohortId <= i
    dat[is_alive & in_study, 'nPeriods'] = dat[is_alive & in_study, 'nPeriods'] + 1
    sentenced_to_death = runif(N) > (1 - dat$trueHr) ** dat$nPeriods
    dat[(is_alive & in_study & sentenced_to_death), 'isDead'] <- 1
  }
  
  return(dat)
}


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

#' Generate simulations using generate_simulation2
#' 
#' Enter vectors of values to be simulated. Function automatically
#' drops extreme or impossible combinations:
#' 
#' - max_cohorts > max_periods
#' - E[H(t)] > max_periods
#' - 2 * E[H(t)] < max_periods
generate_simulations <- function(
  bhrs, thrs, mps, mcs, css, rss
) {
  all_params <- expand.grid(
      baseline_hazard_rate=bhrs, 
      treatment_hazard_ratio=thrs, 
      max_periods=mps, max_cohorts=mcs, cohort_size=css, 
      random_state=rss
    ) %>% 
    filter(max_cohorts <= max_periods) %>% 
    filter(log(0.5) / log(1-baseline_hazard_rate) <= max_periods) %>% 
    filter(max_periods <= 3 * log(0.5) / log(1-baseline_hazard_rate)) %>% 
    as_data_frame()
  
  # Progress bar & stats
  message("Simulating ", nrow(all_params), " datasets...", "\n----|----|----|----|----|")
  pb <- utils::txtProgressBar(min=1, max=nrow(all_params), width=25)
  

  all_params$simulation_id <- seq(nrow(all_params))
  
  sims <- list()
  for (i in seq(nrow(all_params))) {
    utils::setTxtProgressBar(pb, i)
    params = all_params[i, ]
    sims[[i]] <- do.call(generate_simulation2, as.list(params))
  }

  sims_df <- data.table::rbindlist(sims) %>% 
    as_data_frame()
  list(params = all_params, sims_df=sims_df)
}


# save(sims, file = './data/sims.Rdata')
