# coxph-power-analysis
"When do I stop accepting new data for a continuous study?"

## How-To

The `coxph_results` object is relatively small (e.g. 4.2 MB for 115200 simulations). see `data/coxph_results.Rdata` if you don't care about making your own simulations.

`generate_simulations.R` contains the `generate_simulations` function. 

```generate_simulations example
> sims <- generate_simulations(
+   bhrs = c(0.01, 0.1),   # baseline hazard rates
+   thrs = 0.5,            # treatement hazard ratios
+   mps  = 12,             # max periods observed
+   mcs  = c(3, 6, 9, 12), # max cohorts accepted
+   css  = c(10, 20, 40),  # cohort sizes
+   rss  = seq(100)        # random seeds
+ )
Simulating 1200 datasets...
----|----|----|----|----|
=========================

> typeof(sims)
[1] "list"

> attributes(sims)
$names
[1] "params"  "sims_df"
```

`params` contains all sets of parameters tested and `sims_df` contains all simulation data in a single data frame.  

`fit-coxph-models.R` generates the simulations and fits cox models to all of them before outputting `coxph_results.Rdata` which contains the `coxph_results` `data_frame` object.

```
> attributes(coxph_results)
$class
[1] "tbl_df"     "tbl"        "data.frame"

$row.names
...

$names
 [1] "simulationId"         "baselineHazardRate"   "treatmentHazardRatio" "maxPeriods"          
 [5] "maxCohorts"           "cohortSize"           "randomState"          "logHr"               
 [9] "hr"                   "logSe"                "z"                    "p"                   
```

## Introduction 

Consider an MD with a new idea on how best to treat breast cancer. This MD
treated 5 cohorts of patients continuously for the last 5 years, and she wants
to learn if her treatment is effective. She decides to fit a Cox proportional 
hazards model to her data when she encounters a logical problem: 

When do we stop tracking new patients? When do we stop the study? 

This project seeks a general answer to these questions. To limit the breadth of the study, we compare studies of a single, binomial random variable with a fixed effect size. 

### Naive Strategies

The Cox model is shown to perform well with as much as a 25%/75% ratio of
censored/non-censored data. I can use this information.

1. I can use only the first cohort. I only had 10 patients in my first year. 
That's probably not enough to find a significant effect, even if one 
exists. 

1. I can use every cohort. I've treated 100 patients in total, but 50 of those
treated still survive. With so much censored data, my Cox model still may 
not find a significant effect, even if one exists. 

1. I just remove the shortest censors until I reach the 25% threshold, but 
this strategy biases the data.  

1. I can sort my patients by survival time and walk back my data until I hit 
the 25% threshold, but that method is also flawed. 

There must be a general rule for setting the cut off time. We want to
find that rule.

## Data

Here we introduce the simulated data and relationships to each other. We refer to them later by their abbreviated names. 

The __baseline hazard rate__ and __treatment hazard ratio__ are what you'd expect. Each simulation observes a __maximum number of periods__ before "concluding the study."

Each simulation observes a __maximum number of consecutive cohorts__. i.e. the first period of every simulation will introduce a new cohort. Periods accepting new cohorts are named __"open enrollment periods"__. Assume one cohort per year, and no gaps between cohorts. That is, for 5 periods and 3 cohorts, period 1, 2, and 3 will have new cohorts, and periods 4 and 5 will not. Finally, we will control for __cohort size__.
