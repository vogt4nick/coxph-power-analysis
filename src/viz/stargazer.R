
library(stargazer)

load('models/explore-effects.Rdata')

stargazer(linear_effects, quadratic_effects)

stargazer(log_effects)
