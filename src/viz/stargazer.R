
library(stargazer)

load('models/explore-effects.Rdata')

stargazer(linear_effects, quadratic_effects, cubic_effects)

stargazer(diminishing_effects)
