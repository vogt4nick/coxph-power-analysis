
library(stargazer)

load('models/explore-effects.Rdata')

#' https://stackoverflow.com/a/30198126/5905195
mod_stargazer <- function(output.file, ...) {
  output <- capture.output(stargazer(...))
  cat(paste(output, collapse = "\n"), "\n", file=output.file, append=TRUE)
}

'reports/figures/model-results/lin-quad-cubic-effects.tex'

mod_stargazer(
  'reports/figures/model-results/lin-quad-cubic-effects.tex',
  linear_effects, quadratic_effects, cubic_effects
  )

mod_stargazer(
  'reports/figures/model-results/diminishing-effects.tex',
  diminishing_effects
  )
