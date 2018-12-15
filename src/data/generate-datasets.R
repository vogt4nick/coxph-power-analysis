
source('src/data/slim-simulations.R')

logit_inv <- function(x) {
  exp(x) / (exp(x) + 1)
}

x <- c(4, 8, 16, 32)

coxph_simulations <- slim_simulations(
  bhrs = logit_inv(seq(-5, 0)),
  thrs = logit_inv(seq(-5, 0)),
  mps  = x,
  mcs  = x,
  css  = x,
  rss  = seq(20)
)

save(coxph_simulations, file = 'data/interim/coxph_simulations.Rdata')

