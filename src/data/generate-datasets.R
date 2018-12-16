
source('src/data/slim-simulations.R')

logit_inv <- function(x) {
  exp(x) / (exp(x) + 1)
}

coxph_simulations <- slim_simulations(
  bhrs = logit_inv(seq(-8, -0)),
  thrs = logit_inv(seq(-4, 4)),
  mps  = seq(4, 64, 2),
  mcs  = seq(4, 64, 2),
  css  = seq(2, 32, 2),
  rss  = seq(1)
)

save(coxph_simulations, file = 'data/interim/coxph_simulations.Rdata')

