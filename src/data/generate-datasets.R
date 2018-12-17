source('src/data/slim-simulations.R')

logit_inv <- function(x) {
  exp(x) / (exp(x) + 1)
}

coxph_simulations <- slim_simulations(
  bhrs = logit_inv(seq(-6, -0)),
  thrs = logit_inv(seq(-3, 2)),
  mps  = 2 ** seq(2, 6),
  mcs  = 2 ** seq(2, 6),
  css  = 2 ** seq(2, 5),
  rss  = seq(20)
)

save(coxph_simulations, file = 'data/interim/coxph_simulations.Rdata')
