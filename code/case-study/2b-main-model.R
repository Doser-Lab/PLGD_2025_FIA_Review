# 2b-main-model.R: runs the full occupancy model, performs preliminary 
#                 diagnostics and saves results. 
# Author: Michelle Pretorius

rm(list = ls())
library(spOccupancy)
library(dplyr)

set.seed(124) # different seed to initial run

# Load spOccupancy formatted data -----------------------------------------

load("data/SSUSA_ABB_2020_PGOcc_Data.List.rda")

# Run full occupancy model (PGOcc) ----------------------------------------------

# Global model settings
n.omp.threads <- 1
verbose <- TRUE
n.report <- 1000 

n.samples <- 100000
n.burn <- 40000
n.thin <- 10
n.chains <- 3

# Priors (same as initial run)
priors.list <- list(
  alpha.normal = list(mean = 0, var = 2.72),
  beta.normal = list(mean = 0, var = 2.72),
  sigma.sq.p.ig = list(a = 0.1, b = 0.1))

# Detection formula (same as initial run)
det.formula <-  ~ factor(Feature) + 
  scale(Day) + 
  I(scale(Day)^2) +
  (1 | Deployment_ID)

# Occupancy formula for each scale (same as initial run)
occ.formula.200 <- ~ 
  scale(tmean_mean_period) + 
  I(scale(tmean_mean_period)^2) + 
  factor(strata) +
  scale(perc_hard_mast_200) + 
  scale(perc_soft_mast_200) +
  scale(DRYBIO_D_mean_200) +
  scale(CANOPYPCT_mean_200) + 
  I(scale(CANOPYPCT_mean_200)^2)

occ.formula.1000 <- ~ 
  scale(tmean_mean_period) + 
  I(scale(tmean_mean_period)^2) + 
  factor(strata) +
  scale(perc_hard_mast_1000) + 
  scale(perc_soft_mast_1000) + 
  scale(DRYBIO_D_mean_1000) +
  scale(CANOPYPCT_mean_1000) + 
  I(scale(CANOPYPCT_mean_1000)^2)

occ.formula.5000 <- ~ 
  scale(tmean_mean_period) + 
  I(scale(tmean_mean_period)^2) +
  factor(strata) +
  scale(perc_hard_mast_5000) + 
  scale(perc_soft_mast_5000) + 
  scale(DRYBIO_D_mean_5000) +
  scale(CANOPYPCT_mean_5000) + 
  I(scale(CANOPYPCT_mean_5000)^2)

# Load in separate initial values for each scale (from `2a-initial-model.R`)
load('results/model-inits_PGOcc_SSUSA_ABB_2020_200.rda')
load('results/model-inits_PGOcc_SSUSA_ABB_2020_1000.rda')
load('results/model-inits_PGOcc_SSUSA_ABB_2020_5000.rda')

# Run
out.200 <- PGOcc(
  occ.formula = occ.formula.200,
  det.formula = det.formula,
  data = data,
  priors = priors.list,
  inits = inits.list.200, 
  n.samples = n.samples,
  n.report = n.report, 
  n.burn = n.burn, 
  n.thin = n.thin, 
  n.chains = n.chains)

out.1000 <- PGOcc(
  occ.formula = occ.formula.1000,
  det.formula = det.formula,
  data = data,
  priors = priors.list,
  inits = inits.list.1000, 
  n.samples = n.samples,
  n.report = n.report, 
  n.burn = n.burn, 
  n.thin = n.thin, 
  n.chains = n.chains)

out.5000 <- PGOcc(
  occ.formula = occ.formula.5000,
  det.formula = det.formula,
  data = data,
  priors = priors.list,
  inits = inits.list.5000, 
  n.samples = n.samples,
  n.report = n.report, 
  n.burn = n.burn, 
  n.thin = n.thin, 
  n.chains = n.chains)

# Save models -------------------------------------------------------------

save(out.200, file = paste0('results/Full_PGOcc_SSUSA_ABB_2020_200_', 
                            n.samples, '-samples-', Sys.Date(), '.rda'))
save(out.1000, file = paste0('results/Full_PGOcc_SSUSA_ABB_2020_100-_', 
                             n.samples, '-samples-', Sys.Date(), '.rda'))
save(out.5000, file = paste0('results/Full_PGOcc_SSUSA_ABB_2020_5000_',
                             n.samples, '-samples-', Sys.Date(), '.rda'))

# WAIC model comparisons --------------------------------------------------

waicOcc(out.200)
waicOcc(out.1000)
waicOcc(out.5000)

# Model summaries ---------------------------------------------------------

summary(out.200)
summary(out.1000)
summary(out.5000)
