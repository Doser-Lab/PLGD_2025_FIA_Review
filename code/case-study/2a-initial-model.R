# `2a-initial-model.R`: runs a smaller occupancy model and extracts initial 
#                       values for use in `2b-main-model.R`
# Author: Michelle Pretorius

rm(list = ls())
library(spOccupancy)
library(dplyr)

set.seed(123)

# Load spOccupancy formatted data -----------------------------------------

load("data/SSUSA_ABB_2020_PGOcc_Data.List.rda")

# Run occupancy model (PGOcc) ----------------------------------------------

# Global model settings
n.omp.threads <- 1
verbose <- TRUE
n.report <- 1000 

n.samples <- 5000       
n.burn <- 2000       
n.thin <- 1       
n.chains <- 3

# Global detection formula
det.formula <-  ~ factor(Feature) + 
  scale(Day) + 
  I(scale(Day)^2) +
  (1 | Deployment_ID) #random effect

# Occupancy formulae for each scale  
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

# Initial values
inits.list <- list(alpha = 0, 
                   beta = 0,
                   z = apply(data$y, 1, max, na.rm = TRUE),
                   sigma.sq.p = 1) #random effect

# Priors
priors.list <- list(alpha.normal = list(mean = 0, var = 2.72),
                    beta.normal = list(mean = 0, var = 2.72),
                    sigma.sq.p.ig = list(a = 0.1, b = 0.1)) #random effect

# Run
out.200 <- PGOcc(
  occ.formula = occ.formula.200,
  det.formula = det.formula,
  data = data,
  priors = priors.list,
  inits = inits.list, 
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
  inits = inits.list, 
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
  inits = inits.list, 
  n.samples = n.samples,
  n.report = n.report, 
  n.burn = n.burn, 
  n.thin = n.thin, 
  n.chains = n.chains)

# Save the results --------------------------------------------------------

save(out.200, file = paste0('results/Initial_PGOcc_SSUSA_ABB_2020_200_', 
                            n.samples, '-samples.rda'))
save(out.1000, file = paste0('results/Initial_PGOcc_SSUSA_ABB_2020_1000_', 
                             n.samples, '-samples.rda'))
save(out.5000, file = paste0('results/Initial_PGOcc_SSUSA_ABB_2020_5000_',
                             n.samples, '-samples.rda'))


# Extract initial values for each model to use in `2b-main-model` -----------

alpha.inits.200 <- apply(all_models$Scale_200_occ.formula$alpha.samples, 2, median)
beta.inits.200 <- apply(all_models$Scale_200_occ.formula$beta.samples, 2, median)
z.inits.200 <- apply(all_models$Scale_200_occ.formula$z.samples, 2, median)
sigma.sq.p.inits.200 <- apply(all_models$Scale_200_occ.formula$sigma.sq.p.samples, 2, median)

inits.list.200 <- list(alpha = alpha.inits.200, 
                       beta = beta.inits.200, 
                       z = z.inits.200,
                       sigma.sq.p = sigma.sq.p.inits.200)

save(inits.list.200, file = 'results/model-inits_PGOcc_SSUSA_ABB_2020_200.rda')

alpha.inits.1000 <- apply(all_models$Scale_1000_occ.formula$alpha.samples, 2, median)
beta.inits.1000 <- apply(all_models$Scale_1000_occ.formula$beta.samples, 2, median)
z.inits.1000 <- apply(all_models$Scale_1000_occ.formula$z.samples, 2, median)
sigma.sq.p.inits.1000 <- apply(all_models$Scale_1000_occ.formula$sigma.sq.p.samples, 2, median)

inits.list.1000 <- list(alpha = alpha.inits.1000, 
                       beta = beta.inits.1000, 
                       z = z.inits.1000,
                       sigma.sq.p = sigma.sq.p.inits.1000)

save(inits.list.1000, file = 'results/model-inits_PGOcc_SSUSA_ABB_2020_1000.rda')

alpha.inits.5000 <- apply(all_models$Scale_5000_occ.formula$alpha.samples, 2, median)
beta.inits.5000 <- apply(all_models$Scale_5000_occ.formula$beta.samples, 2, median)
z.inits.5000 <- apply(all_models$Scale_5000_occ.formula$z.samples, 2, median)

inits.list.5000 <- list(alpha = alpha.inits.5000, 
                        beta = beta.inits.5000, 
                        z = z.inits.5000)

save(inits.list.5000, file = 'results/model-inits_PGOcc_SSUSA_ABB_2020_5000.rda')
