rm(list = ls())
gc()

# Load in libraries:
library(spOccupancy)
library(dplyr)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(extrafont)

loadfonts(device = "win")

# Load in spOcc data and models -----------------------------------------------
load("data/SSUSA_ABB_2020_PGOcc_Data.List.rda")

load("results/NonSpatial_strata_quad_200_PGOcc_SSUSA_ABB_2020-out1e+05-samples-2026-03-17.rda")
load("results/NonSpatial_strata_quad_1000_PGOcc_SSUSA_ABB_2020-out1e+05-samples-2026-03-17.rda")
load("results/NonSpatial_strata_quad_5000_PGOcc_SSUSA_ABB_2020-out1e+05-samples-2026-03-17.rda")

# Plots for all variables and scales --------------------------------------
# Note we did not use this in the manuscript, we only presented results for "best" model: 5km

# 200m
beta_means_200 <- out.200$beta.samples[, -1]  
beta_means_200 <- as.data.frame(beta_means_200) %>%
  rename_with(~ .x %>%
                str_remove_all("scale\\(|factor\\(|I\\(|\\)") %>%
                str_remove_all("_200"))

beta_means_200_quantiles <- as.data.frame(matrix(NA, ncol(beta_means_200),6)) 
colnames(beta_means_200_quantiles) <- c("parameter", "mean", "q2.5", "q25", "q75", "q97.5")
beta_means_200_quantiles$parameter <- colnames(beta_means_200)

beta_means_200_quantiles$mean <- colMeans(beta_means_200)
beta_means_200_quantiles[,3:6] <- t(apply(beta_means_200, 2, quantile, 
                                           probs=c(0.025, 0.25, 0.75, 0.975)))

beta_means_200_quantiles <- beta_means_200_quantiles %>%
  mutate(model = "200m")

# 1km
beta_means_1000 <- out.1000$beta.samples[, -1] 
beta_means_1000 <- as.data.frame(beta_means_1000) %>%
  rename_with(~ .x %>%
                str_remove_all("scale\\(|factor\\(|I\\(|\\)") %>%
                str_remove_all("_1000"))

beta_means_1000_quantiles <- as.data.frame(matrix(NA, ncol(beta_means_1000),6)) 
colnames(beta_means_1000_quantiles) <- c("parameter", "mean", "q2.5", "q25", "q75", "q97.5")
beta_means_1000_quantiles$parameter <- colnames(beta_means_1000)

beta_means_1000_quantiles$mean <- colMeans(beta_means_1000)
beta_means_1000_quantiles[,3:6] <- t(apply(beta_means_1000, 2, quantile, 
                                           probs=c(0.025, 0.25, 0.75, 0.975)))

beta_means_1000_quantiles <- beta_means_1000_quantiles %>%
  mutate(model = "1km")

# 5km
beta_means_5000 <- out.5000$beta.samples[, -1] 
beta_means_5000 <- as.data.frame(beta_means_5000) %>%
  rename_with(~ .x %>%
                str_remove_all("scale\\(|factor\\(|I\\(|\\)") %>%
                str_remove_all("_5000"))

beta_means_5000_quantiles <- as.data.frame(matrix(NA, ncol(beta_means_5000),6)) 
colnames(beta_means_5000_quantiles) <- c("parameter", "mean", "q2.5", "q25", "q75", "q97.5")
beta_means_5000_quantiles$parameter <- colnames(beta_means_5000)

beta_means_5000_quantiles$mean <- colMeans(beta_means_5000)
beta_means_5000_quantiles[,3:6] <- t(apply(beta_means_5000, 2, quantile, 
                                           probs=c(0.025, 0.25, 0.75, 0.975)))

beta_means_5000_quantiles <- beta_means_5000_quantiles %>%
  mutate(model = "5km")

# Combine
plot_df <- bind_rows(beta_means_200_quantiles, beta_means_1000_quantiles, beta_means_5000_quantiles) %>%
  mutate(model = factor(model, levels=c("200m","1km","5km"), 
                        labels=c("200m","1km","5km"))) %>%
  mutate(coef.zero = q2.5/q97.5 < 0,
         coef.col = if_else(coef.zero == TRUE, true = "grey", false = "black")) %>%
  mutate(parameter = case_match(parameter,
                                "CANOPYPCT_mean"      ~ "Canopy cover",
                                "CANOPYPCT_mean^2"    ~ "Canopy cover\u00B2",
                                "DRYBIO_D_mean"       ~ "Deadwood",
                                "perc_hard_mast"      ~ "Hard mast",
                                "perc_soft_mast"      ~ "Soft mast",
                                "ppt_mean_period"     ~ "Precipitation",
                                "tmean_mean_period"   ~ "Temperature",
                                "tmean_mean_period^2" ~ "Temperature\u00B2",
                                "strataW"             ~ "Region: West",
                                .default = parameter))

# Occupancy
occ.covs <- ggplot(plot_df, aes(x = reorder(parameter, mean), y = mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_errorbar(aes(ymin = q2.5, ymax = q97.5, color = coef.col), width = 0.2, linewidth = 0.7) +
  geom_point(size = 2, aes(color = coef.col)) +
  coord_flip() +  # Horizontal layout is easier to read
  facet_wrap(~ model) +
  labs(x = "", y = "Occurrence") +
  theme_classic() +
  scale_fill_identity() +
  scale_color_identity() +
  theme(panel.spacing = unit(0.7, "cm"),
        text = element_text(family = "LM Roman 10", size = 20))

# Detection

alpha_means <- out.200$alpha.samples[, -1]  
alpha_means <- as.data.frame(alpha_means) %>%
  rename_with(~ .x %>%
                str_remove_all("scale\\(|factor\\(|I\\(|\\)"))

plot_df_det <- alpha_means %>%
  pivot_longer(cols = everything(), 
               names_to = "parameter", 
               values_to = "value") %>%
  group_by(parameter) %>%
  summarise(
    mean = mean(value),
    lower = quantile(value, 0.025),
    upper = quantile(value, 0.975),
    .groups = "drop") %>%
  mutate(coef.zero = lower/upper < 0,
         coef.col = if_else(coef.zero == TRUE, true = "grey", false = "black")) %>%
  mutate(parameter = case_match(parameter,
                                "Day^2"      ~ "Day\u00B2",
                                "Feature_TypeRoad"    ~ "Feature: Road",
                                "Feature_TypeTrail"       ~ "Feature: Trail",
                                "Feature_TypeWater"      ~ "Feature: Water",
                                .default = parameter))

det.covs <- ggplot(plot_df_det, aes(x = reorder(parameter, mean), y = mean)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = coef.col), 
                width = 0.2, size = 0.8) +
  geom_point(size = 3, aes(color = coef.col)) +
  coord_flip() +  # Horizontal layout is easier to read
  labs(x = "", y = "Detection") +
  theme_classic() +
  scale_fill_identity() +
  scale_color_identity() +
  theme(text = element_text(family = "LM Roman 10", size = 20))

# Predictions -----------------------------------------------------------------

n.points <- 100

# Create scaled variables
deadwood_scaled <- seq(min(scale(data$occ.covs$DRYBIO_D_mean_5000)), 
                       max(scale(data$occ.covs$DRYBIO_D_mean_5000)), length.out = n.points)

canopy_scaled <- seq(min(scale(data$occ.covs$CANOPYPCT_mean_5000)), 
                     max(scale(data$occ.covs$CANOPYPCT_mean_5000)), length.out = n.points)

# NB: Columns must match summary exactly:
# 1:Intercept, 2:tmean, 3:tmean^2, 4:strata, 5:hard_mast, 6:soft_mast, 7:Deadwood, 8:Canopy, 9:Canopy^2
X.deadwood <- matrix(0, nrow = n.points, ncol = 9)
X.deadwood[, 1] <- 1                
X.deadwood[, 7] <- deadwood_scaled

X.canopy <- matrix(0, nrow = n.points, ncol = 9)
X.canopy[, 1] <- 1                  
X.canopy[, 8] <- canopy_scaled      
X.canopy[, 9] <- canopy_scaled^2    

# Predict for deadwood
pred.deadwood <- predict(out.5000, X.deadwood, type = 'occupancy')

# Create plot data for deadwood
df.deadwood <- data.frame(Value = deadwood_scaled,
                          Mean = apply(pred.deadwood$psi.0.samples, 2, mean),
                          Low = apply(pred.deadwood$psi.0.samples, 2, quantile, 0.025),
                          High = apply(pred.deadwood$psi.0.samples, 2, quantile, 0.975))

# Predict for canopy
pred.canopy <- predict(out.5000, X.canopy, type = 'occupancy')

# Create plot data for canopy
df.canopy <- data.frame(Value = canopy_scaled,
                        Mean = apply(pred.canopy$psi.0.samples, 2, mean),
                        Low = apply(pred.canopy$psi.0.samples, 2, quantile, 0.025),
                        High = apply(pred.canopy$psi.0.samples, 2, quantile, 0.975))

dw_mean <- mean(data$occ.covs$DRYBIO_D_mean_5000, na.rm = TRUE)
dw_sd   <- sd(data$occ.covs$DRYBIO_D_mean_5000, na.rm = TRUE)

can_mean <- mean(data$occ.covs$CANOPYPCT_mean_5000, na.rm = TRUE)
can_sd   <- sd(data$occ.covs$CANOPYPCT_mean_5000, na.rm = TRUE)

# Back-transform: (scaled_val * SD) + mean
df.deadwood$RealValue <- (df.deadwood$Value * dw_sd) + dw_mean
df.canopy$RealValue   <- (df.canopy$Value * can_sd) + can_mean

# Get data from rug plot
rug_df <- data.frame(deadwood = data$occ.covs$DRYBIO_D_mean_5000,
                     canopy = data$occ.covs$CANOPYPCT_mean_5000)

# Plot 

# Create single simple theme 
theme <- theme_minimal() + 
  theme(text = element_text(family = "LM Roman 10"), # Global font set
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.ticks = element_line(colour = "black"),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18, color = "black"))

p_deadwood <- ggplot(df.deadwood, aes(x = RealValue, y = Mean)) +
  geom_rug(data = rug_df, aes(x = deadwood), 
           inherit.aes = FALSE, 
           sides = "b", 
           alpha = 0.3, 
           color = "black") +
  geom_ribbon(aes(ymin = Low, ymax = High), 
              fill = "#4a4a4a", 
              alpha = 0.15) +
  geom_line(color = "#4a4a4a", 
            size = 1.1) +
  labs(x = expression(paste("Deadwood biomass (tons/acre)")), # Example units
       y = "Probability of Occurrence") +
  scale_y_continuous(limits = c(0, 1), 
                     expand = c(0.01, 0), 
                     breaks = seq(0, 1, 0.2)) +
  theme

p_canopy <- ggplot(df.canopy, aes(x = RealValue, y = Mean)) +
  geom_rug(data = rug_df, aes(x = canopy),
           inherit.aes = FALSE, 
           sides = "b", 
           alpha = 0.3, 
           color = "black") +
  geom_ribbon(aes(ymin = Low, ymax = High), 
              fill = "#35b779", 
              alpha = 0.15) +
  geom_line(color = "#35b779", 
            size = 1.1) +
  labs(x = "Canopy cover (%)",
       y = NULL) + 
  scale_y_continuous(limits = c(0, 1), 
                     expand = c(0.01, 0), 
                     breaks = seq(0, 1, 0.2)) +
  theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

final_plot <- plot_grid(p_deadwood, p_canopy, 
                        labels = c("A", "B"), 
                        label_size = 20,
                        label_fontface = "bold",
                        label_fontfamily = "LM Roman 10", # Ensure labels match
                        align = 'h', 
                        vjust = -0.5, 
                        hjust = -0.5,
                        rel_widths = c(1, 0.9) # Slightly narrower for B since it has no y-axis labels
                        ) +
  theme(plot.margin = margin(t = 20, r = 10, b = 10, l = 10))
