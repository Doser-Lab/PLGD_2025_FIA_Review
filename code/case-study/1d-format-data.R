# 1d-format-data.R: integrates multi-scale forest metrics, PRISM climate data, 
#                   and camera trap sequences to generate a formatted data list 
#                   (including a 3-day windowed detection matrix) for occupancy 
#                   modeling in `spOccupancy`.
# Author: Michelle Pretorius

rm(list = ls())
library(tidyverse)
library(sf)
library(lubridate)

# Function to collapse daily detections into windowed intervals -----------

# NOTE: this function is adapted from https://github.com/dochvam/autocorr_occ_camtraps_reproducible/blob/main/code_helper/helper_fn_clustered.R

# Logic: 1 if any detection, 0 if active but no detection, NA if inactive
format_windows <- function(y, interval) {
  n_sites <- nrow(y)
  n_windows <- floor(ncol(y) / interval)
  y_alt <- matrix(NA, nrow = n_sites, ncol = n_windows)
  
  for (j in 1:n_windows) {
    start_col <- (j - 1) * interval + 1
    end_col   <- j * interval
    window_data <- y[, start_col:end_col, drop = FALSE]
    
    any_ones <- rowSums(window_data == 1, na.rm = TRUE) > 0
    any_data <- rowSums(!is.na(window_data)) > 0
    y_alt[, j] <- ifelse(any_ones, 1, ifelse(any_data, 0, NA))
  }
  return(y_alt)
}

# Load and format occupancy covariates ------------------------------------

buffer_sizes <- c(200, 1000, 5000)

# Helper function to read and join forest/mast data across different scales
load_buffers <- function(prefix) {
  map(buffer_sizes, ~{
    read_csv(paste0("data/", prefix, "_TreeMap_", .x, "m.csv"), 
             show_col_types = FALSE) %>%
      select(Deployment_ID, everything())
  }) %>% 
    reduce(full_join, by = "Deployment_ID")
}

forest_raw <- load_buffers("Forest")
mast_raw   <- load_buffers("Mast")

# Clean and reclassify forest structure variables
# NOTE: forest structure was later scrapped from case study analyses 
occ_covs_forest <- forest_raw %>%
  left_join(mast_raw, by = "Deployment_ID") %>%
  mutate(across(contains(c("CANOPYPCT", "DRYBIO_D")), ~replace_na(.x, 0))) %>%
  mutate(across(contains("FLDSZCD"), ~case_when(is.na(.x) | 
                                                  .x %in% c(0, 1) ~ "Early",
                                                .x == 2 ~ "Mid",
                                                .x %in% 3:5 ~ "Late",
                                                TRUE ~ "Non-forest") %>% 
                  factor(levels = c("Non-forest", "Early", "Mid", "Late")))) %>%
  select(Deployment_ID,
         CANOPYPCT_mean_200, CANOPYPCT_mean_1000, CANOPYPCT_mean_5000,
         DRYBIO_D_mean_200, DRYBIO_D_mean_1000, DRYBIO_D_mean_5000,
         perc_hard_mast_200, perc_hard_mast_1000, perc_hard_mast_5000,
         perc_soft_mast_200, perc_soft_mast_1000, perc_soft_mast_5000)

# Load climate data (PRISM)
prism <- read.csv("data/SSUSA_ABB_2020_PRISM_Point_Locations.csv") %>%
  mutate(tmean_mean = rowMeans(select(., matches("tmean.*(08|09|10|11|12)$")), 
                               na.rm = TRUE)) %>%
  select(Deployment_ID, tmean_mean)

# Combine all site-level covariates (and create regional strata)
occ.covs <- occ_covs_forest %>%
  left_join(prism, by = "Deployment_ID") %>%
  left_join(read.csv("data/SSUSA_ABB_2020_Camera_Point_Locations_ESPG5070.csv") %>% 
              mutate(strata = ifelse(X < -150000, "W", "E")) %>%
              select(Deployment_ID, strata), by = "Deployment_ID") %>%
  arrange(Deployment_ID)

# Format detection matrix (y) ---------------------------------------------
# Note this will have to be downloaded from WildlifeInsights directly 
# (https://app.wildlifeinsights.org/initiatives/2000156/Snapshot-USA).

deployments <- read.csv("data/SSUSA_Full_2020_Deployments.csv") %>%
  filter(Year == 2020, 
         Deployment_ID %in% occ.covs$Deployment_ID) %>%
  arrange(Deployment_ID)

sequences <- read.csv("data/SSUSA_Full_2020_Sequences.csv") %>%
  filter(Year == 2020, 
         Common_Name == "American Black Bear", 
         Deployment_ID %in% deployments$Deployment_ID) %>%
  mutate(Det_Date = as.Date(Start_Time)) %>%
  distinct(Deployment_ID, Det_Date)

# Build daily detection history
y_long <- expand_grid(Deployment_ID = deployments$Deployment_ID, 
                      Day = 1:max(deployments$Survey_Nights)) %>%
  left_join(deployments %>% 
              select(Deployment_ID, Start_Date, Survey_Nights), 
            by = "Deployment_ID") %>%
  mutate(active = Day <= Survey_Nights,
         Current_Date = as.Date(Start_Date) + (Day - 1)) %>%
  left_join(sequences %>% 
              mutate(Detected = 1), 
            by = c("Deployment_ID", "Current_Date" = "Det_Date")) %>%
  mutate(y = ifelse(!active, NA, replace_na(Detected, 0)))

y_matrix <- y_long %>%
  select(Deployment_ID, Day, y) %>%
  pivot_wider(names_from = Day, values_from = y) %>%
  select(-Deployment_ID) %>%
  as.matrix()

# Collapse to 3-day windows
y <- format_windows(y_matrix, interval = 3)

# Format detection covariates (det.covs) ----------------------------------

# Julian Day Matrix
ordinal_mat <- matrix(NA, nrow = nrow(y), ncol = ncol(y))

for (j in 1:ncol(y)) {
  days_offset <- (j - 1) * 3
  v_ordinal <- yday(as.Date(deployments$Start_Date) + days(days_offset))
  v_ordinal[is.na(y[, j])] <- NA
  ordinal_mat[, j] <- v_ordinal
}

det.covs <- list(Day = ordinal_mat,
                 Feature = deployments$Feature_Type %>% 
                   fct_collapse(None = c("Water source, Road dirt, Trail hiking", 
                                         "Water source, hiking",
                                         "Road dirt,Trail hiking"),
                                Water = "Water source", 
                                Road = "Road dirt", 
                                Trail = "Trail hiking"),
                 Deployment_ID = as.numeric(as.factor(deployments$Deployment_ID)))

# Package data (data.list) and save -------------------------------------------

data <- list(y = y,
             occ.covs = occ.covs,
             det.covs = det.covs)

save(data, file = "data/SSUSA_ABB_2020_PGOcc_Data.List.rda")
