# 1a-prep-data.R: cleans the Snapshot USA 2020 camera trap data, saves camera 
#                 locations as csv for use in subsequent scripts and creates 
#                 the study map presented in manuscript
# Author: Michelle Pretorius

rm(list = ls())
library(tidyverse)
library(sf)
library(rangeBuilder)
library(rnaturalearth)
library(ggspatial)
library(extrafont)

# Camera deployment data
# Note this will have to be downloaded from WildlifeInsights directly 
# (https://app.wildlifeinsights.org/initiatives/2000156/Snapshot-USA).
snapALL_deploy <- read.csv("data/SSUSA_Full_2020_Deployments.csv")

# Deployments with duplicate GPS to be removed (hard coded)
duplicate_locs <- c("MT_Forest_River_Trail_20_01B",
                    "NC_Forest_Alligator_River_NWR_20_07",
                    "NC_Forest_Alligator_River_NWR_20_08",
                    "NC_Forest_Alligator_River_NWR_20_09",
                    "NY_Forest_St_Lawrence_University_20_07")

# Data cleaning -----------------------------------------------------------

deployments <- snapALL_deploy %>% 
  filter(Year == 2020) %>% # Filter 2020
  select(-Project, -Site_Name) %>%
  distinct(Deployment_ID, .keep_all = TRUE) %>%
  arrange(Deployment_ID) %>%
  filter(!(Deployment_ID %in% duplicate_locs))

# Convert to simple features (WGS84)
cams_sf <- deployments %>%
  select(Deployment_ID, Longitude, Latitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

# Load IUCN American Black Bear (ABB) range
IUCN <- st_read("data/IUCN_range_map_ABB/data_0.shp", crs = 4326) %>% 
  filter(PRESENCE == 1)

# Keep only cameras within the IUCN range
sf_use_s2(FALSE) # Disable S2 for planar filtering
cams_ABB <- st_filter(cams_sf, IUCN)

# Proximity thinning (200m) -----------------------------------------------

# filterByProximity() returns the indices of points to REMOVE
cams_coords <- st_coordinates(cams_ABB)
rm_coords <- filterByProximity(xy = cams_coords,
                               dist = 0.2,
                               returnIndex = TRUE)

# Filter final dataset and remove non-CONUS regions
final_deployments <- deployments %>%
  filter(Deployment_ID %in% cams_ABB$Deployment_ID,
         !Deployment_ID %in% cams_ABB$Deployment_ID[rm_coords],
         !(grepl('AK_Forest_Chilkat', Deployment_ID)), # filter out Alaska
         !(grepl('HI_Forest', Deployment_ID))) # filter out Hawaii

# Export coordinates for GEE/PRISM
camera_points_4326 <- final_deployments %>%
  select(Deployment_ID, Latitude, Longitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  bind_cols(st_coordinates(.)) %>%
  st_drop_geometry()

camera_points_5070 <- final_deployments %>%
  select(Deployment_ID, Latitude, Longitude) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(5070) %>%
  bind_cols(st_coordinates(.)) %>%
  st_drop_geometry()

write.csv(camera_points_4326, "data/SSUSA_ABB_2020_Camera_Point_Locations_ESPG4326.csv", 
          row.names = FALSE)

write.csv(camera_points_5070, "data/SSUSA_ABB_2020_Camera_Point_Locations_ESPG5070.csv", 
          row.names = FALSE)

# Figure 1: Case study map ------------------------------------------------

# NOTE: we are loading everything in again so this could be run separately to 
#       the code above.

world <- ne_countries(scale = "medium", returnclass = "sf")
cam_locs <- read.csv("data/SSUSA_ABB_2020_Camera_Point_Locations_ESPG5070.csv") %>%
  st_as_sf(coords = c("X", "Y"), crs = 5070)
iucn_extant <- st_read("data/IUCN_range_map_ABB/data_0.shp", crs = 4326) %>%
  filter(PRESENCE ==1)

# Fetch US states and filter to CONUS
us_states <- states(cb = TRUE,
                    resolution = "20m") %>%
  filter(!(NAME %in% c("Alaska", "District of Columbia", "Hawaii", "Puerto Rico"))) %>% 
  shift_geometry()

# Align projections
target_crs <- st_crs(us_states)
world_transformed <- st_transform(world, crs = target_crs)
iucn_transformed <- st_transform(iucn_extant, crs = target_crs)
cams_transformed <- st_transform(cam_locs, crs = target_crs)

# Map colours
col_range  <- "#35b779"  # viridis green (distinct in grayscale)
col_cams   <- "black"  
col_states <- "white"
col_world  <- "#f0f0f0"

ggplot() +
  geom_sf(data = world_transformed, 
          fill = col_world, 
          color = "grey90", 
          linewidth = 0.2) +
  geom_sf(data = us_states, 
          fill = col_states, 
          color = "grey80", 
          linewidth = 0.3) +
  geom_sf(data = subset(iucn_transformed, LEGEND == "Extant (resident)"), 
          fill = col_range, 
          alpha = 0.4, 
          color = NA) +
  geom_sf(data = cams_transformed, 
          color = col_cams, 
          size = 2, 
          alpha = 0.7) +
  geom_vline(xintercept = -150000, linetype = "dashed") +
  annotation_scale(location = "bl", 
                   width_hint = 0.2, 
                   text_size = 8, 
                   text_family = "LM Roman 10") + 
  annotation_north_arrow(location = "bl", 
                         which_north = "true", 
                         pad_x = unit(0.2, "in"), pad_y = unit(0.3, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(datum = NA,
           xlim = st_bbox(us_states)[c(1,3)], 
           ylim = st_bbox(us_states)[c(2,4)]) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        legend.position = "right",
        text = element_text(family = "LM Roman 10", size = 12))

# NOTE: for publication we edited the font of the "N" in the north arrow manually