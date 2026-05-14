# 1b-get-other-covs.R: extracts data from PRISM for use as auxiliary variables in the 
#                       analysis.
# Author: Michelle Pretorius

rm(list = ls())
library(dplyr)
library(prism)
library(terra)

# Download PRISM data -----------------------------------------------------

# set the directory that the prism data will be saved to:
prism_set_dl_dir("data/PRISM/")
prism_check_dl_dir()
PRISM_results <- list()

# Originally has more climate variables we looked at (hence why we created a loop) but ended up with just mean Temperature. Kept loop as may be helpful. 
layers <- c("tmean")

# Loop through PRISM layers
for(layer in layers) {
  
  print(paste0("Currently on layer ", layer))
  
  # This function automatically saves the files to the set directory (and unzips)
  get_prism_monthlys(type = layer, 
                 resolution = "800m",
                 year = 2020, 
                 mon = 8:12,
                 keepZip = FALSE)
}

climate_data <- ls_prism_data() %>%  
  prism_stack(.)  

# Extract PRISM vars at camera locations ----------------------------------

# Loop through downloaded PRISM files (each layer in different folder)
dirs <- list.dirs("data/PRISM/", recursive = F)
files <- c()

for (i in seq_along(dirs)) {
  f <- list.files(dirs[i], pattern = "\\.bil$", full.names = TRUE, recursive = T)
  files <- c(files, f)
}

# Create raster stack from the .tif files
stack <- raster::stack(files)

# Convert stack to raster with different layers
rast <- rast(stack)
rast <- project(rast, "EPSG:5070")

# Extract point value for each layer (and keep point info)
pts <- read.csv("data/SSUSA_ABB_2020_Camera_Point_Locations_ESPG5070.csv")
vect <- terra::vect(pts, geom=c("X", "Y"), crs="EPSG:5070")
vals <- terra::extract(rast, vect, bind=T) # NB specify terra:: as dplyr also has an extract function!

dat.df = as.data.frame(vals, geom="XY")
write.csv(dat.df, "data/SSUSA_ABB_2020_PRISM_Point_Locations.csv")
