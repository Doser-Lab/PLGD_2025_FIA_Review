# Applications of National Forest Inventory data for wildlife conservation and management.

### [Michelle Pretorius](https://www.doserlab.com/), [Angela L. Larsen-Gray](https://www.ncasi.org/), [Jeffrey W. Doser](https://www.doserlab.com/)

<!-- # ### Forest Ecology and Management -->

<!-- # ### Code/Data DOI: -->

### Please contact the first author for questions: Michelle Pretorius (mpretor@ncsu.edu)

---------------------------------

<!-- ## Abstract -->

## Software Requirements

This analysis was conducted using R (version 4.5.1). To ensure reproducibility, we used the following package versions:

+ `spOccupancy`: 0.8.0
+ `sf`: 1.0-21
+ `terra`: 1.8-60
+ `tigris`: 2.2.1
+ `prism`: 0.3.0

## Repository Directory

### [code/meta-analysis](./code/meta-analysis/)

+ `1-summary.R`: summarize results and generate figures from meta-analysis data (`data/meta-analysis.csv`) from systematic review of FIA use in wildlife studies 

### [code/case-study](./code/case-study/)

Contains all code to format and extract data, fit models, and summarize results from the analysis of  American black bear (*Ursus americanus*) occurrence across CONUS using FIA-derived forest attributes ([TreeMap v2020](https://data.fs.usda.gov/geodata/rastergateway/treemap/)) extracted at multiple spatial scales (200m, 1km, 5km) paired with a continent-wide camera trap dataset ([Snapshot USA](https://www.snapshot-usa.org/)). Note that scripts should be run in the order indicated by the numbers in the file names.

+ `1a-prep-data.R`: cleans the Snapshot USA 2020 deployment data (needs to be downloaded; referenced as `SSUSA_Full_2020_Deployments.csv`), saves camera locations as `.csv` for use in subsequent scripts and creates the study map presented in manuscript.
+ `1b-get-GEE.txt`: contains two distinct Google Earth Engine (GEE) scripts to extract forest metrics across 200m, 1km, and 5km buffers for each station. These are not R code and must be run in the [Google Earth Engine Code Editor](https://code.earthengine.google.com/):
  + Script 1: extract percentage cover for hard/soft masting species (according to `FORTYPCD`). 
  + Script 2: extract live canopy cover (`CANOPYPCT`), and above-ground standing deadwood biomass (`DRYBIO_D`).
  + **Note:** Each script should be copied and run separately. Ensure you have uploaded the camera point locations (`data/SSUSA_ABB_2020_Camera_Point_Locations.csv`) as an Asset in GEE before running.
+ `1c-get-covariates.R`: extracts data from PRISM for use as auxiliary variables in the analysis.
+ `1d-format-data.R`: formats Snapshot USA black bear occurrence data (needs to be downloaded; referenced as`SSUSA_Full_2020_Sequences.csv`) and covariates into the necessary format for fitting the single-species occupancy models in `spOccupancy`.
+ `2a-initial-model.R`: runs a smaller occupancy models for each scale (200m, 1km, 5km) and extracts initial values for each scale for use in `2b-main-model.R`
+ `2b-main-model.R`: runs the full occupancy models for each scale (200m, 1km, 5km), compares models using WAIC and saves results for use in `3a-visualisations.R`.
+ `3a-visualisations.R`: summarize results from the black bear case study and generate all case study relevant figures included in the manuscript.  

### [data](./data/)

Contains data used in Systematic Literature Review and the American black bear distribution case study. Note that  Snapshot USA full deployment and sequence records for 2020 are too large to store on GitHub. These can be downloaded from [WildlifeInsights](https://app.wildlifeinsights.org/initiatives/2000156/Snapshot-USA). Other required data, including PRISM and point locations, can be generated with the scripts in the `code/` directory. 

+ `Review_Metadata.csv`: meta data for 138 studies identified in systematic literature review. Metadata includes Article title, DOI (NA where DOI not present), Publication type	Publication year,	Taxa, Common names, Number of species (NA if the total number of species was not quantified),	Single or Multiple species, Number of US States, FIA Region list, FIA Region grouped, Topic, Data source, and Data summary level. 
+ `Forest_TreeMap_[scale].csv`: Forest variables extracted from TreeMap (see `1b-get-GEE.txt` for GEE scripts) on live canopy cover (`CANOPYPCT`), and above-ground standing deadwood biomass (`DRYBIO_D`) across three scales: 200m, 1000m, and 5000m. (e.g., `Forest_TreeMap_200.csv`, etc.)
+ `Mast_TreeMap_[scale].csv`: Forest variables extracted from TreeMap (see `1b-get-GEE.txt` for GEE scripts) on hard/soft masting species (according to `FORTYPCD`) across three scales: 200m, 1000m, and 5000m. (e.g., `Mast_TreeMap_200.csv`, etc.)
+ `IUCN_range_map_ABB/`: folder containing spatial files for the American black bear [IUCN](https://www.iucnredlist.org/species/41687/114251609) geographic range.

### [results](./results/)

Note that model-based results files are too large to store on GitHub, but these files can be generated with the scripts in the `code/` directory.  

+ `Initial_PGOcc_SSUSA_ABB_2020_[scale].rda`: initial values from a smaller model run used to aid in convergence in the final model run across three scales: 200m, 1000m, and 5000m. (e.g., `Initial_PGOcc_SSUSA_ABB_2020_200.rda`, etc.)

### [figures](./figures/)

Contains all figures included in the manuscript and supplemental information. These figures can all be reproduced from the scripts in the `code/` directory.
