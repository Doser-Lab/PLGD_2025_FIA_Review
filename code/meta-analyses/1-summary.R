# 1-summary.R: summarize results and generate figures from meta-analysis data 
#   (`data/meta-analysis.csv`) from systematic review of FIA use in wildlife studies 
# Author: Michelle Pretorius

rm(list = ls())

#devtools::install_github("davidsjoberg/ggsankey")

library(extrafont)
library(ggsankey)
library(ggplot2)
library(dplyr)
library(tigris)
library(sf)
library(patchwork)

# Load in specific font used in figures
loadfonts(device = "win")
par(family = "LM Roman 10")

# Data loading and cleaning -----------------------------------------------

metadata <- read.csv("data/Review_Metadata.csv") %>%
  mutate(Taxa = case_when(Taxa == "Wildlife" ~ "Multiple taxa",  
                          TRUE ~ Taxa),
         Single.Multi.species = case_when(Single.Multi.species == "Multi" ~ "Multiple",  
                                            TRUE ~ Single.Multi.species))

# Supplementary Figure: Publication Timeline ------------------------------

pub.years <- metadata %>%
  group_by(Publication_year) %>%
  summarise(n.pubs = n())

ggplot(pub.years, aes(x=Publication_year, y=n.pubs)) + 
  geom_bar(stat = "identity") + 
  theme_classic() +
  scale_y_continuous(expand=c(0,0)) +
  scale_x_continuous(breaks=seq(1998, 2025, 1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        text = element_text(size = 16)) +
  xlab("Year") +
  ylab("Number of publications")

# Figure 2: Sankey Diagram ------------------------------------------------

TotalCount = nrow(metadata)

# Reformat data for flow visualization
data <- metadata %>% 
  dplyr::select(Topic, Data_source, Single.Multi.species, Taxa) %>% 
  mutate_all(na_if,"") %>%
  make_long(Topic,
            Data_source,
            Single.Multi.species,
            Taxa)

# Calculate percentages for labels
dagg <- data %>%
  dplyr::group_by(node)%>%
  tally() %>%
  dplyr::mutate(pct = n/TotalCount)

df <- merge(data, dagg, by.x = 'node', by.y = 'node', all.x = TRUE)

# Create initial plot
gg_plot <- ggplot(df, aes(x = x,
                          next_x = next_x,                                     
                          node = node,
                          next_node = next_node,        
                          fill = factor(node),
                          label = paste0(node,'\n', " n = ", 
                                         n, ' (',  round(pct* 100,1), '%)' )))

# Specify x-axis node labels
nodelabs <- c("Topic", "FIA use", "Species Count", "Taxa")

# Plot Sankey
gg_plot +
  geom_sankey(flow.alpha = 0.5,          
              node.color = "black",    
              show.legend = FALSE) +        
  geom_sankey_label(size = 5, 
                    color = "black", 
                    fill = "white",
                    family = "LM Roman 10") + 
  theme_bw() + 
  theme(legend.position = 'none') + 
  theme(axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "LM Roman 10", size = 20)) + 
  scale_fill_viridis_d(option = "inferno") +
  labs(fill = 'Nodes') + 
  scale_x_discrete(labels= nodelabs)

# Figure 3: Regional Distribution with Map Inset --------------------------

# Summarize study counts by region and state count
regData <- metadata %>%
  dplyr::select(FIA_region_grouped, Number_states) %>%
  filter(!FIA_region_grouped=="Multiple") %>%
  group_by(FIA_region_grouped, Number_states) %>%
  count(FIA_region_grouped)

# Group states in FIA regions
region_list <- list(
  "Rocky Mountain" = c("Arizona", "Colorado", "Idaho", "Montana", "New Mexico", 
    "Nevada", "Wyoming", "Utah"),
  "Pacific Northwest" = c("Alaska", "California", "Hawaii", "Oregon", "Washington",
    "American Samoa", "Micronesia", "Guam", "Marshall Islands", 
    "Northern Mariana Islands", "Palau"),
  "Northern" = c("Connecticut", "Delaware", "Iowa", "Illinois", "Indiana", "Kansas", 
    "Massachusetts", "Maryland", "Maine", "Michigan", "Minnesota", "Missouri", 
    "North Dakota", "Nebraska", "New Hampshire", "New Jersey", "New York", 
    "Ohio", "Pennsylvania", "Rhode Island", "South Dakota", "Vermont", 
    "Wisconsin", "West Virginia"),
  "Southern" = c("Alabama", "Arkansas", "Florida", "Georgia", "Kentucky", "Louisiana", 
    "Mississippi", "North Carolina", "Oklahoma", "South Carolina", 
    "Tennessee", "Texas", "Virginia", "Puerto Rico", "United States Virgin Islands"))

region_df <- bind_rows(
  lapply(names(region_list), function(region) {
    data.frame(code = region_list[[region]], region = region)})) %>%
  rename(NAME = code,
         FIA_region = region)

# Prepare spatial data
us_states_shift <- states(cb = TRUE, resolution = "20m") %>% 
  shift_geometry() %>%
  left_join(region_df, by = "NAME")

# Create inset map
us_plot <-  ggplot()+
  geom_sf(data = us_states_shift,
          aes(fill = FIA_region),
          color = "black")+
  theme_void()+
  scale_fill_viridis_d() +
  theme(legend.position = "none")

# Final Figure 3 with Inset
ggplot(data=regData, aes(x=Number_states, y=n, fill=FIA_region_grouped)) +
  geom_bar(stat="identity", width=1, colour="black") +
  scale_x_continuous(name="Number of states included in study", 
                     breaks=seq(0, 10,1), 
                     expand=c(0,0)) +
  scale_y_continuous(expand=c(0,0)) +
  ylab("Number of studies") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        text = element_text(family = "LM Roman 10", size = 20)) +
  coord_flip() +
  scale_fill_viridis_d(option = "viridis") +
  labs(fill='FIA Regions') +
  inset_element(us_plot, 0.4, 0.4, 1, 1)
