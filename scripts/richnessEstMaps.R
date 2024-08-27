library(tidyverse)
library(sf)
library(iNEXT) # package that does spp. richness estimates

source("scripts/intersectShapes.R")

l4Eco<- mx_read("drive_data/us_eco_l4/")

l4EcoLatLong <- l4Eco %>% st_transform(., crs = 4326)

l4Joined <- l4EcoLatLong %>% st_join(plant.poll.sf.labels) # find which pollinators are in which ecoregion

# get abundance in each ecoregion and convert to matrix needed for iNEXT
forEst <- l4Joined %>% 
  dplyr::select(US_L4NAME, pollinatorAssignedINatName) %>% st_drop_geometry() %>% 
  filter(!is.na(pollinatorAssignedINatName)) %>%  
  group_by(US_L4NAME, pollinatorAssignedINatName) %>% 
  summarise(n = n()) %>% 
  pivot_wider(names_from = US_L4NAME, values_from = n, values_fill = 0) %>% 
  column_to_rownames(var = "pollinatorAssignedINatName")

chaoEst <- ChaoRichness(forEst) # create estimates
ChaoRichness(forEst)
# get proportion of observed/estimated total
propObs <- chaoEst %>% rownames_to_column(var = "L4Ecoregion") %>% 
  mutate(prop = Observed/Estimator)

#ggiNEXT(iNEXT(forEst$`Semiarid Uplands`))
#adding geometry back in
sppEstSf <- l4EcoLatLong %>% left_join(propObs, by = c("US_L4NAME" = "L4Ecoregion"))

st_write(sppEstSf, "spatial_data/vectors/RichnessEstimate", driver="ESRI Shapefile");

#making map coloured by proportion

sppEstSf %>% 
  ggplot(aes(geometry = geometry))+
  geom_sf(aes(fill = prop))+
  scale_fill_viridis_c(alpha = 0.7)+
  labs(fill = "Proportion Observed vs. Estimated Total Richness")

# a test of observed richness only
sppEstSf %>% 
  ggplot(aes(geometry = geometry))+
  geom_sf(aes(fill = Observed))+
  scale_fill_viridis_c(alpha = 0.8)
  labs(fill = "Observed Richness")

