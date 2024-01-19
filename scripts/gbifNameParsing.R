#gbif name matching
source("scripts/intersectShapes.R")

library(rgbif)

plantNames <- as_tibble(plant.poll.sf.labels %>% 
  group_by(plantINatName) %>% 
  summarise() %>% 
  rename(name = plantINatName)) %>% 
  select(!geometry) %>% 
  mutate(kingdom = "Plantae", phylum = "Tracheophyta")
 
plantOutNames <- name_backbone_checklist(plantNames, verbose = TRUE)

#needs improvement - fuzzy matches and synonyms aren't working properly for a few species 
bestPlantNames <- plantOutNames %>% 
  filter(verbatim_name!= "Anthemideae") %>% 
  filter(phylum == "Tracheophyta") %>% 
  filter(matchType!= "HIGHERRANK") %>% 
  group_by(verbatim_name) %>% 
  arrange(desc(confidence)) %>% 
  slice_head(n = 1) %>% 
  select(verbatim_name, species, canonicalName, everything()) %>% 
  filter(confidence > 89)

plantLookup<- bestPlantNames %>% group_by(verbatim_name, species, canonicalName, order, family, genus) %>% 
  summarise() %>% 
  rename_with(., 
              ~ paste0("plant_", .x)
              )

beeNames <- as_tibble(plant.poll.sf.labels %>% 
                         group_by(pollinatorINatName) %>% 
                         summarise() %>% 
                         rename(name = pollinatorINatName)) %>% 
  select(!geometry) %>% 
  mutate(family = "Hymenoptera")

beeOutNames <- name_backbone_checklist(beeNames, verbose = TRUE) 

#possible problem names
probBees <- beeOutNames %>% filter(matchType == "HIGHERRANK") %>% 
  group_by(verbatim_name) %>% summarise(n = n()) %>% 
  filter(n<2)

#currently - only missing one name: Lasioglossum helianthi, excluded for now
bestBeeNames <- beeOutNames %>% 
  filter(status != "DOUBTFUL") %>% 
  filter(order == "Hymenoptera") %>% 
  filter(matchType!= "HIGHERRANK") %>% 
  group_by(verbatim_name) %>% 
  arrange(desc(confidence)) %>% 
  slice_head(n = 1)
  
beeLookup <- bestBeeNames %>% group_by(verbatim_name, species, canonicalName, order, family, genus) %>% 
  summarise() %>% 
  rename_with(., 
              ~ paste0("bee_", .x)
  )

 # matching to main data ----

plantPolNamedsf <- plant.poll.sf.labels %>% 
  semi_join(bestPlantNames, by = c("plantINatName" = "verbatim_name")) %>% 
  semi_join(bestBeeNames, by = c("pollinatorINatName" = "verbatim_name")) %>% left_join(beeLookup, by = c("pollinatorINatName" = "bee_verbatim_name")) %>% 
  left_join(plantLookup, by = c("plantINatName" = "plant_verbatim_name"))

