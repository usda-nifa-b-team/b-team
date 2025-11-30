# Ecoregion Summaries ---- 
# Nov 2025

# goals: 

  # pollinator host info by ecoregion 

  # introduction page/summary

library(sf)
library(knitr)
library(kableExtra)
library(ggpubr)
library(RColorBrewer)
library(ggspatial) #library(ggsn) replaced
library(shadowtext)
library(tinytex)
library(tidyverse)
library(dplyr)
library(iNEXT) # check name/install

load('Robinson/Data/cleanedV3.Rdata') # or load('Data/cleaned.Rdata')

# actually consider making this stateless? Just might get weird for edge of range - e.g, predicting half of NM with only sliver on W edge 
datSt <- dat %>% 
  filter(state %in% "WA")

datEcoR <- datSt %>% # match ecoreg to pnts
  st_join(ecoRegSimplified, st_within)

# some are just outside (showing up in water probably, or minor CRS diffs) - for now just used st_nearest, might not be perfect
datEcoR_nearest <- datSt %>% semi_join(datEcoR %>% filter(is.na(L3_KEY)) %>% st_drop_geometry()) %>% 
  st_join(ecoRegSimplified, st_nearest_feature) # %>% filter(is.na(L3_KEY))

datEcoR <- datEcoR %>% # add ones back in that needed it
  anti_join(datEcoR_nearest %>% st_drop_geometry(), 
            join_by(collector, day, month, year, verbatimEventDate, country, state, county, locality, phylumPlant, orderPlant,
                    familyPlant, genusPlant, plantGenSpp, taxonRankPlant, familyVolDet, genusVolDet, specificEpithetVolDet, sexVolDet, casteVolDet, genus,
                    species, taxonomist, sex, caste, samplingProtocol, date, volunteerGenSpp, genSpp, family)) %>% 
  rbind(datEcoR_nearest)

intsPerEcoR <- datEcoR %>% st_drop_geometry() %>% 
  filter(!is.na(genSpp)) %>% 
  filter(!is.na(genusPlant)) %>% 
  group_by(name, genSpp, genusPlant) %>% # might adjust this later - using plant genus
  count()

### functions ----
perPlnt_df <- function(df, ecor = "Columbia Plateau"){
  perPlnt <- df %>% 
    filter(name %in% ecor) %>% 
    group_by(name, genusPlant) %>% 
    summarise(nBees = sum(n),
              nBeeSpp = n_distinct(genSpp), 
              beeSpp = paste0(genSpp, collapse = ", "))
  return(perPlnt) 
}

top10 <- function(df, varName){
  dO <- df %>% 
    arrange(desc({{varName}})) %>% 
    head(n = 10)
  return(dO)
}

## Recommendation page ----

### Top 10 plants for bee richness ----

# TODO - use params here to specify ecoregion name

# bee abund, spp. per plant, names

#debugonce(top10)

perPlntGener <- perPlnt_df(intsPerEcoR)
# top 10 for abund. 
top10(perPlntGener, varName = nBees)

# top 10 for spp
top10(perPlntGener, varName = nBeeSpp)

### Plants for specialists ----

# figure it out or use list from the experts? 
perPlntSpec <- intsPerEcoR %>% 
  semi_join(specList, by = "genSpp") %>% 
  perPlnt_df(., ) # get specialist version

# top 10 for abund. 
top10(perPlntSpec, varName = nBees)

# top 10 for spp
top10(perPlntSpec, varName = nBeeSpp)


### Plants for listed spp. ----

# how to get ranks - just ask LR? 

perPlntRare <- intsPerEcoR %>% 
  semi_join(rareList, by = "genSpp") %>% 
  perPlnt_df(., ) # get specialist version

# top 10 for abund. 
top10(perPlntRare, varName = nBees)

# top 10 for spp
top10(perPlntRare, varName = nBeeSpp)
