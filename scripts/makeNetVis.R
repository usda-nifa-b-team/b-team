source("scripts/intersectShapes.R")
source("scripts/modD3_fxns.R")

library(tidyverse)
library(bipartiteD3)
library(r2d3)
library(bipartite)
library(RColorBrewer)

# fixing some names
filtSubset <- plant.poll.sf.labels %>% 
  filter(!grepl("\"", plantINatName)) %>% 
  filter(!grepl("\\(", plantINatName)) %>% 
  filter(!grepl("or", plantINatName)) %>% 
  filter(!grepl("and", plantINatName)) %>% 
  filter(!grepl(",", plantINatName)) %>% 
  filter(!grepl("ceae",plantINatName)) %>% 
  filter(!grepl("eae",plantINatName)) %>%
  mutate(plantINatName = str_remove(plantINatName,"sp.")) %>%
  mutate(plantINatName = str_remove(plantINatName,"sp.")) %>% 
  mutate(plantINatName = str_remove(plantINatName,"Genus")) %>% 
  filter(plantINatName!="ground") %>% 
  filter(plantINatName!="Life") %>% 
  mutate(plantINatName = str_squish(plantINatName)) %>% 
  filter(plantINatName!= "unknown")

steensExample <- filtSubset %>% 
  filter(monument %in% "Steens Mountain CMPA") 

d3ready <- toBipd3(areaSubset = steensExample)

#modded functions from bipartite_D3

bipartiteD3(d3ready,
             MainFigSize = c(400,1760),
             IndivFigSize = c(400,1550),
             PercPos = c(340,450),
             Pad = 2,
             PercentageDecimals = 1,
             SiteNames = steensExample$monument,
             PrimaryLab = "Bees",
             SecondaryLab = "Plants"
)
