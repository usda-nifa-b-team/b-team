source("scripts/intersectShapes.R")

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
rm(bipartiteD3)
library(bipartiteD3)
netVis <- bipartite_D3(d3ready,
             MainFigSize = c(2000,1760),
             IndivFigSize = c(400,1550),
             PercPos = c(340,450),
             Pad = 2,
             PercentageDecimals = 1,
             SiteNames = steensExample$monument,
             PrimaryLab = "Bees",
             SecondaryLab = "Plants"
)
#save_d3_html(netVis, "steensTest.html")

library(r2d3svg)
r2d3svg::save_d3_svg(netVis, "visFile.svg")

library(magick)
library(patchwork)
library(rsvg)

test <- image_read_svg("visFile.svg")

rsvg::rsvg_pdf("visFile.svg", file = "testSvg.pdf", css = "bipartiteD3Script.css")

