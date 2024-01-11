source("scripts/intersectShapes.R")
#library(pdftools)
library(tidyverse)
library(bipartiteD3)
library(r2d3)
library(bipartite)
library(RColorBrewer)

library(magick)
library(patchwork)
library(rsvg)

#library(r2d3svg)
#need to install r2d3svg: remotes::install_github("usda-nifa-b-team/r2d3svg)
# i had to remove htmltools from appdata/local/r/winlibrary to get it to install
#remotes::install_github("usda-nifa-b-team/r2d3svg")

toBipd3 <- function(areaSubset, pol = pollinatorINatName, plnt = plantINatName){
  d3Dat <- areaSubset %>% 
    as_tibble() %>% 
    group_by({{pol}},{{plnt}}) %>% 
    summarise(n = n()) %>% 
    as_tibble() %>% 
    dplyr::select({{pol}},{{plnt}},n)
  return(d3Dat)
}
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

netVis <- bipartite_D3(d3ready,
             MainFigSize = c(990,1760),
             IndivFigSize = c(400,1550),
             PercPos = c(340,450),
             Pad = 2,
             PercentageDecimals = 1,
             SiteNames = steensExample$monument,
             PrimaryLab = "Bees",
             SecondaryLab = "Plants"
)
# save_d3_html(netVis, file = "d3.html") option to save as html

rsvg::rsvg_pdf("visFile2.svg", file = "testSvg2.pdf", css = "bipartiteD3Script.css")

#TODO
#automate the network generation process
#figure out relationship between network size/num spp, parameters etc.
#convert percentages to counts?
#add code to  colour and sort by family/genus
source("scripts/bipartite_D3_plus_save.r")

bipartite_D3m(d3ready,
              savePdf = T,
              file = "test.pdf",
             MainFigSize = c(1390,1760),
             IndivFigSize = c(400,1550),
             PercPos = c(340,450),
             Pad = 2,
             PercentageDecimals = 1,
             SiteNames = steensExample$monument,
             PrimaryLab = "Bees",
             SecondaryLab = "Plants"
)

