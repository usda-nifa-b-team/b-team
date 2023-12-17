source("scripts/intersectShapes.R")
library(pdftools)
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
#save_d3_html(netVis, "testVis.html")
source("r2d3svg/R/save_d3_pdf.R")
source("r2d3svg/R/save_d3_svg.R")
wid <- 1200
hgt <- 1760
save_d3_svg(netVis, "visFile2.svg", width = wid, height = hgt)
#debugonce(save_d3_svg)
#need to recode save d3 svg to open with a window 10% bigger than the visualization

rsvg::rsvg_pdf("visFile2.svg", file = "testSvg2.pdf", css = "bipartiteD3Script.css")
