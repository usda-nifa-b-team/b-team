# TODO 
# make total figure size adapt to individual 
# figure out way to size figure appropriately

source("scripts/gbifNameParsing.R")
#library(pdftools)
library(tidyverse)
library(bipartiteD3)
source("scripts/bipartite_D3_plus_save.r")
library(r2d3)
library(bipartite)
library(RColorBrewer)
library(magick)
library(patchwork)
library(rsvg)
library(chromote)

#library(r2d3svg)
# originally had to install r2d3svg: remotes::install_github("usda-nifa-b-team/r2d3svg), should be unecessary now
# had to remove htmltools from appdata/local/r/winlibrary to get it to install
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

makeNetVis <- function(data, subsetName = "none", isMon = TRUE, isNF = FALSE,
                       static = TRUE, fileNm = NULL,
                       figTitle = "all"){ 

famCols <- tibble(cols=c(brewer.pal(5,'Set1'), "black"), 
                    fams = c('Andrenidae','Apidae','Colletidae',
                             'Halictidae','Megachilidae', "Melittidae"))
  if(!is.null(subsetName)){
if(isMon == TRUE){
df <- data %>% 
    filter(monument %in% subsetName) %>% 
  rename(place = monument) %>% 
  as_tibble() %>% 
  select(!geometry)
}
else if(isNF == TRUE){
df <- data %>% 
  filter(forest %in% subsetName) %>% 
  rename(place = forest)%>% 
  as_tibble() %>% 
  select(!geometry)
}
  }
else{ 
  df <- data %>% 
    mutate(place = figTitle) %>% 
    as_tibble() %>% 
    select(!geometry)
  }
famColoured <- df %>% 
    left_join(famCols, by = c("bee_family" = "fams"))
order <- famColoured %>% group_by(pollinatorINatName,bee_family,cols) %>% 
  summarise() %>% arrange(bee_family)
orderBeeName <- order$pollinatorINatName

famName <- famColoured %>% group_by(pollinatorINatName, cols) %>% summarise()
famNameVec <- as.character(famName$cols)
names(famNameVec) <- famName$pollinatorINatName

d3r <- toBipd3(df, plnt = plant_genus) %>% filter(!is.na(plant_genus))

if(static == FALSE){
  source("scripts/BP_JS_WriterReactive.R")
  outD3 <- bipartite_D3m(data = d3r,
                savePdf = F,
                file = NULL,
                MainFigSize = c(1100,1600),
                IndivFigSize = c(400,2000),
                Pad = 2,
                PercentageDecimals = 1,
                colouroption = 'manual',
                NamedColourVector = famNameVec, 
                ColourBy = 1,
                SortPrimary = orderBeeName,
                SiteNames = df$place, #not the most impressive way to do this but works
                PrimaryLab = "Bees", 
                SecondaryLab = "Plants"
  )
  return(outD3)
}

if(static == TRUE){
  source("scripts/BP_JS_WriterStatic.R")
  bipartite_D3m(data = d3r,
                savePdf = T,
                file = fileNm,
                MainFigSize = c(1100,1600),
                IndivFigSize = c(400,2000),
                Pad = 2,
                PercentageDecimals = 1,
                colouroption = 'manual',
                NamedColourVector = famNameVec, 
                ColourBy = 1,
                SortPrimary = orderBeeName,
                SiteNames = df$place, 
                PrimaryLab = "Bees", 
                SecondaryLab = "Plants"
  )
 }
}
makeNetVis(data = plantPolNamedsf, isMon = F, isNF=T, 
                    subsetName = unique(plantPolNamedsf$forest)[2], static = T, fileNm = "outNet2.pdf")

makeNetVis(data = plantPolNamedsf, isMon = T, isNF=F, 
           subsetName = unique(plantPolNamedsf$monument)[3], static = F)

