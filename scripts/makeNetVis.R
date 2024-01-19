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

makeNetVis <- function(data, subsetName, isMon = TRUE, isNF = FALSE, static = TRUE, fileNm = NULL){ 

famCols <- tibble(cols=c(brewer.pal(5,'Set1'), "black"), 
                    fams = c('Andrenidae','Apidae','Colletidae',
                             'Halictidae','Megachilidae', "Melittidae"))
  
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
  source("bipartiteD3/R/BP_JS_Writer4.R")
  outD3 <- bipartite_D3m(data = d3r,
                savePdf = F,
                file = NULL,
                MainFigSize = c(1100,1600),
                IndivFigSize = c(400,1200),
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
  source("bipartiteD3/R/BP_JS_Writer2.R")
  bipartite_D3m(data = d3r,
                savePdf = T,
                file = fileNm,
                MainFigSize = c(1100,1600),
                IndivFigSize = c(400,1200),
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
                    subsetName = unique(plantPolNamedsf$forest)[2], static = F, fileNm = "outNet2.pdf")
