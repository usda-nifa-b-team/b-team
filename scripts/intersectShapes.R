library(dplyr)

source("scripts/utils.R")
source("scripts/fetchShapes.R")

# Necessary otherwise we get Loop 0 is not valid: Edge x has duplicate vertex with edge y. since S2 library is fussy about geometry
sf::sf_use_s2(FALSE)

forests <- mx_read("drive_data/FS_National_Forests_Dataset")
monuments <- mx_read("drive_data/BLM_National_Monuments")

plant.poll <- timedFread("tabular_data/plant-pollinators-OBA-assigned.csv")

plant.poll.sf <- st_as_sf(plant.poll, coords = c("decimalLongitude", "decimalLatitude"), crs=4326) # CRS is WGS:1984

monuments.intersection <- as.integer(st_intersects(plant.poll.sf, monuments))

plant.poll.sf.monuments <- plant.poll.sf %>% mutate(
    monument = if_else(is.na(monuments.intersection), '', monuments$Label[monuments.intersection])
) 

forests.intersection <- as.integer(st_intersects(plant.poll.sf, forests))

plant.poll.sf.labels <- plant.poll.sf %>% mutate(
  monument = if_else(is.na(monuments.intersection), '', monuments$Label[monuments.intersection]),
  forest = if_else(is.na(forests.intersection), '', forests$FORESTNAME[forests.intersection])
)

monument.table <- table(plant.poll.sf.labels$monument)
monument.table
forest.table <- sort(table(plant.poll.sf.labels$forest), decreasing = T)
forest.table

tabOneForest <- function (name) {
  withForest <- plant.poll.sf.labels[plant.poll.sf.labels$forest == name, ]
  withFreq <- sort(table(withForest$scientificName), decreasing = T)
  count <- min(length(withFreq), 10)
  withFreq <- withFreq[1:count]
  cat("Top", count, " pollinators for ", name)
  print(withFreq)
  cat("\n")
}

forestNames <- unlist(dimnames(forest.table))

for (row in 1:length(forestNames)) {
  name <- forestNames[row]
  tabOneForest(name)
}
