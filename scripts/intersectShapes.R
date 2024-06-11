library(dplyr)

source("scripts/utils.R")
source("scripts/fetchShapes.R")

# Necessary otherwise we get Loop 0 is not valid: Edge x has duplicate vertex with edge y. since S2 library is fussy about geometry
sf::sf_use_s2(FALSE)

forests <- mx_read("drive_data/FS_National_Forests_Dataset")
monuments <- mx_read("drive_data/BLM_National_Monuments")
ecol3 <- mx_read("drive_data/us_eco_l3/")

plant.poll <- timedFread("tabular_data/plant-pollinators-OBA-assigned-subset.csv")

plant.poll.sf <- st_as_sf(plant.poll, coords = c("decimalLongitude", "decimalLatitude"), crs=4326) # CRS is WGS:1984

monuments.intersection <- as.integer(st_intersects(plant.poll.sf, monuments))
forests.intersection <- as.integer(st_intersects(plant.poll.sf, forests))
ecol3.intersection <- as.integer(st_intersects(plant.poll.sf, ecol3))

addLabels <- function (obs) {
  obs.labels <- obs %>% mutate(
    monument = if_else(is.na(monuments.intersection), '', monuments$Label[monuments.intersection]),
    forest = if_else(is.na(forests.intersection), '', forests$FORESTNAME[forests.intersection]),
    US_L3CODE = if_else(is.na(ecol3.intersection), '', ecol3$US_L3CODE[ecol3.intersection])
  )
}

plant.poll.sf.labels <- addLabels(plant.poll.sf)
plant.poll.labels <- addLabels(plant.poll)

timedWrite(plant.poll.labels, "tabular_data/plant-pollinators-OBA-assigned-subset-labels.csv")

l3regions <- ecol3 %>% st_drop_geometry %>% dplyr::distinct(US_L3CODE, US_L3NAME) %>% mutate_at("US_L3CODE", as.numeric)  %>% arrange(US_L3CODE)
timedWrite(l3regions, "tabular_data/us-eco-l3-regions.csv")

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
  if (name != "") {
      tabOneForest(name)
  }
}
