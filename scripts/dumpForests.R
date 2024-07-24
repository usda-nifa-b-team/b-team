library(tidyverse)
library(sf)

source("scripts/intersectShapes.R")

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