library(sf)
library(dplyr)
library(purrr)
library(plotly)
library(glue)
library(geojsonio)
library(rjson)
library(rlist)

source("scripts/utils.R")

# cribbed from https://github.com/dkahle/ggmap/blob/master/R/calc_zoom.r
calc_zoom <- function (bbox) {
  lonlength <- bbox$xmax - bbox$xmin
  latlength <- bbox$ymax - bbox$ymin
  zoomlon <- ceiling(log2(360 * 2/lonlength))
  zoomlat <- ceiling(log2(180 * 2/latlength))
  zoom <- min(zoomlon, zoomlat)
}

stylesNonMapbox <- rjson::fromJSON(file = "src/json/stylesNonMapbox.json", simplify = FALSE)
baseStyle <- stylesNonMapbox[["carto-positron"]]

read_vectors <- function (layerNames) {
  fileNames <- glue('spatial_data/vectors/{layerNames}');
  lapply(fileNames, FUN=mx_read);
}

feature_to_geojson <- function (feature) {
  feature.geojson.json <- geojson_json(feature);
  feature.geojson <- rjson::fromJSON(feature.geojson.json);
}

load_and_convert_geojson <- function (styling) {
  styling$data <- read_vectors(styling$Layer);
  
  start <- Sys.time()
  styling$geojson <- lapply(styling$data, feature_to_geojson)
  end <- Sys.time()
  message("Converted ", nrow(styling), " rows to GeoJSON in ", round(end - start, 3), "s")
  styling
}

rowsToSources <- function (rows) {
  sources <- list()
  for(i in 1:nrow(rows)) {
    row <- rows[i,]
    # Again God only knows why it ends up wrapped up back in a 1-element list again
    sources[[row$Layer]] = list(type = "geojson", data = row$geojson[[1]])
  }
  sources
}

rowsToLayers <- function (rows, highlightedLayers) {
  # Massive guide to iterating over rows in R all of which are very silly: https://github.com/jennybc/row-oriented-workflows/tree/master
  layers <- list()
  
  for(i in 1:nrow(rows)) {
    row <- rows[i,]
    message("Layer ", row$Layer, " opacity ", row$fillOpacity)
    # Due to a limitation in WebGL we need to add separate features for outline and fill
    if(row$fillOpacity != 0) {
      message("fillLayer ", row$Layer)
      fillLayer <- list(type="fill", id=row$Layer, source=row$Layer, "fill-sort-key"=row$Z_Order, "mx-fill-pattern"=row$fillPattern,
                        label=row$Label,
                        paint=list("fill-color"=row$fillColor, "fill-opacity"=row$fillOpacity))
      # Nutty syntax explained in https://stackoverflow.com/questions/14054120/adding-elements-to-a-list-in-r-in-nested-lists
      layers <- c(layers, list(fillLayer))
    }
    if (row$Layer %in% highlightedLayers) {
      # ref: https://plotly.com/python/reference/layout/mapbox/
      outlineLayer <- list(type="line", id=glue("{row$Layer}-highlight"), source=row$Layer, "line-sort-key"=5,
                           paint=list("line-color"="yellow", "line-width"=row$outlineWidth+2, "line-opacity"=row$outlineOpacity))
    } else {
      outlineLayer <- list(type="line", id=glue("{row$Layer}-outline"), source=row$Layer, "line-sort-key"=row$Z_Order + 0.5,
                           paint=list("line-color"=row$outlineColor, "line-width"=row$outlineWidth, "line-opacity"=row$outlineOpacity))
    }
    layers <- c(layers, list(outlineLayer))
  }
  layers
}

plot_mapbox_map = function (id, bbox, sources, styling, highlightedLayers) {

  allSources <- c(baseStyle$sources, sources);
  allLayers <- c(baseStyle$layers, rowsToLayers(styling, highlightedLayers));
  
  usedSourceNames <- sapply(allLayers, function (layer) {layer$source});
  usedSourcesIndex = sapply(names(allSources), function (name) {name %in% usedSourceNames})
  usedSources <- allSources[usedSourcesIndex]

  style <- list(id=id, version=8, sources=usedSources, layers=allLayers, glyphs=baseStyle$glyphs);

  map <- plot_ly(height = 600, width = 800)

  map <- map %>% add_trace(
    type = "choroplethmapbox"
  )
  
  map <- map %>% layout(
    # TODO: This title doesn't display
    legend = list(title = "Regions"),
    mapbox=list(
      style=style,
      center = list(lon = ((bbox[1] + bbox[3]) / 2), lat = ((bbox[2] + bbox[4]) / 2)),
      zoom = calc_zoom(bbox) - 1.2,
      # Put this on to avoid blowing up the plotly driver
      "_fitBounds" = list(bounds = bbox)
    )
  )
  map
}
