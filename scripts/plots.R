library(dplyr)
library(plotly)
library(glue)
library(geojsonio)
library(jsonlite)

source("scripts/geomUtils.R")
#source("scripts/intersectShapes.R")

make_grid <- function (recs) {
  bbox <- st_bbox(recs)
  # bbox is supplied as xmin, ymin, xmax, ymax - that is, long_left, lat_bottom, long_right, lat_top
  longsize <- (bbox[3] - bbox[1]) / 20
  midlat <- (bbox[4] + bbox[2]) / 2
  latsize <- hortis.longToLat(longsize, midlat)
  grid <- st_make_grid(recs, cellsize = c(longsize, latsize))
  grid <- mutate(st_sf(grid), cell_id = 1:length(lengths(grid)))
  grid
}

plot_phenology <- function (pollinator, fieldName, fieldValue) {
  # Pattern from https://stackoverflow.com/questions/27197617/filter-data-frame-by-character-column-name-in-dplyr
  # Interesting discussion at https://www.tidyverse.org/blog/2019/06/rlang-0-4-0/
  filtered <- plant.poll.sf.labels %>% filter(!!as.symbol(fieldName) == fieldValue & scientificName == pollinator)
  cat("Filtered to ", nrow(filtered), " records ")
  filtered.recs <- st_drop_geometry(filtered) %>% mutate(month = factor(month(eventDate), levels = 1:12))
  # filtered
  bars <- filtered.recs %>% count(month, name = "count", .drop = FALSE)
  phen <- bars %>% plot_ly(x = c(1:12), y = ~count, type = 'bar') %>%
    layout(xaxis=list(tickvals=c(1:12), ticktext=month.name))
  print(phen)
  
  plant.counts <- filtered.recs %>% count(plantScientificName, name = "count")
  
  pie <- plot_ly(plant.counts, height = 520, labels = ~plantScientificName, values = ~count, type = 'pie') %>% 
    layout(title = str_glue("Proportions of {nrow(filtered)} plants visited by {pollinator} in {fieldValue}"),
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        autosize = T)
  print(pie)
  
  grid <- make_grid(filtered)
  # plot(grid)
  
  gridded <- filtered %>% st_join(grid) %>% st_drop_geometry
  
  summed <- gridded %>% group_by(cell_id) %>% count(name = "obs_count")
  
  summed.grid <- merge(grid, summed, by = "cell_id")
  
  # plot(summed.grid)
  
  grid.geojson.json <- geojson_json(summed.grid)
  grid.geojson <- fromJSON(grid.geojson.json)
  
  bbox <- st_bbox(filtered)
  
  map <- plot_ly() 
  map <- map %>% add_trace(
    summed,
    type = "choroplethmapbox",
    geojson = grid.geojson,
    locations = summed$cell_id,
    z = summed$obs_count,
    zmin = 0,
    zmax = 13,
    colorscale = "Viridis",
    featureidkey="properties.cell_id",
    marker=list(line=list(
      width=1),
      opacity=0.5
    )
  ) %>% layout(
    mapbox=list(
      style="carto-positron",
      center = list(lon = ((bbox[1] + bbox[3]) / 2), lat = ((bbox[2] + bbox[4]) / 2)),
      zoom = 6
      #bounds = list(west=bbox[1], east=bbox[3], north=bbox[4], south=bbox[2])
    )
  )
  map
  
  return (list(phen = phen, pie = pie, map = map))
}

plots <- plot_phenology("Osmia", "forest", "Deschutes National Forest")
