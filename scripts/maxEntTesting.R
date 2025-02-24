# using wallace to get a starting point
library(wallace)
library(sf)
library(tidyverse)
library(raster)

# replacing wallace function that doesn't error/notify when duplicate environmental values occur and fails to crop to the extent requested ----
penvs_bgMask_RRMOD <- function (occs, envs, bgExt, logger = NULL, spN = NULL) 
{
  if (is.null(bgExt)) {
    logger %>% writeLog(type = "error", hlSpp(spN), "Before sampling background points, define the background extent.")
    return()
  }
  smartProgress(logger, message = paste0("Masking rasters for ", 
                                         spName(spN), "..."), {
                                           bgCrop <- raster::crop(envs, bgExt)
                                           bgMask <- raster::mask(bgCrop, bgExt)
                                           occsEnvsVals <- as.data.frame(raster::extract(bgMask, 
                                                                                         occs[, c("longitude", "latitude")], cellnumbers = TRUE))
                                           occs.dups <- duplicated(occsEnvsVals[, 1])
                                           if (sum(occs.dups) > 0) {
                                             print("Duplicate environmental data present, cropping to extent will not occur")
                                             bgMask <- terra::project(terra::rast(bgMask), terra::rast(envs), 
                                                                      method = "near")
                                             bgMask <- methods::as(bgMask, "Raster")
                                           }
                                         })
  logger %>% writeLog(hlSpp(spN), "Environmental data masked.")
  return(bgMask)
}

# getting file to write a temp version ----
 source("scripts/intersectShapes.R")

# st_write(plant.poll.sf.labels, "pltPolSfLabels4testing.csv", append = FALSE)

# plant.poll.sf.labels <- read_csv("pltPolSfLabels4testing.csv") %>% 
#   mutate(obsNo =`Observation No.`)
# 
# plantPolGeom <- st_read("pltPolSfLabels4testing.shp") %>% 
#   dplyr::select(ObsrvN_, geometry)
# 
# plant.poll.sf.labels %>% left_join(plantPolGeom, by = c("obsNo" = "ObsrvN_"))


# might need this later to get only species with more than 20 obs - need to remove genus only spp. too
# plant.poll.sf.labels %>% group_by(scientificName) %>% 
#   summarise(n = n()) %>% 
#   filter(n>20)
  
# occurrences of Bombus caliginosus - should be a good test

bCalig <- plant.poll.sf.labels %>% filter(scientificName %in% "Bombus caliginosus") %>% 
  mutate(longitude = st_coordinates(.)[,1],
                      latitude = st_coordinates(.)[,2]) # get coords in columns for next step
sfOccs_Ab <- bCalig

occs_Ab <- st_drop_geometry(sfOccs_Ab)
# straight from wallace 

envs_Ab <- envs_worldclim(
  bcRes = 2.5, 
  bcSel = c('bio01', 'bio02', 'bio03', 'bio04', 'bio05', 'bio06', 'bio07', 'bio08', 'bio09', 'bio10', 'bio11', 'bio12', 'bio13', 'bio14', 'bio15', 'bio16', 'bio17', 'bio18', 'bio19'), 
  mapCntr = c(-120.4927, 44.1611), # Mandatory for 30 arcsec resolution   
  doBrick = FALSE)

occs_xy_Ab <- occs_Ab[c('longitude', 'latitude')]
occs_vals_Ab <- as.data.frame(raster::extract(envs_Ab, occs_xy_Ab, cellnumbers = TRUE))
# Remove duplicated same cell values
occs_Ab <- occs_Ab[!duplicated(occs_vals_Ab[, 1]), ]
occs_vals_Ab <- occs_vals_Ab[!duplicated(occs_vals_Ab[, 1]), -1]
# remove occurrence records with NA environmental values
occs_Ab <- occs_Ab[!(rowSums(is.na(occs_vals_Ab)) >= 1), ]
# also remove variable value rows with NA environmental values
occs_vals_Ab <- na.omit(occs_vals_Ab)
# add columns for env variable values for each occurrence record
occs_Ab <- cbind(occs_Ab, occs_vals_Ab)

occs_Ab <- occs_Ab %>% mutate(scientific_name = scientificName, occID = `Observation No.`)

# skipped a step here filtering occurrences within area - all should be within bounds of project

# thin occs to avg range size - saying 1.5km for Bombus 
# debugonce(poccs_thinOccs)

occs_Ab <- poccs_thinOccs(
  occs = occs_Ab, 
  thinDist = 1.5) #km 

# need to match env. variables to max possible - say oregon (or ecoregion?)
# debugonce(penvs_drawBgExtent)
bgExt_Ab <- penvs_drawBgExtent(
    polyExtXY = matrix(c(-124.550, -124.550 , -121.355, -121.355, 46.182, 42.072, 42.072, 46.182 ),ncol=2,byrow=FALSE), 
  polyExtID = "testID", 
  drawBgBuf = 0.1, 
  occs = occs_Ab)
# Mask environmental data to provided extent
# having trouble here - won't mask to extent, stays as maxes

# as far as I can see, bgExt is exactly the same structure as original
# gets to bg_crop within below function fine
# but when it masks, it doesn't work - error was due to an if statement that derails the process and uses terra, I'm not sure why or what it does
# debugonce(penvs_bgMask)
bgMask_Ab <- penvs_bgMask_RRMOD(
  occs = occs_Ab,
  envs = envs_Ab,
  bgExt = bgExt_Ab)

maxPoints <- raster::ncell(bgMask_Ab) - raster::freq(bgMask_Ab, 
                                     value = NA)[[1]]
# Sample background points from the provided area
bgSample_Ab <- penvs_bgSample(
  occs = occs_Ab,
  bgMask =  bgMask_Ab,
  bgPtsNum = maxPoints - (ceiling(0.01*maxPoints))) # taking off 1 percent just to be safe

# Extract values of environmental layers for each background point
bgEnvsVals_Ab <- as.data.frame(raster::extract(bgMask_Ab,  bgSample_Ab))

##Add extracted values to background points table
bgEnvsVals_Ab <- cbind(scientific_name = paste0("bg_", "Bombus caliginosus"), bgSample_Ab,
                       occID = NA, year = NA, institution_code = NA, country = NA,
                       state_province = NA, locality = NA, elevation = NA,
                       record_type = NA, bgEnvsVals_Ab)

# R code to get partitioned data
groups_Ab <- part_partitionOccs(
  occs = occs_Ab ,
  bg =  bgSample_Ab, 
  method = "block",
  bgMask = bgMask_Ab,
  aggFact = 2) 

# Run maxent model for the selected species
model_Ab <- model_maxent(
  occs = occs_Ab,
  bg = bgEnvsVals_Ab,
  user.grp = groups_Ab, 
  bgMsk = bgMask_Ab,
  rms = c(1, 2), 
  rmsStep =  1,
  fcs = 'L',
  clampSel = TRUE,
  algMaxent = "maxnet",
  parallel = TRUE,
  numCores = 4)


# Select current model and obtain raster prediction
m_Ab <- model_Ab@models[["fc.L_rm.2"]] 
predSel_Ab <- predictMaxnet(m_Ab, bgMask_Ab,
                            type = "cloglog", 
                            clamp = TRUE)
# extract the suitability values for all occurrences
occs_xy_Ab <- occs_Ab[c('longitude', 'latitude')]
# determine the threshold based on the current prediction
occPredVals_Ab <- raster::extract(predSel_Ab, occs_xy_Ab)
# Define probability of quantile based on selected threshold
thresProb_Ab <- switch("p10", 
                       "mtp" = 0, "p10" = 0.1, "qtp" = 0)
# Define threshold value
thres_Ab <- stats::quantile(occPredVals_Ab, probs = thresProb_Ab)
# Applied selected threshold
predSel_Ab <- predSel_Ab > thres_Ab

# Get values of prediction
mapPredVals_Ab <- getRasterVals(predSel_Ab, "cloglog")

raster::plot(predSel_Ab)
mapPredVals_Ab <- getRasterVals(predSel_Ab, "raw")
# Define colors and legend  
rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
legendPal <- colorNumeric(rev(rasCols), mapPredVals_Ab, na.color = 'transparent')
rasPal <- c('gray', 'blue')

maxentEvalPlot_Ab <- ENMeval::evalplot.stats(
  model_Ab,
  "auc.val",
  "rm",
  "fc")
#plot
maxentEvalPlot_Ab

n <- mxNonzeroCoefs(model_Ab@models[["fc.L_rm.2"]], "maxnet")

# Create response curves
for (i in n) {
  maxnet::response.plot(
    model_Ab@models[["fc.L_rm.2"]],
    v = i,
    type = "cloglog")
}

# Generate map
m <- leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) 

m  %>%
  leaflet::addLegend("bottomright", colors = c('gray', 'blue'),
                     title = "Thresholded Suitability<br>(Training)",
                     labels = c("predicted absence", "predicted presence"),
                     opacity = 1, layerId = "train") %>% 
  #add occurrence data
  addCircleMarkers(data = occs_Ab, lat = ~latitude, lng = ~longitude,
                   radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                   fillOpacity = 0.2, weight = 2) %>% 
  ##Add model prediction
  addRasterImage(predSel_Ab, colors = rasPal, opacity = 0.7,
                 group = 'vis', layerId = 'mapPred', method = "ngb") %>%
  ##add background polygons
  addPolygons(data = bgExt_Ab, fill = FALSE,
              weight = 4, color = "blue", group = 'proj')


#run_wallace()

# 0 to 1 suitability

m_Ab <- model_Ab@models[["fc.L_rm.2"]] 
predSel_Ab <- predictMaxnet(m_Ab, bgMask_Ab,
                            type = "logistic", 
                            clamp = TRUE)
# extract the suitability values for all occurrences
occs_xy_Ab <- occs_Ab[c('longitude', 'latitude')]
# determine the threshold based on the current prediction
occPredVals_Ab <- raster::extract(predSel_Ab, occs_xy_Ab)
# Define probability of quantile based on selected threshold
thresProb_Ab <- switch("p10", 
                       "mtp" = 0, "p10" = 0.1, "qtp" = 0)
# Define threshold value
thres_Ab <- stats::quantile(occPredVals_Ab, probs = thresProb_Ab)
# Applied selected threshold
predSel_Ab <- predSel_Ab > thres_Ab

# raster::plot(predSel_Ab) # checks that the output looks okay

# Get values of prediction
mapPredVals_Ab <- getRasterVals(predSel_Ab, "logistic")


# Define colors and legend  
rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
legendPal <- colorNumeric(rev(rasCols), mapPredVals_Ab, na.color = 'transparent')
rasPal <- c('gray', 'blue')
