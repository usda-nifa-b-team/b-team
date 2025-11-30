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

makeMaxEnt <- function(allOccs, specSpp = NA, specState = NA, statePoly = usStates, boundPoly = NULL, yesNo = FALSE){
  if(is.na(specSpp)) { # make sure one spp is provided at a time
    nSpp <- n_distinct(allOccs$genSpp) 
    if(nSpp!=1) {
    stop("Specify a species, there is more than one in data")  
      }
    else if(nSpp ==1){
      specSpp <- allOccs$genSpp[[1]]
    }
  }
  
  if(!is.na(specState)){ 
    allOccs <- allOccs %>% 
      filter(state %in% specState) 
    }

  sfOcc <- allOccs %>% # convert all occurrences to format ready for maxent fxns
    filter(genSpp %in% specSpp) %>% 
    mutate(longitude = st_coordinates(.)[,1],
           latitude = st_coordinates(.)[,2])

  occs <- st_drop_geometry(sfOcc)
  
  # masking fxns can't handle multipolygons, so create min reasonable concave hull ratio for each state
  usStates$concaveHullRatio = c(0.05, 0.08, 0.1, 0.1, 0.3,0.05,0.05,0.2, 0.01, 0.01, 0.2 )
  # based on following order: "ID" "WA" "NM" "CA" "CO" "UT" "WY" "NV" "MT" "AZ" "OR"
  
  # comes straight from Wallace ---- 
  
  envs <- envs # must run envs_worldclim before this 
  
  occs_xy <- occs[c('longitude', 'latitude')]
  occs_vals <- as.data.frame(raster::extract(envs, occs_xy, cellnumbers = TRUE))
  # Remove duplicated same cell values
  occs <- occs[!duplicated(occs_vals[, 1]), ]
  occs_vals <- occs_vals[!duplicated(occs_vals[, 1]), -1]
  # remove occurrence records with NA environmental values
  occs <- occs[!(rowSums(is.na(occs_vals)) >= 1), ]
  # also remove variable value rows with NA environmental values
  occs_vals <- na.omit(occs_vals)
  # add columns for env variable values for each occurrence record
  occs <- cbind(occs, occs_vals)
  
  occs <- occs %>% mutate(scientific_name = genSpp) # has an occID col as created above
  
  # skipped a step here filtering occurrences within area - all should be within bounds of project
  
  # thin occs to avg range size - saying 1.5km for Bombus 
  # debugonce(poccs_thinOccs)
  
  occs <- poccs_thinOccs(
    occs = occs, 
    thinDist = 1.5) #km 

  if(!is.na(specState))
  { 
    boundPoly <- statePoly %>% filter(STUSPS == specState) # if state specified, filter states poly to specified state 
    
    includePointsBox <- st_coordinates(st_concave_hull(boundPoly, ratio = boundPoly$concaveHullRatio))[, c(1,2)] # get matrix of polygon boundary to mask to, only cols 1 and 2 
      }
  else if (is.na(specState)){
    includePointsBox <- st_coordinates(st_as_sfc(st_bbox(sfOcc)))[,c(1,2)]
  }
    
  # need to match env. variables to max possible - say oregon (or ecoregion?)
  # debugonce(penvs_drawBgExtent)
  biogExt <- penvs_drawBgExtent(
    polyExtXY = includePointsBox, 
    polyExtID = "testID", 
    drawBgBuf = 0.5, 
    occs = occs)
  # Mask environmental data to provided extent
  # having trouble here - won't mask to extent, stays as maxes - maybe fixed with concave hull poly creation?
  
  # as far as I can see, biogExt is exactly the same structure as original
  # gets to bg_crop within below function fine
  # but when it masks, it doesn't work - error was due to an if statement that derails the process and uses terra, I'm not sure why or what it does
  # debugonce(penvs_bgMask)
  biogMask <- penvs_bgMask(occs = occs, 
               envs = envs, 
               bgExt = biogExt)
  
  # biogMask <- penvs_bgMask_RRMOD(
  #   occs = occs,
  #   envs = envs,
  #   bgExt = biogExt)
  
  maxPoints <- raster::ncell(biogMask) - raster::freq(biogMask, 
                                                       value = NA)[[1]]
  # Sample background points from the provided area
  biogSample <- penvs_bgSample(
    occs = occs,
    bgMask =  biogMask,
    bgPtsNum = maxPoints - (ceiling(0.01*maxPoints))) # taking off 1 percent just to be safe
  
  # Extract values of environmental layers for each background point
  biogEnvVals <- as.data.frame(raster::extract(biogMask,  biogSample))
  
  ##Add extracted values to background points table
  biogEnvVals <- cbind(scientific_name = paste0("bg_", specSpp), biogSample,
                         occID = NA, year = NA, institution_code = NA, country = NA,
                         state_province = NA, locality = NA, elevation = NA,
                         record_type = NA, biogEnvVals)
  
  # R code to get partitioned data
  groups <- part_partitionOccs(
    occs = occs ,
    bg =  biogSample, 
    method = "block",
    bgMask = biogMask,
    aggFact = 2) 
  
  # Run maxent model for the selected species
  modOut <- model_maxent(
    occs = occs,
    bg = biogEnvVals,
    user.grp = groups, 
    bgMsk = biogMask,
    rms = c(1, 2), 
    rmsStep =  1,
    fcs = 'L',
    clampSel = TRUE,
    algMaxent = "maxnet",
    parallel = TRUE,
    numCores = 4)
  
  print("Made it to model")
  
  # Select current model and obtain raster prediction
  m_Ab <- modOut@models[["fc.L_rm.2"]] 
  predSel_Ab <- predictMaxnet(m_Ab, biogMask,
                              type = "cloglog", 
                              clamp = TRUE)
  # extract the suitability values for all occurrences
  occs_xy <- occs[c('longitude', 'latitude')]

  ## If you want 0 or 1 - this code needs to be included ----
  # # determine the threshold based on the current prediction
  # occPredVals_Ab <- raster::extract(predSel_Ab, occs_xy)
  # # Define probability of quantile based on selected threshold
  # thresProb_Ab <- switch("p10", 
  #                        "mtp" = 0, "p10" = 0.1, "qtp" = 0)
  # # Define threshold value
  # thres_Ab <- stats::quantile(occPredVals_Ab, probs = thresProb_Ab)
  # # Applied selected threshold
  # predSel_Ab <- predSel_Ab > thres_Ab
  
  
  ## OTHERWISE ----
  
  # Get values of prediction
  mapPredVals_Ab <- getRasterVals(predSel_Ab, "cloglog")
  
  raster::plot(predSel_Ab)
  mapPredVals_Ab <- getRasterVals(predSel_Ab, "raw")
  # Define colors and legend  
  rasCols <- c("#2c7bb6", "#abd9e9", "#ffffbf", "#fdae61", "#d7191c")
  legendPal <- colorNumeric(rev(rasCols), mapPredVals_Ab, na.color = 'transparent')
  rasPal <- c('gray', 'blue')
  
  maxentEvalPlot_Ab <- ENMeval::evalplot.stats(
    modOut,
    "auc.val",
    "rm",
    "fc")
  #plot
 # print(maxentEvalPlot_Ab)
  
  # n <- mxNonzeroCoefs(modOut@models[["fc.L_rm.2"]], "maxnet")
  
  # Create response curves
 # print( for (i in n) {
 #    maxnet::response.plot(
 #      modOut@models[["fc.L_rm.2"]],
 #      v = i,
 #      type = "cloglog")
 #    
 # }
 # )
  
  m <- leaflet() %>% addProviderTiles(providers$Esri.WorldTopoMap) 
  
  m  %>%
    leaflet::addLegend("bottomright", colors = c('gray', 'blue'),
                       title = paste0("Thresholded Suitability<br>(Training) - ", specSpp),
                       labels = c("predicted absence", "predicted presence"),
                       opacity = 1, layerId = "train") %>% 
    #add occurrence data
    addCircleMarkers(data = occs, lat = ~latitude, lng = ~longitude,
                     radius = 5, color = 'red', fill = TRUE, fillColor = "red",
                     fillOpacity = 0.2, weight = 2) %>% 
    ##Add model prediction
    addRasterImage(predSel_Ab, colors = rasPal, opacity = 0.9,
                   group = 'vis', layerId = 'mapPred', method = "ngb") %>%
    ##add background polygons
    addPolygons(data = biogExt, fill = FALSE,
                weight = 4, color = "blue", group = 'proj')

}  

 
