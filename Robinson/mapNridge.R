library(sf)
library(tidyverse)
library(basemaps)

load('Robinson/Data/cleanedV3.Rdata')

df2map <- dat %>% 
  filter(genSpp %in% "Bombus occidentalis") %>% 
  filter(state %in% "OR") %>% 
  st_transform(crs = 3857)

ridgePlot1(df = b26 %>% filter(genSpp %in% "Bombus occidentalis"), varSplit = caste)

mapTitle <- unique(df2map$genSpp)

orExt <- orEcoReg %>% 
  summarise() %>% 
  st_transform(crs = 3857)

#basemap_plot(ext = orExt, )
#rstr <- basemap_raster(ext = orExt, map_service = "esri", map_type = "world_physical_map", map_res = 1)
or_base <- terra::rast("Robinson/Shapefiles/or_raster.tif")
or_t <- terra::as.data.frame(or_base, xy = TRUE)
ggplot()+
  terrainr::geom_spatial_rgb(data = or_t, 
                             aes(x = x,
                             y = y,
                             r = or_raster_1,
                             g = or_raster_2,
                             b = or_raster_3))+
  #geom_raster(data = or_t, aes(x = x, y = y, fill = value ))+
#basemap_gglayer(ext = orExt, map_service = "esri", map_type = "usa_topo_maps", map_res = 1 )+
  #gg_raster(r = rstr)+
  #basemap_gglayer(ext = orExt, map_service = "osm", map_type = "streets")+
  #scale_fill_identity() + 
  coord_sf()+
theme(legend.position = "")+
  geom_sf(data = df2map, color = "red")+
  geom_sf(data = orEcoReg, aes(fill = name),  alpha = 0.25)+
  labs(title = mapTitle)+
  theme_classic()


#get_maptypes()  
