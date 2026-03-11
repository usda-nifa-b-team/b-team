library(sf)
library(tidyverse)
library(basemaps)
library(cowplot) # Needed for publication-quality ggplots
library(ggridges)
library(sjPlot)
library(readxl)
library(ggspatial)

# functions ----

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

makeVar <- function(df, varSplit){
  
  var.counts <- df %>% 
    select({{varSplit}}, Short.label.name) %>% 
    group_by({{varSplit}}) %>% 
    distinct() %>% 
    tally(name = "Species")
  
  var.props <- df %>% 
    group_by({{varSplit}}) %>% 
    tally(name = "Prop") %>% 
    mutate(prop.abund = Prop/nrow(b26))
  
  var <- left_join(var.counts, var.props, by = join_by({{varSplit}}))
  
  return(var)
}

colourMapping <- function(varDf, var, bombusTF){
  if(bombusTF == TRUE){
    color_mapping <- c("#E31A1C", "#FDBF6F", "green4")
    names(color_mapping) <- c("gyne", "worker", "drone")
  }else if(bombusTF == F){
    color_mapping <- c("dodgerblue2", "orchid1" )
    names(color_mapping) <- c("Male", "Female")
  }else{
    c24 <- c(
      "maroon", "#E31A1C", "green4", "#6A3D9A",
      "#FF7F00", "gold1", "mediumpurple1","skyblue2", "#FB9A99",
      "palegreen2", "#CAB2D6", "#FDBF6F", "gray70", "khaki2",
      "dodgerblue2", "orchid1", "deeppink1", "blue1", "steelblue4",
      "darkturquoise", "green1", "yellow4", "yellow3", "darkorange4"
    )
    varLevs <- levels(varDf %>% pull({{var}}))
    
    color_mapping <- setNames(c24, varLevs)
  } 
  return(color_mapping)
}

varRecords <- function(df, varSplit){
  
  genRec <- df %>% 
    group_by({{varSplit}}) %>% 
    tally() %>% #filter(n > 19) %>% 
    mutate(stars = case_when(n < 50 & n > 20 ~ "*",
                             n < 100 & n >= 50 ~ "**",
                             n < 1000 & n >= 100 ~ "***",
                             n >= 1000 ~ "****"))
  return(genRec)
}

# varRec <- varRecords(b26, caste) # formerly genus_records

datRidges <- function(df, varSplit, varRec = varRec){
  dat_ridges <- df %>% 
    semi_join(varRec, by = join_by({{varSplit}})) %>% 
    mutate(week = week(Date))
  return(dat_ridges)
}

# dat_ridges <- datRidges(b26, caste)

varOrders <- function(df = dat_ridges, varSplit, orderLev = c("female", "male"), bombusTF = T){
  
  if(bombusTF == T){
    var_orders <- df %>% 
      select({{varSplit}}, week) %>% 
      group_by({{varSplit}}) %>% 
      summarise(peak = Mode(week)) %>% 
      mutate(orderLevel = factor(caste, levels = c("gyne", "worker", "drone"))) %>% 
      data.frame()
  }  else {
    var_orders <- df %>% 
      select({{varSplit}}, week) %>% 
      group_by({{varSplit}}) %>% 
      summarise(peak = Mode(week)) %>%
      mutate(orderLevel = factor({{varSplit}}, levels = orderLev)) %>% 
      arrange(desc(peak)) %>% 
      data.frame()
  }  
  return(var_orders)
}
levOrder <- function(var_orders, varSplit){
  var_orders <- var_orders %>% mutate(orderLev = as.factor({{varSplit}}))
  level_order <- levels(var_orders %>% pull(orderLev))
  return(level_order)
}
ridgePlot1 <- function(df, varSplit, bombusTF = T){
  
  varMade <- makeVar(df, varSplit = {{varSplit}})
  color_mapping <- colourMapping(varDf = varMade, var = {{varSplit}}, bombusTF = bombusTF ) # using set colour scheme
  varRec2 <- varRecords(df, {{varSplit}})
  dat_ridges <- datRidges(df, varRec = varRec2, {{varSplit}})
  var_orders <- varOrders(dat_ridges, varSplit = {{varSplit}}, bombus = bombusTF) # using set order
  level_order <- levOrder(var_orders = var_orders, varSplit = {{varSplit}})
  
  bombSp <-pull(dat_ridges %>% 
                  group_by(genSpp) %>% summarise())
  
  ploti <- dat_ridges %>% select(week, everything()) %>% 
    ggplot(aes(x = week,
               y = factor({{varSplit}}, level = level_order),
               fill = {{varSplit}})) + # varSplit
    # facet_wrap(~{{varSplit}})+
    geom_density_ridges(aes(height = stat(density)),
                        scale = 5,
                        rel_min_height = 0.01,
                        stat = "density",
                        bw = "bcv", alpha = 0.75) +
    theme_ridges(grid = F) +
    scale_fill_manual(values = color_mapping) +
    theme(legend.position = "none")+ #+
    labs(x = "", y = "") +
    geom_vline(xintercept = 0, color = "white", linetype = "dashed", alpha = 0)+
    geom_vline(xintercept = 12.6,
               color = "grey50", linetype = "dashed") +
    geom_vline(xintercept = 25.3,
               color = "grey50", linetype = "dashed") +
    geom_vline(xintercept = 38.4,
               color = "grey50", linetype = "dashed") +
    geom_text(data = varRec2, aes(x = 55, y = {{varSplit}},
                                  label = n), size = 3)+
    annotate(geom = "text", x = 9, y = 9.5, label = "Winter", # x values are set because they depend on week of year
             size = 4) +
    annotate(geom = "text", x = 18, y = 9.5, label = "Spring",
             size = 4) +
    annotate(geom = "text", x = 32, y = 9.5, label = "Summer",
             size = 4) +
    annotate(geom = "text", x = 41, y = 9.5, label = "Fall",
             size = 4)+
    annotate(geom = "text", x = 52/2, y = 10.5, label = bombSp, vjust = 1.5, fontface = "italic")+
    scale_x_continuous(breaks = c(-20, -10, 0, 10, 20, 30, 40, 50),
                       labels = c("","", "1 January", "6 March", "15 May",
                                  "24 July", "3 October", "12 December"))
  
  return(ploti)
}

#ridgePlot1(df = b26 %>% filter(genSpp %in% "Halictus tripartitus"), bombusTF = F,  varSplit = sex)

# Data import ----

itis <- read_csv("Robinson/Data/ITIS_global_bee_taxonomy_lookup_7_28_2025.csv") %>% 
  dplyr::group_by(genus, family) %>% summarise() %>% ungroup() %>% 
  mutate(family = as.factor(family))

load('Robinson/Data/cleanedV3.Rdata')

dat <- dat %>% mutate(sex = str_to_title(sex))

b26_int <- dat %>% 
  filter(state %in% "OR") %>% 
  filter(!is.na(genSpp))

b26_int2 <- b26_int %>% st_drop_geometry() %>% 
  group_by(country,          state,    county,           locality ,  family,           genus,      genSpp,  caste, sex,            plantGenSpp,   samplingProtocol, year,             month,           
           day,              verbatimEventDate) %>% 
  count() %>% 
  mutate(n = case_match(samplingProtocol, 
                        "aerial net"~ n, 
                        "vane trap" ~ 1, 
                        .default = n)) %>% 
  arrange(desc(n)) %>% 
  ungroup() %>% 
  rename(Count = n)

b26 <- b26_int2 %>% 
  mutate(caste = case_when(
    caste%in% "worker" & genSpp %in%c("Bombus insularis", "Bombus flavidus") ~ "gyne",# fixing some workers which don't exist for Psithyrus
    .default = caste ) ) %>% 
  filter(!genSpp %in% "Bombus impatiens") %>%  # only one record, makes a blank figure
  uncount(Count) %>% 
  mutate(across(where(is.numeric), as.factor), # convert to factors
         date=paste(year,month,day,sep='-'), # no missing year month dates in this data
         date=as.Date(date,format='%Y-%m-%d'), 
         Date = date, # make date from individ columns - note done this way to allow for use of full OBA data in future (already has "date") 
         Year = year(Date), # Create year column
         Month = month(Date), # create month column
         JD = yday(Date), # julian date 
         ToY = case_when(JD < 135 ~ "early", # used their time of year defs
                         JD > 205 ~ "late",
                         TRUE ~ "mid"),
         StationYear = paste(county, Year, sep = "_")) %>% 
  mutate( Short.label.name = genSpp) %>% 
  filter(!is.na(caste)|!is.na(sex))

#debugonce(ridgePlot1)

# grp 1 - bombus with enough obs

# %>% filter(nd<10)

ridge_bombus <- function(df){
  sppx <- unique(df$genSpp)
  
  op_r <- ridgePlot1(df, bombusTF = T, varSplit = caste)
  
  ggsave(plot = op_r, path = "Robinson/phenPlots/", filename = str_c(sppx, "_phenology.pdf"), dpi = 320 , paper = "special", width = 11, height = 8.5 )
}

bombusOpts <- b26 %>% 
  filter(!str_detect(genSpp, "spp\\.$")) %>% 
  group_by(genSpp, caste) %>% 
  summarise(nd = n_distinct(date), n = n()) %>% 
  filter(str_detect(genSpp, "Bombus")) %>% 
  filter(!is.na(caste)) %>% 
  arrange(n) %>% 
  filter(!(genSpp%in% "Bombus morrisoni" & caste %in% "gyne"))

b26 %>% semi_join(bombusOpts) %>%ungroup() %>% 
  filter(!is.na(caste)) %>% 
  split(~genSpp) %>% 
  map(\(x) ridge_bombus(x))

#debugonce(ridgePlot1)
#(oP_r <- ridgePlot1(df = b26 %>% filter(genSpp %in% "Bombus morrisoni") %>% filter(!is.na(caste)), bombusTF = T,  varSplit = caste))



# grp 2 - non bombus with enough obs

# b26 %>% 
#   filter(!str_detect(genSpp, "spp\\.$")) %>% 
#   group_by(genSpp) %>% 
#   summarise(nd = n_distinct(date))

ridge_non <- function(df){
  sppx <- unique(df$genSpp)
  
  op_r <- ridgePlot1(df, bombusTF = F, varSplit = sex)
  
  ggsave(plot = op_r, path = "Robinson/phenPlots/", filename = str_c(sppx, "_phenology.pdf"), dpi = 320 , paper = "special", width = 11, height = 8.5 )
}

nonBombusOpts <- b26 %>% 
  filter(!str_detect(genSpp, "spp\\.$")) %>% 
  group_by(genSpp, sex) %>% 
  summarise(nd = n_distinct(date), n = n()) %>% 
  ungroup() %>% 
  filter(!str_detect(genSpp, "Bombus")) %>% 
  filter(!is.na(sex)) %>% 
  arrange(n) %>% 
  filter(nd>3) #%>% # seems to struggle below this number of unique dates
  #filter(str_detect(genSpp, "^A", negate = TRUE))  
  #%>% filter(row_number() < 5)# %>% 
  # filter(!(genSpp%in% "Bombus morrisoni" & caste %in% "gyne"))

#debugonce(ridgePlot1)

b26 %>% semi_join(nonBombusOpts) %>%ungroup() %>% 
  filter(!is.na(sex)) %>% 
  split(~genSpp) %>% 
  map(\(x) ridge_non(x))

# grp 3 - non bombus with not enough obs - not doing for now

#ggsave(plot = oP_r,  str_c("Robinson/", spp2plot, "_phenology.pdf"), dpi = 320 , paper = "special", width = 11, height = 8.5 )

# map of range ----
df2map <- dat %>% 
  filter(state %in% "OR") %>% 
  st_transform(crs = 3857)

orExt <- orEcoReg %>% 
  summarise() %>% 
  st_transform(crs = 3857)

# spp2plot <- "Halictus tripartitus"
# df2plot <- df2map %>% 
#   filter(genSpp %in% spp2plot )
# 
# mapTitle <- spp2plot
# #basemap_plot(ext = orExt, )
# #rstr <- basemap_raster(ext = orExt, map_service = "esri", map_type = "world_physical_map", map_res = 1)
# #or_base <- terra::rast("Robinson/Shapefiles/or_raster.tif")
# #or_t <- terra::as.data.frame(or_base, xy = TRUE)
# (oP <- ggplot()+
#   # terrainr::geom_spatial_rgb(data = or_t, 
#   #                            aes(x = x,
#   #                            y = y,
#   #                            r = or_raster_1,
#   #                            g = or_raster_2,
#   #                            b = or_raster_3))+
#   #geom_raster(data = or_t, aes(x = x, y = y, fill = value ))+
# basemap_gglayer(ext = orExt, map_service = "esri", map_type = "usa_topo_maps", map_res = 1 )+
#   #gg_raster(r = rstr)+
#   #basemap_gglayer(ext = orExt, map_service = "osm", map_type = "streets")+
#   scale_fill_identity() + 
#   coord_sf()+
#   geom_sf(data = df2plot, color = "red", size = 1.5)+
#   geom_sf(data = orEcoReg, alpha = 0.25)+
#   labs(title = mapTitle, y = "", x = "", color = "")+
#   theme_classic()+
#     theme_void()+
#   theme(legend.position = "", plot.title = element_text(size = 15, face = "italic", hjust = 0.5))+
#   ggspatial::annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm") )+
#   ggspatial::annotation_scale(location = "br", pad_y = unit(0.1, "cm"))
# #annotate(geom = "text", x = 52/2, y = 10.5, label = bombSp, vjust = 1, fontface = "italic")+
# )
# 
# ggsave(plot = oP, filename = str_c("Robinson/", spp2plot, "_map.pdf"), dpi = 320 , paper = "special", width = 11, height = 8.5 )

#get_maptypes()  

# rmarkdown::render(
#   input = "Robinson/ridgeParent.Rmd",
#   output_file = "testMapnRidge.pdf",
# )

# mapping ----

mapFxn <- function(df){
  
  mapTitle_sppx <- unique(df$genSpp)
  
  g <- ggplot()+
    basemap_gglayer(ext = orExt, map_service = "esri", map_type = "usa_topo_maps", map_res = 1 )+
    scale_fill_identity() + 
    coord_sf()+
    geom_sf(data = df, color = "red", size = 1.6)+
    geom_sf(data = orEcoReg, alpha = 0.25)+
    labs(title = mapTitle_sppx, y = "", x = "", color = "")+
    theme_classic()+
    theme_void()+
    theme(legend.position = "", plot.title = element_text(size = 15, face = "italic", hjust = 0.5))+
    ggspatial::annotation_north_arrow(location = "tr", height = unit(1, "cm"), width = unit(1, "cm") )+
    ggspatial::annotation_scale(location = "br", pad_y = unit(0.1, "cm"))
  
  ggsave(plot = g, path = "Robinson/mapPlots/", filename = str_c(mapTitle_sppx, "_map.pdf"), dpi = 320 , paper = "special", width = 11, height = 8.5 )
  }

df2map <- dat %>% 
  filter(state %in% "OR") %>% 
  st_transform(crs = 3857)

# testSpp <- df2map %>% st_drop_geometry() %>% 
#   filter(!str_detect(genSpp, "spp.$")) %>% 
#   sample_n(size = 5) %>% group_by(genSpp) %>% 
#   count()
# testSpp
  
df2map %>% 
  filter(!str_detect(genSpp, "spp.$")) %>% 
  split(~genSpp) %>% 
  map(\(x) mapFxn(x))

  

