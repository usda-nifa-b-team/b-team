# Species summaries 
# Nov 2025

# goals: 

# maxent map - see to do in maxEntTesting.R - elev chart
# hosts - floral summary - disc with 2 or 3 rings (middle family, second genera, species third level), % of each fam, gen, spp
# some basic summary stats 
# specialization? 

# remotes::install_github("showteeth/ggpie")
remotes::install_github("cardiomoon/webr")
library(ggpie)

ggpie(data = diamonds, group_key = "cut", count_type = "full",
      label_info = c("count", "ratio"), label_type = "horizon", label_split = NULL,
      label_size = 4, label_pos = "in")

bCalig %>% st_drop_geometry() %>% arrange(familyPlant) %>%
  filter(!is.na(genusPlant)) %>% 
  #group_by(familyPlant) %>% 
#  count() %>% 
ggnestedpie(., group_key = c("familyPlant", "genusPlant"), 
            count_type = "full")
library(webr)
