# State Summaries ----
# Nov 2025

# goals: 

  # statewide richness 

  # identify areas of high richness - chao estimator
  
  # identify areas that would benefit from greater sampling - chao estimator

library(sf)
library(knitr)
library(kableExtra)
library(ggpubr)
library(RColorBrewer)
library(ggspatial) #library(ggsn) replaced
library(shadowtext)
library(tinytex)
library(tidyverse)
library(dplyr)
library(iNEXT) # check name/install

load('Robinson/Data/cleanedV3.Rdata') # or load('Data/cleaned.Rdata')

datSt <- dat %>% 
  filter(state %in% "WA")

# datSt %>% 
#   group_by(genSpp)
## Statewide Richness ----
# convert to matrix for inext - county level?

datMat <- datSt %>% st_drop_geometry() %>% 
  select(county, genSpp) %>%
  filter(!is.na(genSpp)) %>% 
  group_by(county, genSpp) %>% 
  count() %>% 
  pivot_wider(values_from = n, names_from = genSpp, values_fill = 0) %>% 
  column_to_rownames(var = "county")

chaoDat <- chao(datMat)

countyRichEst <- usCountiesW %>% filter(STUSPS %in% "WA") %>% 
  left_join(chaoDat, by = c("county" = altName))

# plot for actual
ggplot(countyRichEst)+
  geom_sf(aes(fill = chaoActual))+
  geom_shadowtext() # add number actual 

# version for estimated
ggplot(countyRichEst)+
  geom_sf(aes(fill = estRichness))+
  geom_shadowtext() # add number estimated 

## turnt into function ---- 

# OR use params for state? Simplifies function, allows multiple plots
stateSummaries <- function(df, state2do = "WA"){
}


# more advanced would be tiling the state but that's a lot more complex - maybe could use spp. models in future
