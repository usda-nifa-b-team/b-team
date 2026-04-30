# Render script for V3 Student Reports

# This version is for Myles to test running this on the server
# The full version has two "pathways", one for observers that have observations in a year, one for those with 0
# This version includes only the "pathway" for the first 10 observers with some observations in 2023, in the state of OR

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

load('Robinson/Data/cleanedV3.Rdata')

# Setup - State/Year Combos ----
# function to set up df to render diff prov/year combos 
renderAnything <- function(stProv, yr2make, repFor, subFold){
  load('Robinson/Data/cleanedV3.Rdata') # or load('Data/cleaned.Rdata')
  
  dat <- dat %>% filter(state %in% stProv)
  repFor <- str_replace(repFor, "\\|", "and") 
  # mutate(output_file = str_replace(output_file, "\\|", "and")) # necessary step for weird names
  rmarkdown::render(
    input = "Robinson/templateSheetV3_TESTING.Rmd", # could replace this with function input too
    output_file = str_c("reports_", yr2make, "/", stProv, "/", subFold, "/", str_replace_all(repFor, " ", "_"),"_",stProv, "_", yr2make,"_", "Summary.pdf"),
    params = list(collectorName = repFor,
                  state = stProv, 
                  year = yr2make) 
  )
}

# 2024 Examples of making reports for a state ----

datWA <- dat %>% # get only the data for the state of interest
  filter(state %in% "WA")

# find all collectors in state - should work for all even if no collections in year of interest
wa24 <-
  datWA %>% st_drop_geometry() %>%
  group_by(collector, state) %>%
  summarise(n = n_distinct(genSpp, na.rm = T)) %>% ungroup() %>%
  # filter(n>0) %>%
  rename(stProv = state, repFor = collector) %>%
  mutate(yr2make = 2024,
         subFold = "all") %>% # need to have a subfolder in working directory titled this
  select(!n)

purrr::pwalk(wa24, renderAnything) # WA collectors

# files are set up to write to a folder called reports_yr2make/stProv/subFold, 
# with filenames = repFor_stProv_yr2make_Summary.pdf

# %>% filter(row_number()%in% c(1,2)) # can use this to filter wa24 to only include a couple reports for quicker testing

# repeat as necessary for other states
  # get data only for state
  # confirm year is as desired (reports will focus on collections in the year specified, but some sections do include collections from all years)
  # create data with collector info
  # run purrr::pwalk on data 