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

# Some 2023 ---------------------------------------------------------------

some2023 <- caught23 %>%
  filter(nCaught >= 1) %>% 
  dplyr::select(collector)

some23dat <- dat %>% semi_join(some2023) # some 23 obs 
volNamesSome23 <- tibble(names=unique(some23dat$collector)) # name options some 23

reportsSome23 <- tibble( input = "Robinson/templateSheetV3_TESTING.Rmd", # needs to be run in a way that the rmd is located at this relative path
  collec = tibble(names=unique(some23dat$collector)),
  output_file = stringr::str_c(str_replace_all(collec$names, " ", "_"), "_Summary2023.pdf"), # writes to whatever directory the files are in
  params = purrr::map(collec$names, ~ list(collectorName = ., year = "2023", state = "OR"))) %>% 
  head(n = 10) # keep first 10 observers

reportsSome23 %>% dplyr::select(!collec) %>% pwalk(.f = rmarkdown::render) # make pdf reports

