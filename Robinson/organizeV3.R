#ORGANIZE DATA FROM A. MELATHOPOLOUS AND MAKE FUNCTIONS
#WRITTEN BY SAMUEL ROBINSON, FALL 2021
#UPDATED 2025 BY ROWAN RAMPTON TO WORK WITH NEW STANDARDIZED DARWIN CORE OUTPUT
#V3 - adds shapefiles from other locations

## STRUCTURE:
# 1. Load and clean raw data, and make helper functions (organize.R)
# 2. Create template sheet that has the basic structure of the document needed for volunteers (templateSheet.Rmd)
# 3. Run template sheet for each volunteer, each making a separate PDF (makeReports.R)

#Load packages -------------------------
library(tidyverse)
library(sf)

#Functions -----------------

#Whitespace test - returns TRUE if leading or trailing whitespace found
wSpaceTest <- function(x){
  a1 <- sort(unique(x))
  a2 <- gsub('(^\\s|\\s$)','',sort(unique(x)))
  any(a1!=a2)
}

#Creates genus/species column, accounting for empty species labels
makeGenSpp <- function(data,genCol,sppCol,newCol=genSpp) {
  require(dplyr); require(tidyr)
  data %>%
    mutate({{newCol}}:=case_when(
      !is.na({{genCol}}) & !is.na({{sppCol}}) ~ paste({{genCol}},{{sppCol}},sep=' '),
      !is.na({{genCol}}) & is.na({{sppCol}}) ~ paste0({{genCol}},' spp.'),
      TRUE ~ NA_character_)
    )
}

#Function to make abundance plots from data frame
abundPlots <- function(d,family=family,genus=genus,genSpp=genSpp,colourSet='Set1',scaleYtext=c(1,1,1),keepSpp=TRUE){
  require(RColorBrewer)
  require(dplyr); require(tidyr)
  require(ggpubr)
  options(dplyr.summarise.inform=FALSE)
  
  if('sf' %in% class(d)) d <- d %>% sf::st_drop_geometry() #Drop geometry if sf object
  
  #Colours for individual families
  famCols <- data.frame(
    family = c('Andrenidae','Apidae','Colletidae','Halictidae','Megachilidae', 'Melittidae'), # added Melittidae as non grey
    cols=brewer.pal(6,colourSet)) #Colour scheme for families
  
  #Species abundance plot
  
  #Data for histograms
  plotDat <- d %>% filter(!grepl('spp\\.$',{{genSpp}}) | keepSpp) %>%  #Filter out "spp" unless keepSpp==TRUE
    count({{family}},{{genSpp}}) %>% #Count family and genSpp occurrences
    arrange(desc({{family}}),n) %>% ungroup() %>% #Arrange by family
    mutate({{genSpp}}:=factor({{genSpp}},level={{genSpp}}))
  
  #Data for coloured background rectangles
  rectDat <- d %>% filter(!grepl('spp\\.$',{{genSpp}}) | keepSpp) %>%
    count({{family}},{{genSpp}}) %>%  #Count family and genSpp occurrences
    group_by({{family}}) %>% summarize(nSpp=n()) %>% ungroup() %>%
    arrange(desc({{family}})) %>% #Arrange by family
    mutate(ymax=cumsum(nSpp)+0.5,ymin=lag(ymax,default=0.5)) %>%
    rowwise() %>% mutate(ymid=mean(c(ymax,ymin))) %>%
    mutate(xmin=0,xmax=max(plotDat$n)) %>%
    left_join(famCols,by=deparse(substitute(family)))
  
  nSppHeight <- rectDat$nSpp
  
  #Species plot
  titleText <- paste0('Species (',nrow(plotDat),' total)')
  sppPlot <- ggplot()+ geom_col(data=plotDat,aes(n,{{genSpp}}))+ #Make columns
    geom_rect(data=rectDat, #Make background rectangles
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill={{family}}),
              alpha=0.3,show.legend = FALSE)+
    geom_col(data=plotDat,aes(n,{{genSpp}},fill={{family}}),show.legend = FALSE)+ #Columns (again)
    geom_text(data=rectDat,aes(x=xmax*0.9,y=ymid,label={{family}}),hjust=1)+ #Add text
    theme(axis.text.y=element_text(vjust=0.5,size=8*scaleYtext[1]))+ #Change theme
    labs(y=NULL,x='Number of specimens',title=titleText)+
    scale_fill_manual(values=rev(as.character(rectDat$cols)))+
    coord_cartesian(ylim = c(-1, n_distinct(plotDat$genSpp)))
  
  #Data for genus abundance plots
  plotDat <- d %>%  #Data for histograms
    count({{family}},{{genus}}) %>% #Count family and genus occurrences
    arrange(desc({{family}}),n) %>% ungroup() %>%
    mutate({{genus}}:=factor({{genus}},level={{genus}})) #Re-order genus
  
  #Data for background rectangles
  rectDat <- d %>%
    group_by({{family}},{{genus}}) %>% summarize(n=n()) %>%
    group_by({{family}}) %>% summarize(nSpp=n()) %>% ungroup() %>%
    arrange(desc({{family}})) %>%
    mutate(ymax=cumsum(nSpp)+0.5,ymin=lag(ymax,default=0.5)) %>%
    rowwise() %>% mutate(ymid=mean(c(ymax,ymin))) %>%
    mutate(xmin=0,xmax=max(plotDat$n)) %>%
    left_join(famCols,by=deparse(substitute(family)))
  
  #Genus plot
  titleText <- paste0('Genera (',nrow(plotDat),' total)')
  genPlot <- ggplot()+ geom_col(data=plotDat,aes(n,{{genus}}))+
    geom_rect(data=rectDat,
              aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill={{family}}),
              alpha=0.3,show.legend = FALSE)+
    geom_col(data=plotDat,aes(n,{{genus}},fill={{family}}),show.legend = FALSE)+
    geom_text(data=rectDat,aes(x=xmax*0.9,y=ymid,label={{family}}),hjust=1)+
    theme(axis.text.y=element_text(vjust=0.5,size=8*scaleYtext[2]))+
    labs(y=NULL,x='Number of specimens',title=titleText)+
    scale_fill_manual(values=rev(as.character(rectDat$cols)))+
    coord_cartesian(ylim = c(-1, n_distinct(plotDat$genus)))
  
  #Family abundance plots
  plotDat <- d %>%  #Data for histograms
    group_by({{family}}) %>% summarize(n=n()) %>% ungroup() %>%
    arrange(desc({{family}}),n) %>%
    left_join(famCols,by=deparse(substitute(family))) %>%
    mutate({{family}}:=factor({{family}},level={{family}}))
  
  #Make family plot
  titleText <- paste0('Families (',nrow(plotDat),' total)')
  famPlot <- ggplot()+ geom_col(data=plotDat,aes(n,{{family}}))+ #Make plot
    geom_col(data=plotDat,aes(n,{{family}},fill={{family}}),show.legend = FALSE)+
    theme(axis.text.y=element_text(vjust=0.5,size=8*scaleYtext[3]))+
    labs(y=NULL,x='Number of specimens',title=titleText)+
    scale_fill_manual(values=as.character(plotDat$cols))
    
  #Put all plots together into a single plot
  a <- ggarrange(sppPlot,ggarrange(genPlot,famPlot,nrow=2),ncol=2)
  return(a)
}

# # Test abundPlots
# debugonce(abundPlots)
# abundPlots(filter(dat,collector=="Pam Arion"),family,genus,genSpp,scaleYtext = c(1,0.75,1.25),keepSpp=FALSE)
# abundPlots(dat,family=family,genus=genus,genSpp=genSpp,scaleYtext = c(0.5,0.75,1.25),keepSpp=FALSE)

#Function to make abundance kable table, split up by some other variable (e.g. Site)
abundTable <- function(d,col1,col2,totals=FALSE){
  require(tidyverse); require(knitr); require(kableExtra)
  if(missing(col1)|missing(col2)) stop('Specify columns to use')
  
  col2 <- enquo(col2) #Defuses arguments
  
  if(totals){
    #Marginal column/row totals
    d <- d %>% count({{col1}},{{col2}}) %>%
      pivot_wider(names_from={{col1}},values_from=n,values_fill=0) %>%
      arrange({{col2}}) %>%
      rowwise() %>% mutate(TOTAL=sum(c_across(-{{col2}}))) %>%
      #Hacky way around dealing with `summarize(across(-col2,sum))` (can't be used within bind_rows)
      column_to_rownames(var=as_label(col2)) %>%
      bind_rows(.,colSums(.)) %>% rownames_to_column(var=as_label(col2)) %>%
      mutate({{col2}} := c({{col2}}[-n()],'TOTAL')) %>%
      column_to_rownames(var=as_label(col2))
  } else {
    #Version without marginal totals
    d <- d %>% count({{col1}},{{col2}}) %>%
      pivot_wider(names_from={{col1}},values_from=n,values_fill=0) %>%
      arrange({{col2}}) %>%
      column_to_rownames(var = as_label(col2))
  }
  return(d)
}

# #Test abundTable
# set.seed(123)
# (d <- data.frame(c1=sample(letters,300,TRUE),c2=sample(letters,300,TRUE)))
# abundTable(d,c2,c1,FALSE)
# abundTable(d,c1,c2,TRUE)

#Function to make a "wide kable table", with column names replicated across the page
wideTable <- function(d,countBy={{countBy}},countNum={{countNum}},nsplit=NA,colNames=c("countBy","count"),
                      caption='',
                      returnDF=FALSE,...){
  require(tidyverse)
  require(knitr)
  library(kableExtra)
  if(is.na(nsplit)) stop('Specify number of splits to perform')
  if('sf' %in% class(d)) d <- d %>% sf::st_drop_geometry() #Drop geometry if sf object
  
  d <- d %>%
    # #Get counts in each category - moved outside of function
    # filter(!is.na({{countBy}})) %>% count({{countBy}}) %>%
    # arrange(n) %>%
    mutate({{countBy}}:=factor({{countBy}},levels={{countBy}})) %>%
    #Splits into nsplit sets of columns
    mutate(ord=cut(rev(rank(as.numeric({{countBy}}))),nsplit,labels=1:nsplit)) %>%
    arrange(ord,desc({{countNum}})) %>% mutate(across(c({{countBy}},{{countNum}}),as.character)) %>%
    group_by(ord) %>% mutate(id=1:n()) %>% ungroup() %>%
    pivot_longer(cols={{countBy}}:{{countNum}}) %>%
    unite(name,name,ord,sep="_") %>% pivot_wider(id_cols=id,names_from=name,values_from=value) %>%
    select(-id) %>% mutate(across(everything(),~ifelse(is.na(.)," ",.)))
  
  if(returnDF) return(d) #Returns dataframe
  
  d %>% kable(col.names=rep(colNames,nsplit),align=rep(c("r","l"),nsplit),
              caption=caption) %>%
    column_spec(column=seq(1,nsplit*2,2),italic=TRUE) %>% #Genus names
    kable_styling(latex_options = c("striped"),...)
}

#Load and organize data ---------------------------

# round 1 - import 
dat1 <- read_csv("Robinson/Data/OBA_2018-2024_all_records.csv") %>% 
  rename_with(.cols = everything(), .fn = make.names) %>% 
  select(
         recordedBy, 
         day, month, year, verbatimEventDate, # date = verbatimEventDate - make own date anyway in next round
         country, stateProvince, county, locality, # location names 
         decimalLatitude, decimalLongitude, 
         contains('plant'), #plant columns
         contains('Det'), #Volunteer determinations
         family, genus, specificEpithet, identifiedBy, # actual determinations
         sex, caste, # new, maybe not needed?
         samplingProtocol # new needed to remove trapped records
    )  

# round 2 - rename 

dat <-  dat1 %>% 
  mutate(date=paste(year,month,day,sep='-')) %>% # note a couple missing months/days - traps or missing collection method
  mutate(date=as.Date(date,format='%Y-%m-%d')) %>% 
  rename(collector = recordedBy) %>% 
  # mutate(country=ifelse(country=='United States of America','USA',country), # shouldn't be necessary anymore
  #        stateProvince=ifelse(stateProvince=='Oregon','OR',stateProvince)) %>% 
  rename('state'='stateProvince','taxonomist'='identifiedBy') %>% 
  rename('species'='specificEpithet','lat'='decimalLatitude','lon'='decimalLongitude') %>%
  #rename_with(.fn=~paste0('plant',tools::toTitleCase(gsub('plant_','',.x,fixed=TRUE))),.cols=contains('plant_')) %>% #Rename plant columns - have changed, redone
  rename(plantGenSpp = speciesPlant)
  # rename_with(.fn=~gsub('Det..Volunteer...','volunteer',.x),.cols=contains('Volunteer')) - names changed, not needed

# dat <- dat2 %>% filter(!is.na(collector))  # currently 3 rows with missing collectors - fix in future?
  
if(any(is.na(dat$collector))){
  warning('Removed ',sum(is.na(dat$collector)),' specimens with no collector listed')
  print(dat %>% filter(is.na(collector)))
  dat <- dat %>% filter(!is.na(dat$collector))
}

vol <- dat$collector
vol <- gsub("; "," and ",vol,fixed=TRUE) #Semicolons
vol <- gsub("(\\&|\\/)"," and ",vol) #Ampersands and forward-slashes
vol <- gsub('(^\\s|\\s$)','',vol) #Leading/trailing spaces
vol <- gsub("  "," ",vol,fixed = TRUE) #Double spaces

uvol <- sort(unique(vol)) #Unique volunteers
cNames <- names(table(tolower(uvol))[table(tolower(uvol))>1]) #Names with capitalization problems
for(i in 1:length(cNames)){
  versions <- uvol[tolower(uvol) %in% cNames[i]] #Differently capitalized versions of the same name
  numCaps <- sapply(gregexpr("[A-Z]", versions, perl=TRUE),length) #Number of capitals per version
  repVersion <- versions[which.max(numCaps)] #Version to replace others with
  versions <- versions[!versions %in% repVersion] #Other versions
  warning(paste0('Replacing (', paste(versions,collapse=' + '),') with ',repVersion))
  for(j in versions) vol <- gsub(j,repVersion,vol,fixed = TRUE)
}

if(any(vol!=dat$collector)){
  warning('Replaced in collector names: ; & / double-spaces leading/trailing spaces.\nReplaced names with capitalization differences.\nCheck collector names:',immediate. = TRUE)
  for(i in unique(sort(vol))) print(i)
} 

dat$collector <- vol #Replace
rm(vol,uvol,cNames,versions,numCaps,repVersion,i,j) #Cleanup

#Plant columns

#Check plant genus column for multiple names
badNames <- dat %>%
  mutate(nonstandard=str_count(genusPlant,' ')>0 & is.na(plantGenSpp)) %>%
  filter(!is.na(nonstandard),nonstandard) %>% pull(genusPlant) %>%
  unique()

if(length(badNames)>0){
  warning('Multiple words found in plant genus column:\n\n',
          paste(badNames,collapse='\n'),'\n\n',
          'Splitting across genus/species columns')
  
  dat <- dat %>%
    mutate(plantGenSpp=ifelse(genusPlant%in%badNames,genusPlant,plantGenSpp)) %>%
    mutate(genusPlant=ifelse(genusPlant%in%badNames,sapply(str_split(dat$plantGenSpp,' '),first),genusPlant))
  rm(badNames)
}

#Checks plant species column
dat <- dat %>% #
  mutate(plantGenSpp=case_when(
    is.na(plantGenSpp) & !is.na(genusPlant) ~ paste0(genusPlant,' spp.'), #Adds Genus spp. when Genus available, but not species
    str_count(plantGenSpp,' ')==0 & !is.na(plantGenSpp) ~ paste0(plantGenSpp,' spp.'), #Adds spp. to genus-only plants
    TRUE ~ plantGenSpp
  ))

#Check for entries with varietal or hybrid marks, or with too many species names
badNames <- dat %>%
  mutate(nonstandard=str_detect(plantGenSpp,'( x | z )') | #Hybrid marks
           str_detect(plantGenSpp,' var\\..*') | #Varietal marks
           str_count(plantGenSpp,' ')>1 #Too many species names
  ) %>%
  filter(nonstandard) %>% pull(plantGenSpp) %>%
  unique()

if(length(badNames)>0){
  badNamesLoc <- which(dat$plantGenSpp %in% badNames)
  dat <- dat %>%
    mutate(plantGenSpp=str_replace_all(plantGenSpp,'( x | z | z | × )',' ')) %>% #Strips hybrid "x" marks
    mutate(plantGenSpp=str_replace_all(plantGenSpp,'\\svar\\..*','')) %>% #Strips varietal info
    mutate(plantGenSpp=sapply(str_split(plantGenSpp,' '),function(x) ifelse(length(x)==1,x[1],str_c(x[1:2],collapse=' '))))
  #Drops last plant name
  warning('Non-standard plant names found:\n\n',
          paste(badNames,collapse='\n'),'\n\n',
          'Plant names changed to:\n\n',
          paste(unique(dat$plantGenSpp[badNamesLoc]),collapse='\n')
  )
  rm(badNames,badNamesLoc)
}

#Insect columns

#Check for entries with varietal or hybrid marks, or with too many species names
badNames <-  dat %>%
  mutate(nonstandard=str_detect(species,'(nr\\.|aff\\.)') |
           str_detect(species,'grp$') |
           str_detect(species,'sp\\.') |
           str_count(species,' ')>0 |
           str_detect(species,'\\/')
  ) %>%
  filter(nonstandard) %>% pull(species) %>% unique()

if(length(badNames)>0){
  #Drops species name
  warning('Non-standard (taxonomist) bee species names found:\n\n',
          paste(badNames,collapse='\n'),'\n\n',
          'Changed to "spp."'
  )
  dat$species[dat$species %in% badNames] <- NA
}

badNames <- dat %>%
  mutate(nonstandard=str_detect(specificEpithetVolDet,'(nr\\.|aff\\.)') |
           str_detect(specificEpithetVolDet,'grp$') |
           str_detect(specificEpithetVolDet,'sp\\.') |
           str_count(specificEpithetVolDet,' ')>0 |
           str_detect(specificEpithetVolDet,'/')) %>%
  filter(nonstandard) %>% pull(specificEpithetVolDet) %>% unique()

if(length(badNames)>0){
  #Drops species name
  warning('Non-standard (volunteer) bee species names found:\n\n',
          paste(badNames,collapse='\n'),'\n\n',
          'Changed to "spp."'
  )
  dat$specificEpithetVolDet[dat$specificEpithetVolDet %in% badNames] <- NA
}

dat <- dat %>%
  makeGenSpp(genusVolDet,specificEpithetVolDet,volunteerGenSpp) %>% #Volunteer
  makeGenSpp(genus,species,genSpp) #Taxonomist

itis <- read_csv("Robinson/Data/ITIS_global_bee_taxonomy_lookup_7_28_2025.csv") %>% 
  dplyr::group_by(genus, family) %>% summarise() # family-genus table

dat <- dat %>% select(!family) %>% left_join(itis) # fill family column

if(!('family' %in% names(dat))){
  warning('Family column not found for final determination. Populating from genus table.')
  genFamTable <- read.csv('./Data/genFamTable.csv',stringsAsFactors = FALSE)
  dat <- dat %>% left_join(genFamTable,by='genus')
}

if(any(with(dat,is.na(familyVolDet) & !is.na(genusVolDet)))){
  warning('Volunteer genus recorded, but not family. Populating from genus table.')
  dat$familyVolDet <- NULL
  
  # genFamTable <- read.csv('./Data/genFamTable.csv',stringsAsFactors = FALSE) %>% # replaced with global taxonomy
  #   rename_with(.fn=~paste0('volunteer',tools::toTitleCase(.x)))
  
  itisVolFam <- itis %>% rename(genusVolDet = genus, familyVolDet = family)
  
  dat <- dat %>% 
    mutate(genusVolDet = str_to_title(str_squish(genusVolDet))) %>% 
    select(!familyVolDet) %>% 
    left_join(itisVolFam) # could fix a couple spelling errors for hyleaus and hapropoda, others are Epimelissodes and Ripiphorus- blister beetle?
  
  if(any(with(dat,is.na(familyVolDet) & !is.na(genusVolDet)))){ #Check again
    badNames <- dat %>% filter(is.na(familyVolDet) & !is.na(genusVolDet)) %>% pull(genusVolDet) %>% unique()
    warning('Genus not found in genus table:\n\n',
            paste(badNames,collapse='\n'),'\n\n',
            'Update genus table and re-run.'
    )
    
  }
}


# Adding spatial data  ----------------------------------------------------

dat <- dat %>%
  filter(!is.na(lat)&!is.na(lon)) %>% #Strips out missing locations
  st_as_sf(coords=c('lon','lat')) %>% #Set lon and lat as coordinates
  st_set_crs(4269) %>% #Set coordinate reference system (NAD83)
  st_transform(3643) #Transform to Oregon Lambert system

#Shapefiles of Oregon counties
orCounties <- st_read("Robinson/Shapefiles/orcntypoly.shp") %>% #Read in county polygons
  select(unitID:altName) %>% st_set_crs(4269) %>% #NAD83
  st_transform(3643) %>%  #Transform to Oregon Lambert
  mutate(nRecords=sapply(st_contains(.,dat),length), #Number of bee specimens per county
         nFlwGenera=sapply(st_contains(.,dat),function(x) length(unique(na.omit(dat$genusPlant[x]))))) #Number of flower genera

# adding western states
includedStates <- c("AZ", "CA", "CO", "ID", "MT", "NM", "NV", "OR", "UT", "WA", "WY" ) 
usStates <- st_read("Robinson/Shapefiles/cb_2018_us_state_500k/cb_2018_us_state_500k.shp") %>% 
  filter(STUSPS %in% includedStates)

stFP <- usStates %>% st_drop_geometry() %>% 
  mutate(stateName = NAME) %>% 
  select(STATEFP, STUSPS, stateName) # finding codes for filtering county shapefile

usCounties <- st_read("Robinson/Shapefiles/cb_2018_us_county_500k/cb_2018_us_county_500k.shp")

usCountiesW <- usCounties %>% 
  left_join(stFP, join_by(STATEFP)) %>% # join names of included states
  filter(!is.na(STUSPS)) %>% # remove non-included states
   mutate(unitID = COUNTYNS, 
          altName = NAME, 
          instName = paste0(altName," County")) %>% 
  select(unitID, 
         instName, 
         altName,
         stateName,
         STUSPS,
         geometry) %>% 
  st_transform(3643) %>%  #Transform to Oregon Lambert
  mutate(nRecords=sapply(st_contains(.,dat),length), #Number of bee specimens per county
         nFlwGenera=sapply(st_contains(.,dat),function(x) length(unique(na.omit(dat$genusPlant[x])))))

bcRegions <- st_read("Robinson/Shapefiles/bcPoly/ABMS_LGL_ADMIN_AREAS_SVW/ABMS_LAA_polygon.shp") %>%
  mutate(county = AA_TYPE) %>% 
  filter(AA_TYPE %in% c("Self-governing First Nations Area", "Regional District")) %>% # regional districts seem reasonable?
  mutate(unitID = AA_ID, # convert to same format as OR counties
         instName = AA_NAME, 
         altName = AA_NAME, 
         stateName = "British Columbia",
         STUSPS = "BC") %>% 
  select(unitID, instName, altName, stateName, STUSPS, geometry) %>% 
  st_transform(3643) %>%  #Transform to Oregon Lambert
  mutate(nRecords=sapply(st_contains(.,dat),length), #Number of bee specimens per county
         nFlwGenera=sapply(st_contains(.,dat),function(x) length(unique(na.omit(dat$genusPlant[x])))))

bcBorder <- st_read("Robinson/Shapefiles/bcProv/ABMS_PROVINCE_SP/ABMS_PROV_polygon.shp")

allCounty <- usCountiesW %>% rbind(bcRegions)

## basic checks for success
# ggplot()+
#   geom_sf(data = bcBorder)+
# geom_sf(data = bcRegions, colour = "red")+
#   geom_sf(data = usStates)+
#   geom_sf(data = usCountiesW, colour = "red")

#Shapefiles of Oregon ecoregions
orEcoReg <- st_read("Robinson/Shapefiles/or_eco_l3.shp") %>% transmute(name=NA_L3NAME) %>% #Read in ecoregions
  st_transform(3643) %>% #Transform to Oregon Lambert
  mutate(name=gsub(' and ',' & ',name)) %>% #Replace "and" with "&"
  mutate(name=gsub('Cascades Slopes','Cascades\nSlopes',name)) %>% #Add line break (\n) to "Cascades Slopes"
  mutate(nRecords=sapply(st_contains(.,dat),length), #Number of bee specimens per ecoregion
         nFlwGenera=sapply(st_contains(.,dat),function(x) length(unique(na.omit(dat$genusPlant[x]))))) #Number of flower genera

allEcoReg <- st_read("Robinson/Shapefiles/us_eco_l3_state_boundaries/us_eco_l3_state_boundaries.shp") %>% 
  rename(name = US_L3NAME) %>% 
  left_join(stFP, by = c("STATE_NAME" = "stateName")) %>% # only included states
  filter(!is.na(STUSPS)) %>% 
  st_transform(3643) %>% #Transform to Oregon Lambert
  mutate(name=gsub(' and ',' & ',name)) %>% #Replace "and" with "&"
  mutate(name=gsub('Cascades Slopes','Cascades\nSlopes',name)) %>% #Add line break (\n) to "Cascades Slopes"
  mutate(nRecords=sapply(st_contains(.,dat),length), #Number of bee specimens per ecoregion
         nFlwGenera=sapply(st_contains(.,dat),function(x) length(unique(na.omit(dat$genusPlant[x]))))) #Number of flower genera
  
#Overwrite county name using shapefiles, using lat/lon data from individual records (takes ~10 sec)
countyMatch <- sapply(st_within(dat,allCounty),function(x) x) #Get county indices for locations

if(any(!sapply(countyMatch,length)!=0)){
  warning(paste0(sum(!sapply(countyMatch,length)!=0),' samples located outside of ', str_flatten(includedStates, collapse = ", "), " and British Columbia discarded"))
  # ggplot()+ geom_sf(data=orCounties)+ #Map of samples outside Oregon
  #   geom_sf(data=filter(dat,sapply(countyMatch,length)==0))
  dat <- dat %>% filter(sapply(countyMatch,length)!=0)
}

# below doesn't work with NA for county, changed to missing text to get it to work
dat <- dat %>% mutate(
  county = case_when(is.na(county) ~ "missing" ,
                     .default = county)
)

if(any(sum(dat$county!=allCounty$altName[unlist(countyMatch)]))){
  warning(paste0(sum(dat$county!=allCounty$altName[unlist(countyMatch)]),' recorded county names are not inside matching county borders. Changed to match counties.'))
  dat$county <- allCounty$altName[unlist(countyMatch)]
}

#Record singleton species for "awards" (only 1 specimen found)
singles <- dat %>% st_drop_geometry() %>% group_by(genSpp) %>%
  summarize(n=n(),collector=first(collector)) %>%
  filter(!grepl('spp\\.',genSpp)) %>%
  filter(n==1)

dat <- dat %>% mutate(across(c(day, month, locality), .fns = str_trim )) # fixing whitespace as found below

#Test for whitespace
if(any(apply(st_drop_geometry(dat),2,wSpaceTest))){
  stop('Trailing/leading whitespace found')
  apply(st_drop_geometry(dat),2,wSpaceTest)
} 

#Test for repeated spaces
if(any(apply(st_drop_geometry(dat),2,function(x) any(grepl('  ',sort(unique(x))))))){
  stop('Double spaces found')
  apply(st_drop_geometry(dat),2,function(x) any(grepl('  ',sort(unique(x)))))
}

#TODO add column that splits east and west of cascades by ecoregion

#Save data to Rdata file
save.image('Robinson/Data/cleanedV3.Rdata') #Cleaned data + helper function

