#ORGANIZE DATA FROM A. MELATHOPOLOUS AND MAKE FUNCTIONS
#WRITTEN BY SAMUEL ROBINSON, FALL 2020

#Load packages -------------------------
library(tidyverse)
library(sf)


#Load and organize data ---------------------------
dat <- read.csv("Robinson/Data/volunteer_csv.csv",stringsAsFactors = FALSE,sep=',',strip.white = TRUE,na.strings=c('NA','')) %>% 
  select(day:year,County,Location:Genus,Species,Sex,Caste,associatedTaxa) %>%
  rename_with(tolower) %>% rename('lat'='dec..lat.','lon'='dec..long.','flwGenus'='associatedtaxa') %>% 
  mutate(across(c(genus,species,lat,lon),~str_trim(.))) %>% #For some reason, whitespace isn't being trimmed properly
  mutate_at(vars(subfam:genus),~gsub('pan trap \\(assorted colors\\)','Pan',.)) %>% #Excel error?
  mutate(species=ifelse(is.na(species),'spp.',species)) %>% 
  mutate(genSpp=paste(genus,species,sep=' ')) %>% 
  filter(collector!='') %>%  #Blank collector label
  mutate(day=ifelse(day>30 & month==6,30,day)) %>% #Fix bad dates
  mutate(year=ifelse(year==2023,2018,year)) %>% 
  mutate(date=paste(year,month,day,sep='-')) %>%
  mutate(date=as.Date(date,format='%Y-%m-%d')) %>% 
  mutate(flwGenus=gsub('foraging on: ','',flwGenus,fixed=TRUE)) %>% 
  mutate(flwGenus=word(flwGenus,1)) %>% 
  st_as_sf(coords=c('lon','lat')) %>% st_set_crs(4269) %>% st_transform(3643) #Coordinates

#Shapefiles of Oregon counties
orCounties <- st_read("Robinson/Shapefiles/orcntypoly.shp") %>%
  select(unitID:altName) %>% st_set_crs(4269) %>% #NAD83
  st_transform(3643) %>%  #Oregon Lambert
  mutate(nRecords=sapply(st_contains(.,dat),length), #Number of bee specimens per county
         nFlwGenera=sapply(st_contains(.,dat),function(x) length(unique(dat$flwGenus[x])))) #Number of flower genera

#Shapefiles of Oregon ecoregions
orEcoReg <- st_read("Robinson/Shapefiles/or_eco_l3.shp") %>% transmute(name=NA_L3NAME) %>% 
  st_transform(3643) %>% #Oregon Lambert
  mutate(name=gsub(' and ',' & ',name)) %>% mutate(name=gsub('Cascades Slopes','Cascades\nSlopes',name)) %>% 
  mutate(nRecords=sapply(st_contains(.,dat),length), #Number of bee specimens per ecoregion
         nFlwGenera=sapply(st_contains(.,dat),function(x) length(unique(dat$flwGenus[x])))) #Number of flower genera

#Overwrite county name using shapefiles, using lat/lon data from individual records (takes ~10 sec) 
dat$county <- orCounties$altName[sapply(st_within(dat,orCounties),function(x) x)]

#Singleton species for "awards" (only 1 found in collection)
singles <- dat %>% st_drop_geometry() %>% group_by(genSpp) %>% 
  summarize(n=n(),collector=first(collector)) %>% filter(n==1)

# Functions ---------------------

#Function to make abundance plots from data frame
abundPlots <- function(d,colSet='Set1',scaleYtext=c(1,1,1)){
  require(RColorBrewer)
  require(tidyverse)
  require(ggpubr)
  options(dplyr.summarise.inform=FALSE)
  
  #Colours for individual families
  famCols <- data.frame(
    family=c('Andrenidae','Apidae','Colletidae','Halictidae','Megachilidae'),
    cols=brewer.pal(5,colSet)) #Colour scheme for families
  
  #Species abundance plot (no genera-only records)
  
  plotDat <- d %>% filter(species!='spp.') %>%  #Data for histograms
    group_by(family,genSpp) %>% summarize(n=n()) %>%
    arrange(desc(family),n) %>% ungroup() %>% 
    mutate(genSpp=factor(genSpp,level=genSpp))
  
  rectDat <- d %>% filter(species!='spp.') %>%  #Data for background rectangles
    group_by(family,genSpp) %>% summarize(n=n()) %>% 
    group_by(family) %>% summarize(nSpp=n()) %>% ungroup() %>% 
    arrange(desc(family)) %>% 
    mutate(ymax=cumsum(nSpp)+0.5,ymin=lag(ymax,default=0.5)) %>% 
    rowwise() %>% mutate(ymid=mean(c(ymax,ymin))) %>%
    mutate(xmin=0,xmax=max(plotDat$n)) %>% 
    left_join(famCols,by='family')
  
  (sppPlot <- ggplot()+ geom_col(data=plotDat,aes(n,genSpp))+ #Make plot
      geom_rect(data=rectDat,
                aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=family),
                alpha=0.3,show.legend = FALSE)+
      geom_col(data=plotDat,aes(n,genSpp,fill=family),show.legend = FALSE)+
      geom_text(data=rectDat,aes(x=xmax*0.9,y=ymid,label=family),hjust=1)+
      theme(axis.text.y=element_text(vjust=0.5,size=8*scaleYtext[1]))+
      labs(y=NULL,x='Number of specimens',title='Species')+
      scale_fill_manual(values=rev(as.character(rectDat$cols))))
  
  #Genera abundance plots
  
  plotDat <- d %>%  #Data for histograms
    group_by(family,genus) %>% summarize(n=n()) %>%
    arrange(desc(family),n) %>% ungroup() %>% 
    mutate(genus=factor(genus,level=genus)) 
  
  rectDat <- d %>%  #Data for background rectangles
    group_by(family,genus) %>% summarize(n=n()) %>% 
    group_by(family) %>% summarize(nSpp=n()) %>% ungroup() %>% 
    arrange(desc(family)) %>% 
    mutate(ymax=cumsum(nSpp)+0.5,ymin=lag(ymax,default=0.5)) %>% 
    rowwise() %>% mutate(ymid=mean(c(ymax,ymin))) %>% 
    mutate(xmin=0,xmax=max(plotDat$n)) %>% 
    left_join(famCols,by='family')
  
  (genPlot <- ggplot()+ geom_col(data=plotDat,aes(n,genus))+ #Make plot
      geom_rect(data=rectDat,
                aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=family),
                alpha=0.3,show.legend = FALSE)+
      geom_col(data=plotDat,aes(n,genus,fill=family),show.legend = FALSE)+
      geom_text(data=rectDat,aes(x=xmax*0.9,y=ymid,label=family),hjust=1)+
      theme(axis.text.y=element_text(vjust=0.5,size=8*scaleYtext[2]))+
      labs(y=NULL,x='Number of specimens',title='Genera')+
      scale_fill_manual(values=rev(as.character(rectDat$cols))))
  
  #Family abundance plots
  plotDat <- d %>%  #Data for histograms
    group_by(family) %>% summarize(n=n()) %>% ungroup() %>% 
    arrange(desc(family),n) %>% 
    mutate(family=factor(family,level=family)) %>% 
    left_join(famCols,by='family')
  
  (famPlot <- ggplot()+ geom_col(data=plotDat,aes(n,family))+ #Make plot
      geom_col(data=plotDat,aes(n,family,fill=family),show.legend = FALSE)+
      theme(axis.text.y=element_text(vjust=0.5,size=8*scaleYtext[3]))+
      labs(y=NULL,x='Number of specimens',title='Families')+
      scale_fill_manual(values=as.character(plotDat$cols)))
  
  #Put all plots together
  (a <- ggarrange(sppPlot,ggarrange(genPlot,famPlot,nrow=2),ncol=2))
  return(a)
  
}

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
# 
# abundTable(c2,c1,FALSE)
# abundTable(d,c1,c2,TRUE)
# debugonce(abundTable)

#Future function: split long single-column table into multi-column "wide" table

#Save data to Rdata file
save.image('Robinson/Data/cleaned.Rdata') #Cleaned data + helper function
