# run new template to test issues

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

load('Robinson/Data/cleanedV3.Rdata') # or load('Data/cleaned.Rdata')

# TODO 
# figure out how to render with state x collector combo - create combined variable?

#DONE

#DONE(?)
# get figures working (depending on how render works) that depend on different state shapefiles etc.

# Test render reports -----------------------------------------------------

# top4testing <- dat %>%
#   filter(state!="OR") %>%
#   group_by(collector) %>%
#   count() %>%
#   arrange(desc(n)) %>%
#   st_drop_geometry() %>%
#   head(n = 15) %>%
#   dplyr::select(!n)
# 
# stateTests <- dat %>%
#   semi_join(top4testing) %>%
#   filter(!is.na(genSpp)) %>%
#   filter(state!="OR") %>%
#   group_by(collector) %>%
#   mutate (n = n()) %>%
#   arrange(desc(n))
# 
#  dat <- stateTests
# 
# lr <- dat %>% filter(collector %in% "Lisa Robinson") %>%
#   filter(!is.na(genSpp))
# 
  rmarkdown::render(
    input = "Robinson/templateSheetV3_TESTING.Rmd",
      output_file = "testV3_WA.pdf",
      params = list(collectorName = "Lisa Robinson",
                    state = "OR", 
                    year = "2023")
  )
  
load('Robinson/Data/cleanedV3.Rdata') # or load('Data/cleaned.Rdata')
# dat is being overwritten in rmd and here
  rmarkdown::render(
    input = "Robinson/templateSheetV3_TESTING.Rmd",
    output_file = "testV3_OR.pdf",
    params = list(collectorName = "Lisa Robinson",
                  state = "OR", 
                  year = "2023")
  )
  
# List volunteers for making all reports ---- 

volNames <- tibble(names=unique(dat$collector)) # name options

# make named list of parameters for r markdown params - this will need to make sure there's more than one observation possible
reports <- tibble( 
  collec = tibble(names=unique(dat$collector)),
  filename = stringr::str_c(str_replace_all(collec$names, " ", "_"), "Summary.pdf"),
  params = purrr::map(collec, ~ list(collectorName = .)))

# to run
# 
toRun <- reports %>% filter(row_number()%in%1)

# might be faster to do: 

# TODO - do we remove obs without IDs at the start, or produce blank reports?

# finding number of catches in each year
caughtAll <- dat %>% st_drop_geometry() %>% 
  filter(!is.na(genus)) %>% filter(!is.na(family)) %>% # do pre filtering here - only bees ided to genus or family
  filter(samplingProtocol == "aerial net") %>% # only netted bees
  filter(!is.na(day)) %>% 
  group_by(collector, year) %>%
  summarise(n = n()) %>% 
  pivot_wider(names_from = year, values_from = n, values_fill = 0) %>% 
  pivot_longer(cols = `2024`:`2017`, values_to = "nCaught", names_to = "year")

# for running 2023 by itself
caught23 <- caughtAll %>% ungroup() %>% 
  filter(year%in%2023)

# make named list of parameters for r markdown params - one for those with observations in 2023, one for those with 0

# None 2023 ---------------------------------------------------------------

none2023 <- caught23 %>%
  filter(nCaught < 1) %>% 
  dplyr::select(collector)

no23dat <- dat %>% semi_join(none2023) # get subset of dat observers with no 2023 observations

volNamesNo23 <- tibble(names=unique(no23dat$collector)) # name of observers with no 2023 obs. 

# code for confirming reports for observers with vertical separator in collector name are working
# rpVert <- no23dat %>% filter(str_detect(string = collector, pattern = "\\|")) 
# 
# rpVertRep23 <- tibble( input = "Robinson/templateSheetNewFormat25_no2023.Rmd",
#                        collec = tibble(names = unique(rpVert$collector)),
#                        output_file = stringr::str_c("noneReports/", str_replace_all(collec$names, " ", "_"), "_Summary2023.pdf"),
#                        params = purrr::map(collec$names, ~ list(collectorName = .)))
# 
# # extra step to deal with vertical lines in names
# rpVertRep23 %>% mutate(output_file = str_replace(output_file, "\\|", "and")) %>% 
#   dplyr::select(!collec) %>% pwalk(.f = rmarkdown::render)

reportsNo23 <- tibble( input = "Robinson/templateSheetNewFormat25_no2023.Rmd", # this is not updated to V3 yet
                         collec = tibble(names=unique(no23dat$collector)),
                         output_file = stringr::str_c("noneReports/", str_replace_all(collec$names, " ", "_"), "_Summary2023.pdf"),
                         params = purrr::map(collec$names, ~ list(collectorName = .)))

reportsNo23 %>% mutate(output_file = str_replace(output_file, "\\|", "and")) %>% 
  dplyr::select(!collec) %>% pwalk(.f = rmarkdown::render) # need to create folder when you want to run this for real

#jsonlite::write_json(reportsNo23$output_file, "Robinson/no2023beefiles.JSON")
# Some 2023 ---------------------------------------------------------------

some2023 <- caught23 %>%
  filter(nCaught >= 1) %>% 
  dplyr::select(collector)

# quick check that all reports are included in filters
done <- some2023 %>% rbind(none2023)
caught23 %>% anti_join(done)

# check for reports that somehow don't match either - 
# only Andony - obs, but none ided to species in 2023 is the problem, for now just ran manually below

rmarkdown::render(
  input = "Robinson/templateSheetNewFormat25_no2023.Rmd",
  output_file = "Andony_Melathopoulos_Report_2023.pdf",
  params = list(collectorName = "Andony Melathopoulos")
)

some23dat <- dat %>% semi_join(some2023) # some 23 obs 
volNamesSome23 <- tibble(names=unique(some23dat$collector)) # name options some 23

reportsSome23 <- tibble( input = "Robinson/templateSheetV3_TESTING.Rmd",
  collec = tibble(names=unique(some23dat$collector)),
  output_file = stringr::str_c(str_replace_all(collec$names, " ", "_"), "_Summary2023.pdf"),
  params = purrr::map(collec$names, ~ list(collectorName = .,
                                           state = "OR", # might be a way to get collec$names to be a combo variable, then have a fxn separating it wider etc.
                                           year = "2023")))

reportsSome23[4,] %>% dplyr::select(!collec) %>% pwalk(.f = rmarkdown::render)

#jsonlite::write_json(reportsSome23$output_file, "Robinson/some2023beefiles.JSON")
# Below here - troubleshooting errors -------------------------------------

# debugonce(abundPlots)

dat1 <- dat %>%
  filter(samplingProtocol == "aerial net") %>% # remove non netted specimens
  filter(!is.na(day)) # remove missing dates

abundPlots(dat1,colourSet="Set1")

volCounties <- dat1 %>% count(county) %>% 
  st_join(orCounties,.) 

ggplot(volCounties)+
  geom_sf(aes(fill=n),show.legend = FALSE)+
  geom_sf(data=dat1,col="black",size=0.5)+
  geom_shadowtext(data=volCounties,
                  aes(x=st_coordinates(st_centroid(volCounties))[,1],y=st_coordinates(st_centroid(volCounties))[,2],
                      label=n))+
  # scale_fill_continuous(low="lightblue",high="red",na.value="white")+
  scale_fill_distiller(palette="OrRd",direction=1,na.value="white")+
  annotation_scale()+ # replaced obsolete ggsn N bar and scale below with ggspatial equivalent
  annotation_north_arrow(location = "tr", style = north_arrow_minimal())+
  #scalebar(orCounties,dist=25,dist_unit="mi",transform=FALSE,model="NAD83",st.size=3,location="bottomright") +
  #north(orCounties,symbol=3, location="topleft")+
  theme_bw() + theme(axis.text=element_blank(), axis.title=element_blank(),
                     axis.line = element_blank(),axis.ticks = element_blank(),
                     panel.grid = element_blank())

abundPlots(dat1,colourSet="Set1",scaleYtext = c(0.4,0.5,1.1))

p1 <- dat1 %>% st_drop_geometry() %>% count(collector) %>% 
  arrange(n) %>% mutate(collector=factor(collector,levels=collector)) %>% 
  mutate(isCol=collector=="VOLUNTEER_NAME") %>% 
  ggplot(aes(x=collector,y=n))+geom_col(aes(fill=isCol),show.legend=FALSE)+
  geom_hline(yintercept=c(50,500,1000),linetype="dashed",col="grey")+
  coord_flip()+scale_fill_manual(values=c("black","red"))+
  labs(y="Number of Specimens",x=NULL)+
  scale_y_continuous(breaks=c(50,250,500,750,1000,1250))

# Bar plot of species per volunteer, (x = Collector, y = # specimens), with hline and 500 and 50 bees
p2 <- dat1 %>% st_drop_geometry() %>%  group_by(collector) %>% summarize(n=length(unique(genSpp))) %>% 
  arrange(n) %>% mutate(collector=factor(collector,levels=collector)) %>% 
  mutate(isCol=collector=="VOLUNTEER_NAME") %>% 
  ggplot(aes(x=collector,y=n))+geom_col(aes(fill=isCol),show.legend=FALSE)+
  geom_hline(yintercept=c(50,100),linetype="dashed",col="grey")+
  coord_flip()+scale_fill_manual(values=c("black","red"))+
  labs(y="Number of Unique Species",x=NULL)+
  scale_y_continuous()

ggarrange(p1,p2,ncol=2)

dat1 %>% ggplot()+
  geom_sf(data=orCounties,aes(fill=ifelse(nRecords==0,NA,nRecords)),show.legend = FALSE)+
  geom_sf(size=0.5)+
  # geom_sf_text(data=st_centroid(orCounties),aes(geometry=geometry,label=nRecords),col="white",size=3)+ #Regular text is hard to read
  geom_shadowtext(data=orCounties,
                  aes(x=st_coordinates(st_centroid(orCounties))[,1],y=st_coordinates(st_centroid(orCounties))[,2],
                      label=ifelse(nRecords==0,NA,nRecords)))+
  annotation_scale()+ # replaced obsolete ggsn N bar and scale below with ggspatial equivalent
  annotation_north_arrow(location = "tr", style = north_arrow_minimal())+
  #scalebar(orCounties,dist=25,dist_unit="mi",transform=FALSE,model="NAD83",st.size=3,location="bottomright") +
  #north(orCounties,symbol=4,location="topleft")+
  # scale_fill_continuous(low="lightblue",high="red",na.value="white")+
  scale_fill_distiller(palette="OrRd",direction=1,na.value="white")+
  theme_bw() + theme(axis.text=element_blank(), axis.title=element_blank(),
                     axis.line = element_blank(),axis.ticks = element_blank(),
                     panel.grid = element_blank())


orEcoReg %>% ggplot()+
  geom_sf(aes(fill=name))+
  geom_sf(data=dat1,size=0.5)+
  geom_shadowtext(aes(x=st_coordinates(st_centroid(orEcoReg))[,1],y=st_coordinates(st_centroid(orEcoReg))[,2],
                      label=nRecords))+
  annotation_scale()+ # replaced obsolete ggsn N bar and scale below with ggspatial equivalent
  annotation_north_arrow(location = "tr", style = north_arrow_minimal())+
  #scalebar(orEcoReg,dist=25,dist_unit="mi",transform=FALSE,model="NAD83",st.size=3,location="bottomright") +
  #north(orEcoReg,symbol=4,location="topleft")+labs(fill="Ecoregion")+
  scale_fill_brewer(type="qual",palette=3)+
  theme_bw() + theme(axis.text=element_blank(), axis.title=element_blank(),
                     axis.line = element_blank(),axis.ticks = element_blank(),
                     panel.grid = element_blank())

quantFun <- function(x, q) { #"Borrowed" from dplyr 1.0.0 summary
  tibble(x = quantile(x, q), q = q)
}

#Info for flight season
phenoRange <- dat1 %>% st_drop_geometry() %>% mutate(doy=as.numeric(format(date,format="%j"))) %>%
  summarize(quantFun(x=doy,q=c(0.05,0.25,0.5,0.75,0.95))) %>% 
  mutate(xOffset=as.Date(as.character(x-10),format="%j"), #x-Location for text label
         yOffset=length(unique(dat1$genSpp[!grepl(".spp",dat1$genSpp)])), #y-location for text label
         x=as.Date(as.character(x),format="%j"), #Actual date
         lab=paste0(q*100,"%"), # Percentile Label
         dat=format(x,format="%B %d")) #Date label

rectDat <- with(phenoRange,data.frame(perc=c("50th","90th"),
                                      ymin=x[q==0.25|q==0.05],ymax=x[q==0.75|q==0.95],
                                      xmin=c(0,0),xmax=rep(yOffset[1],2)))

dat1 %>% st_drop_geometry() %>% mutate(doy=as.numeric(format(date,format="%j"))) %>% 
  filter(species!="spp.") %>% group_by(genSpp) %>% 
  summarize(n=n(),quantFun(x=doy,q=seq(0,1,0.25))) %>%
  mutate(q=rank(q)) %>% 
  pivot_wider(names_from=q,names_prefix="q",values_from=x) %>% 
  mutate(across(c(q1,q2,q4,q5),~ifelse(n>10,.,NA))) %>% 
  ungroup() %>% 
  arrange(desc(q3)) %>% mutate(across(c(q1:q5),~as.Date(as.character(.),format="%j"))) %>% 
  # filter(n>20) %>% 
  mutate(genSpp=factor(genSpp,levels=genSpp)) %>%
  ggplot(aes(x=genSpp)) + geom_linerange(aes(ymin=q1,ymax=q5))+
  
  geom_linerange(aes(ymin=q2,ymax=q4),size=1)+ #Bee emergence dates
  geom_point(aes(y=q3),size=1)+
  # geom_text(data=phenoRange,aes(x=yOffset,y=xOffset,label=lab),col="red",alpha=0.8)+ #Percentile markers
  # geom_hline(data=phenoRange,aes(yintercept=x),col="red",linetype="dashed",alpha=0.8)+
  geom_rect(data=rectDat,aes(x=NULL,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.2,show.legend = FALSE)+
  coord_flip()+labs(x=NULL,y=NULL)+
  theme(panel.grid.major.x=element_line(colour="grey"),
        #panel.grid.minor.x=element_line(colour="grey"),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=3))+
  scale_y_date(date_breaks="1 month",date_labels="%B",date_minor_breaks="1 weeks")

dat1 %>% filter(state %in% "WA") %>% 
  filter(collector %in% "Lisa Robinson") %>% 
  st_drop_geometry() %>% 
  abundTable(county,genus) %>%
  kable(format="latex",longtable=TRUE,caption="Number of bee specimens from each county, by genus. You may want to focus your sampling in under-sampled counties.") %>%
  kable_styling(latex_options = c("striped","repeat_header"),font_size=4.25) %>% 
  row_spec(0, angle=90) %>% #Rotate row 0 (header) by 90 degrees
  column_spec(column=1,italic=TRUE,latex_column_spec = "r") %>% #Genus names
  column_spec(column=2:length(unique(data$county))+1) %>% #Counties + data
  column_spec(column=length(unique(data$county))+2,latex_column_spec = "l")



# testing ecoregion split for phenology -----------------------------------

dataSplitEco <- dataNoNA %>% st_join(orEcoReg)

phenoE <- dataSplitEco %>%
  filter(!is.na(name)) %>% 
  filter(str_detect(name, 
    "Blue Mountains|Columbia Plateau|Eastern Cascades\nSlopes & Foothills|Northern Basin & Range|Snake River Plain" ))

phenoW <- dataSplitEco %>%
  filter(!is.na(name)) %>% 
  filter(str_detect(name, 
                    "Cascades|Coast Range|Willamette Valley|Klamath Mountains" ))

phenoRangeW <- phenoW %>% st_drop_geometry() %>% filter(!is.na(date)) %>% 
  mutate(doy=as.numeric(format(date,format="%j"))) %>%
  summarize(quantFun(x=doy,q=c(0.05,0.25,0.5,0.75,0.95))) %>% 
  mutate(xOffset=as.Date(as.character(x-10),format="%j"), #x-Location for text label
         yOffset=length(unique(phenoW$genSpp[!grepl(".spp",phenoW$genSpp)])), #y-location for text label
         x=as.Date(as.character(x),format="%j"), #Actual date
         lab=paste0(q*100,"%"), # Percentile Label
         dat=format(x,format="%B %d")) #Date label

rectDatW <- with(phenoRangeW,data.frame(perc=c("50th","90th"),
                                        ymin=x[q==0.25|q==0.05],ymax=x[q==0.75|q==0.95],
                                        xmin=c(0,0),xmax=rep(yOffset[1],2)))


phenoW %>% st_drop_geometry() %>% filter(!is.na(date)) %>% 
  mutate(doy=as.numeric(format(date,format="%j"))) %>%
  filter(species!="spp.") %>% group_by(genSpp) %>%
  summarize(n=n(),quantFun(x=doy,q=seq(0,1,0.25))) %>%
  mutate(q=rank(q)) %>%
  pivot_wider(names_from=q,names_prefix="q",values_from=x) %>%
  mutate(across(c(q1,q2,q4,q5),~ifelse(n>10,.,NA))) %>%
  ungroup() %>%
  arrange(desc(q3)) %>% mutate(across(c(q1:q5),~as.Date(as.character(.),format="%j"))) %>%
  # filter(n>20) %>%
  mutate(genSpp=factor(genSpp,levels=genSpp)) %>%
  ggplot(aes(x=genSpp)) + geom_linerange(aes(ymin=q1,ymax=q5))+
  
  geom_linerange(aes(ymin=q2,ymax=q4),size=1)+ #Bee emergence dates
  geom_point(aes(y=q3),size=1) +
  
  # geom_text(data=phenoRange,aes(x=yOffset,y=xOffset,label=lab),col="red",alpha=0.8)+ #Percentile markers
  # geom_hline(data=phenoRange,aes(yintercept=x),col="red",linetype="dashed",alpha=0.8)+
  geom_rect(data=rectDatW,aes(x=NULL,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax),alpha=0.2,show.legend = FALSE)+
  
  coord_flip()+labs(x=NULL,y=NULL)+
  theme(panel.grid.major.x=element_line(colour="grey"),
        #panel.grid.minor.x=element_line(colour="grey"),
        axis.text.x=element_text(size=7),
        axis.text.y=element_text(size=3))+
  scale_y_date(date_breaks="1 month",date_labels="%B",date_minor_breaks="1 weeks")
