# run new template to test issues

library(sf)
library(knitr)
library(kableExtra)
library(ggpubr)
library(RColorBrewer)
library(sf)
library(ggspatial) #library(ggsn) replaced
library(shadowtext)
library(tinytex)
library(tidyverse)

load('Robinson/Data/cleaned2025.Rdata') # or load('Data/cleaned.Rdata')

# Test render reports -----------------------------------------------------

rmarkdown::render(
  input = "Robinson/templateSheetNewFormat25.Rmd",
  output_file = "MichaelO'Loughlin_Report_2023.pdf",
  params = list(collectorName = "Michael O'Loughlin")
)

rmarkdown::render(
  input = "Robinson/templateSheetNewFormat25_no2023.Rmd",
  output_file = "ad_Report_2023.pdf",
  params = list(collectorName = "Bonnie Shoffner")
)
#readLines('templateSheet.Rmd') #Rmarkdown
volNames <- tibble(names=unique(dat$collector)) # name options

# make named list of parameters for r markdown params - this will need to make sure there's more than one observation possible
reports <- tibble( 
  collec = tibble(names=unique(dat$collector)),
  filename = stringr::str_c(str_replace_all(collec$names, " ", "_"), "Summary.pdf"),
  params = purrr::map(collec, ~ list(collectorName = .)))

# to run
# 
toRun <- reports %>% filter(row_number()%in%1)

dat %>% filter(samplingProtocol%in%"aerial net") %>%  
  st_drop_geometry %>% 
  filter(year== 2023) %>% 
  filter(str_detect(collector, "\\|")) %>% 
    filter(!is.na(genus)) %>% 
  group_by(collector, genSpp) %>% 
  summarise(n())

# might be faster to do: 

# TODO - do we remove obs without IDs at the start, or produce blank reports?

caughtAll <- dat %>% st_drop_geometry() %>% 
  filter(!is.na(genus)) %>% filter(!is.na(family)) %>% # do pre filtering here - only bees ided to genus or family
  filter(samplingProtocol == "aerial net") %>% # only netted bees
  filter(!is.na(day)) %>% 
  group_by(collector, year) %>%
  summarise(n = n()) %>% 
  pivot_wider(names_from = year, values_from = n, values_fill = 0) %>% 
  pivot_longer(cols = `2019`:`2017`, values_to = "nCaught", names_to = "year")

caught23 <- caughtAll %>% ungroup() %>% 
  filter(year%in%2023)

# make named list of parameters for r markdown params - one for 2023 observers, one for not

# None 2023 ---------------------------------------------------------------

none2023 <- caught23 %>%
  filter(nCaught < 1) %>% 
  select(collector)

no23dat <- dat %>% semi_join(none2023) #no 23 obs

volNamesNo23 <- tibble(names=unique(no23dat$collector)) # name options no 23

# reportsNo23 <- tibble( 
#   collec = tibble(names=unique(no23dat$collector)),
#   filename = stringr::str_c(str_replace_all(collec$names, " ", "_"), "_Summary.pdf"),
#   params = purrr::map(collec, ~ list(collectorName = .)))

rpVert <- no23dat %>% filter(str_detect(string = collector, pattern = "\\|")) #%>% 

# netOnly<- rpVert %>% filter(samplingProtocol %in% "aerial net")
rpVertRep23 <- tibble( input = "Robinson/templateSheetNewFormat25_no2023.Rmd",
                       collec = tibble(names = unique(rpVert$collector)),
                       output_file = stringr::str_c("noneReports/", str_replace_all(collec$names, " ", "_"), "_Summary2023.pdf"),
                       params = purrr::map(collec$names, ~ list(collectorName = .)))

# extra step to deal with vertical lines in names
rpVertRep23 %>% mutate(output_file = str_replace(output_file, "\\|", "and")) %>% 
  select(!collec) %>% pwalk(.f = rmarkdown::render)

reportsNo23 <- tibble( input = "Robinson/templateSheetNewFormat25_no2023.Rmd",
                         collec = tibble(names=unique(no23dat$collector)),
                         output_file = stringr::str_c("noneReports/", str_replace_all(collec$names, " ", "_"), "_Summary2023.pdf"),
                         params = purrr::map(collec$names, ~ list(collectorName = .)))

reportsNo23 %>% filter(!str_detect(string = output_file, pattern = "\\|")) %>% 
  select(!collec) %>% pwalk(.f = rmarkdown::render)

#jsonlite::write_json(reportsNo23$output_file, "Robinson/no2023beefiles.JSON")
# Some 2023 ---------------------------------------------------------------

some2023 <- caught23 %>%
  filter(nCaught >= 1) %>% 
  select(collector)

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

reportsSome23 <- tibble( input = "Robinson/templateSheetNewFormat25.Rmd",
  collec = tibble(names=unique(some23dat$collector)),
  output_file = stringr::str_c("someReports/", str_replace_all(collec$names, " ", "_"), "_Summary2023.pdf"),
  params = purrr::map(collec$names, ~ list(collectorName = .)))

reportsSome23 %>% select(!collec) %>% pwalk(.f = rmarkdown::render)

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
