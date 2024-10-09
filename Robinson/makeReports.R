# GENERATE VOLUNTEER REPORTS USING TEMPLATE
# WRITTEN BY SAMUEL ROBINSON, FALL 2020

#Load packages ------------
library(rmarkdown)
library(knitr)

#Load data ----------

load('Robinson/Data/cleaned.Rdata')
volNames <- data.frame(names=unique(dat$collector)) #Collectors
rm(list=ls()[ls()!='volNames']) #Other files not needed from cleaned.Rdata

template <- readLines('Robinson/templateSheet.Rmd') #Rmarkdown template to use

#Make pdfs -------------

#Escape special characters in names (I think only O'Loughlin is problematic)
volNames$escNames <- gsub("'","\\\\'",volNames$names,fixed=TRUE)
volNames$escNames <- gsub("-"," ",volNames$escNames,fixed=TRUE) #Dashes in names cause problems as well

#Make file paths for output pdfs
path <- gsub("(\\s|\'|\\-)","_",volNames$names)
volNames$rmdFile <- paste0(path,'_Summary.Rmd')
# volNames$rmdFile <- file.path('.','Output_Sheets',paste0(path,'_Summary.Rmd'))
volNames$pdfFile <- paste0(path,'_Summary.pdf')
volNames$htmlFile <- paste0(path,'_Summary.html') ## adding html option


# setwd("Robinson/Data/Output_Sheets") #Move to Output_Sheets directory
# file.remove(dir()) #Clear files
#Loop for making PDFs. Takes roughly 10 seconds per person. Could vectorize or parallelize this to make it faster.
for(i in 1:nrow(volNames)){ 
  
  #Approach: make Rmd file, knit/render to pdf, delete Rmd file
  a <- gsub('VOLUNTEER_NAME',volNames$names[i],template) #Make volunteer-specific text
  a <- gsub('VOLUNTEER_ESCNAME',volNames$escNames[i],a) #Escaped character
  writeLines(a,volNames$rmdFile[i]) #Write Rmd file to folder
  render(input=volNames$rmdFile[i],output_file=volNames$pdfFile[i],quiet=TRUE,clean=TRUE,envir=new.env()) ## can replace pdfFile with htmlFile 
  #For some reason, kableExtra doesn't properly write \usepackage into preamble before knitting, so template has to have all the necessary LaTeX packages in header_includes
  
  #Alternative approach: parameterized reports (https://bookdown.org/yihui/rmarkdown/parameterized-reports.html) where names/escaped names are passed as arguments

  #Remove extra files and folders
  cleanup <- c(volNames$rmdFile[i],gsub('Rmd','log',volNames$rmdFile[i]),gsub('Rmd','tex',volNames$rmdFile[i]))
  file.remove(cleanup)
  unlink(paste0('./',gsub('.Rmd','_files',volNames$rmdFile[i])),recursive=TRUE,force=TRUE)
}
rm(a,i)
