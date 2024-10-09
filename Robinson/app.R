# shiny
library(shiny)
library(tidyverse)
library(sf)
library(knitr)
library(kableExtra)
library(ggpubr)
library(RColorBrewer)
library(sf)
library(ggspatial) #library(ggsn) replaced
library(shadowtext)
library(tinytex)
library(iNEXT)

load('Data/cleaned.Rdata')

#readLines('templateSheet.Rmd') #Rmarkdown
volNames <- tibble(names=unique(dat$collector)) # name options

# make named list of parameters for r markdown params
reports <- tibble( 
  collec = tail(tibble(names=unique(dat$collector))),
  filename = stringr::str_c(str_replace_all(collec$names, " ", "_"), "Summary.pdf"),
  params = purrr::map(collec, ~ list(collectorName = .)))
  
chaoTab <- dat %>% st_drop_geometry() %>% 
    group_by(collector, genSpp) %>%
    summarise(n = n())

# reports %>% 
#   select(output_file = filename, params) %>% 
#   pwalk(rmarkdown::render, input = "templateSheetRR.Rmd") # - old code to make pdfs per person - works differently in shiny

ui <- fluidPage( # set up page with download button etc. 
  titlePanel( "Download PDF"),
  sidebarPanel(
  selectInput(inputId = "names", label = "Collector", choices = unique(volNames$names)),
  downloadButton("report")
),
mainPanel(plotOutput("sppRich"),
uiOutput("sppRichTab")
)
)

server <- function(input, output){
  
  output$sppRichTab <- renderTable({
    chaoTabPer  <- chaoTab %>% filter(collector %in% input$names) %>% ungroup() 
  })
  
  output$sppRich <- renderPlot({
   chaoDat  <- chaoTab %>% filter(collector %in% input$names) %>% ungroup() %>% 
    pivot_wider(names_from = collector, values_from = n, values_fill = 0) %>% 
    column_to_rownames(var = "genSpp")
  
  ggiNEXT(iNEXT(chaoDat, q = 1, nboot = 25))
  
  # sppRich  <- ggiNEXT(iNEXT(chaoDat))
    
    # chaoEst <-  iNEXT( # create estimates
    
    
    # params <- list(collectorName = input$names) - was trying to make html output here but needs more time
    # 
    # HTML(markdown::markdownToHTML(knit("templateSheetRR.Rmd",  params = params, quiet = TRUE)))
  })
  
  output$report <- downloadHandler(
   filename = paste0(str_replace_all(input$names, " " ,""), "Results.pdf"), 
     content = function(file) {
       tempReport <- file.path(tempdir(), "templateSheetRR.Rmd")
       file.copy("templateSheetRR.Rmd", tempReport, overwrite = TRUE)
       
       params <- list(collectorName = input$names)
       
        rmarkdown::render(
          input = tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      
                          })
}

shinyApp(ui = ui, server = server)

#runExample("10_download")   # file download wizard

# server <- function(input, output){
#   
#   output$sppRich <- renderTable({
#     
#     input$names
#     # not using this anymore but kept to have something working
#     # dat %>% st_drop_geometry() %>% 
#     #     filter(collector %in% input$names) %>% 
#     #     group_by(collector) %>% 
#     #     summarise(n = n_distinct(genSpp)) 
#     
#   })
# }