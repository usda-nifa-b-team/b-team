Describing shiny app process 
Rowan Rampton, Aug 26, 2024

The app.R contains the shiny app code - all consist of code you want to run, then 3 parts - a user interface + server + shinyapp call

The code at the start loads the packages required, loads the robinson data - I have barely looked at the format and need to do so more, will try to update this later when I do
It then makes name selection options (collectors for now), and uses those to make parameters (params) for parameterized r markdown output - lets you put variables into the output

It then sets up a ui - title, sidebar - selection and download button, mainpanel - this could be anything but for now I've made it show spp accumulation 

Then the server makes outputs to go into the ui - right now it has output$sppRich and output$sppRichTab to go in the mainPanel - extrapolation and spp list. 
In the sidepanel - it has output$report, which controls what happens when the download button is pressed. Both are based on input$names - which is the selection made using the drop down. 

The output$report is made by creating a list of options for the parameters (params) and then rendering a pdf based on the parameter. 

I modified the original rmd to be parameterized instead of using a custom loop - much easier to run in shiny. New name: templateSheetRR.rmd
I made a couple minor fixes to make the rmd run before that too, things like replacing outdated packages in the mapping software. 








