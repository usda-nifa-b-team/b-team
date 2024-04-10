# Modifying the js to be R independent

# makenetvis was used to produce a standard bipartiteD3Script.js for the steens network

# the default script (bipartiteD3Script.js) was renamed steens.js, with a copy to modify called steensEXP.js

# Below alters bipd3m() to read a .js and make the d3 widget instead of writing a script then making a d3 widget ----

# this tests my modified steensEXP.js - to be R independent ----

bipartite_D3TestExp<- function(MainFigSize = c(1100, 3600),
                            IndivFigSize = c(400,2000))
{
  LoadVisJS()
  
  r2d3Out  <- r2d3::r2d3(data = NA, script = "steensEXP.js",
                         height= MainFigSize[2], width= MainFigSize[1] ,
                         d3_version = '5',
                         dependencies ="vizjs.js")
  
return(r2d3Out)
}

bipartite_D3TestExp()


# can compare to original ----
bipartite_D3Test<- function(MainFigSize = c(1100,1600),
                            IndivFigSize = c(400,2000))
{
  LoadVisJS()
  
  r2d3Out  <- r2d3::r2d3(data = NA, script = paste0("steens",".js"),
                         height= MainFigSize[2], width= MainFigSize[1] ,
                         d3_version = '5',
                         dependencies ="vizjs.js")
  
  return(r2d3Out)
}

bipartite_D3Test()

#here are my current colour by rules 

famCols<- tibble(cols=c(brewer.pal(5,'Set1'), "black"), 
                 fams = c('Andrenidae','Apidae','Colletidae',
                          'Halictidae','Megachilidae', "Melittidae"))

#extracting the data used for some testing below ----
#debugonce(makeNetVis)
makeNetVis(data = plantPolNamedsf, isMon = T, isNF=F,
         subsetName = unique(plantPolNamedsf$monument)[3], static = F)
# within debug, wrote testdata.csv to file 
df <- read_csv("testData.csv")

# also need family name vector
famName <- read_csv("famName.csv")
famNameVec <- as.character(famName$cols)
names(famNameVec) <- famName$pollinatorINatName
NamedColourVector <- famNameVec

#and order vector
order <- read_csv("beeOrder.csv")
orderBeeName <- order$pollinatorINatName
SortPrimary <- orderBeeName 

# this code makes the data in the script -----
#it might be useful at some point to convert data from r format to that required for js
JSON <- "JSON"
JSONColumn <- df %>% unite(col = "JSON", 1:2, sep = "\",\"") %>%
  mutate(JSON = paste0("\"", JSON, "\"")) %>% unite(col = "JSON",
                                                    sep = ",") %>% mutate(JSON = paste0("[", JSON, "]"))
data <- paste0("var data=[", paste0(JSONColumn$JSON, collapse = ",\n"),
               "]\n\n\n")

# other definitions required ----
#most of these are replaceable, some are
ColourBy  <-  1
colouroption <- "manual"
MainFigSize <-  c(1100,1600) 
IndivFigSize <- c(400,2000)
IncludePerc <- TRUE
BarSize <- 35
mp <- c(1,1) 
MinWidth <- 10
Pad <- 1
Orientation <- "vertical"
PrimaryLab <-  "Bees" 
SecondaryLab <- "Plants"
AxisLabels <- c(PrimaryLab, SecondaryLab)
FigureLabel <- "Steens Mountain CMPA" # this will need to come from the outside with the data
# colour names 
ToColour <- unique(df[, ColourBy]) # this just extracts the column of names to colour by
ToColour <- ToColour[ToColour != HighlightLab] # this could be to highlight by collector

#to be written into the js, this needs to know what format the names come in as, and have a family to colour table available
  #makes list of colours in script by bee name ----
colours <- paste0("var color = {", paste0("'", names(NamedColourVector),
                                               "':'", NamedColourVector, "'", collapse = ","),
                    "};\n") 
# setup ----
SetUp <- paste0(" src=\"vizjs.js\"\n\n\n  var svg = d3.select(\"body\")\n                 .append(\"svg\").attr(\"width\",",
                MainFigSize[1], ").attr(\"height\", ", MainFigSize[2],
                ");")

# setting label positions, this could be redone in js if you can find the str length function ----

  BoxLabPos <- (c(max(stringr::str_length(df[, 1])), 
                  max(stringr::str_length(df[, 2]))) * 1.2) + 20

max(stringr::str_length(df[, 1]))

  PercPos <- (BoxLabPos) * 6 + c(-5, 20)

LeftSidePadding = 20 + BoxLabPos[1] + IncludePerc * PercPos[1]
RightSidePadding = 20 + BoxLabPos[2] + IncludePerc * PercPos[2]
TotalNeededSidePadding = sum(20 + BoxLabPos + IncludePerc *
                               PercPos) + BarSize + IndivFigSize[1]
WPerPlot <- (MainFigSize[1] - LeftSidePadding)/mp[2]

ColPos <- rep(floor(seq(from = LeftSidePadding, by = WPerPlot,
                        length = mp[2])), mp[1])

HPerPlot <- (MainFigSize[2] - 100)/mp[1]

RowPos <- rep(floor(seq(from = 50, by = HPerPlot, length = mp[1])),
              each = mp[2])

# I believe this sends the svg requirements to viz bp and it makes it  ----
i <- 1 # regular code has a loop for each column in data i.e. diff. sites
BaseFigure <- paste0("var g", 1, " = svg.append(\"g\").attr(\"transform\",\"translate(",
                       ColPos[1], ",", RowPos[1], ")\");\n
                         var bp", i, "=viz.bP()\n 
                         .data(data)\n    
                         .value(d=>d[",i + 1, "])\n   
                         .min(", MinWidth,")\n  
                         .pad(", Pad, ")\n
                         .height(",IndivFigSize[2], ")\n
                         .width(", IndivFigSize[1], ")\n
                         .barSize(",BarSize, ")\n
                         .fill(d=>color[d.",c("primary", "secondary")[ColourBy], "])",
              paste0(".sortPrimary(sort([\"", paste0(SortPrimary, collapse = "\",\""), "\"]))\n"),
                       ".orient(\"", Orientation, "\");\n\ng", i, ".call(bp",
                       i, ");")
  
# next statement of label positions ----
# all this can be replaced, assuming size of figure is automated ----
  Labelling <- paste0("g", i, ".append(\"text\")\n
                          .attr(\"x\",", 17.5,").attr(\"y\",-8)\n
                          .style(\"text-anchor\",\"middle\")\n 
                          .text(\"",AxisLabels[1], "\") \n; \n 
                          
                          g",i, ".append(\"text\")\n 
                          .attr(\"x\",",IndivFigSize[1]-17.5,")\n
                          .attr(\"y\",-8).style(\"text-anchor\",\"middle\")\n
                          .text(\"",AxisLabels[2], "\");\n 
                          
                          g",i,".append(\"text\")\n
                          .attr(\"x\",", (IndivFigSize[1]-17.5)/2,").attr(\"y\",-25)\n  
                          .style(\"text-anchor\",\"middle\")\n
                          .attr(\"class\",\"header\")\n
                          .text(\"",FigureLabel[i], "\");")
    
    
 # mouseover definitions and label positions ----
  
# can all be replaced
  MouseOver <- paste0("\n\n g", i, ".selectAll(\".mainBars\")\n 
                        .on(\"mouseover\",mouseover)\n      
                        .on(\"mouseout\",mouseout);")
 
    BoxLabels <- paste0("\n\n g", i, ".selectAll(\".mainBars\").append(\"text\").attr(\"class\",\"label\")\n 
                          .attr(\"x\",d=>(d.part==\"primary\"? -",BoxLabPos[1], ":", BoxLabPos[2], "))\n
                          .attr(\"y\",d=>+6)\n 
                          .text(d=>d.key)\n    
                          .attr(\"text-anchor\",d=>(d.part==\"primary\"? \"end\": \"start\"));")

      BoxPerc <- paste0("\n\n g", i, ".selectAll(\".mainBars\").append(\"text\").attr(\"class\",\"lab\")\n
                          .attr(\"x\",d=>(d.part==\"primary\"? -",PercPos[1], ":", PercPos[2], "))\n
                          .attr(\"y\",d=>+6)\n 
                          .text(function(d){return (d.value)}) 
                          .attr(\"text-anchor\",d=>(d.part==\"primary\"? \"end\": \"start\")); ")
  
  
  FigureFacets <- paste0( BaseFigure, Labelling, MouseOver, BoxLabels, BoxPerc)

# this makes mouseover function ----
  # all replaceable
is <- 1:(ncol(df) - 2)
MO_funcs <- paste0("\n\nfunction mouseover(d){\n"
                   , paste0("bp",is, ".mouseover(d);\n 
  g",is, ".selectAll(\".mainBars\")\n
  .select(\".lab\")\n .text(function(d){ return (d.value)});",
                            collapse = "\n"), "\n}\n\n 
  function mouseout(d){\n",
                   paste0("bp", is, ".mouseout(d);\n 
         g",is, ".selectAll(\".mainBars\")\n  
         .select(\".lab\")\n
         .text(function(d){ return (d.value)});",
                          collapse = "\n"), "\n}")
# this puts it all together into the script, just with text ----
Output <- paste0(data, "\n function sort(sortOrder){\n
                   return function(a,b){ return d3.ascending(sortOrder.indexOf(a),sortOrder.indexOf(b)) }\n
                   }\n", colours, FigureFacets, MO_funcs, sep = "\n\n")

writeLines (Output, paste0("testOutput", ".js"))
writeLines(" .mainBars{\n    shape-rendering: auto;\n
             fill-opacity: 1;\n
             stroke-width: 0.5px;\n
             stroke: rgb(0, 0, 0);\n
             stroke-opacity: 0;\n  }\n
             .subBars{\n    
             shape-rendering:crispEdges;\n
             }\n
             .edges{\n
             stroke:none;\n 
             fill-opacity:0.3;\n 
             }\n   
             .label{\n 
             color:#000000;\n
             }",
           paste0(filename, ".css"))

# eventual tasks/ questions that need more info ----

#creating a lookup table in the javascript function, or having this come from elsewhere?



