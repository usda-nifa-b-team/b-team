#convert occurrences to data for visualization
toBipd3 <- function(areaSubset, pol = pollinatorINatName, plnt = plantINatName){
  areaSubset %>% 
    as_tibble() %>% 
    group_by({{pol}},{{plnt}}) %>% 
    summarise(n = n()) %>% 
    as_tibble() %>% 
    dplyr::select({{pol}},{{plnt}},n)
}

#modded d3

BP_JS_Writer <- function (df, filename = "JSBP", colouroption = c("monochrome", 
                                                  "brewer", "manual")[1], HighlightLab = "Unlinked", HighlightCol = "#3366CC", 
          monoChromeCol = "rgb(56,43,61)", ColourBy = c(1, 2)[2], BrewerPalette = "Accent", 
          NamedColourVector, MainFigSize = NULL, SortPrimary = NULL, 
          SortSecondary = NULL, mp = c(1, 1), MinWidth = 10, Pad = 1, 
          IndivFigSize = c(200, 400), BarSize = 35, Orientation = c("vertical", 
                                                                    "horizontal")[1], EdgeMode = c("straight", "smooth")[2], 
          AxisLabels = NULL, FigureLabel = NULL, BoxLabPos = NULL, 
          IncludePerc = TRUE, PercentageDecimals = 0, PercPos = NULL, 
          CSS_Output_Supress = FALSE, PRINT = FALSE) 
{
  JSON <- "JSON"
  LoadVisJS()
  JSONColumn <- df %>% unite(col = "JSON", 1:2, sep = "\",\"") %>% 
    mutate(JSON = paste0("\"", JSON, "\"")) %>% unite(col = "JSON", 
                                                      sep = ",") %>% mutate(JSON = paste0("[", JSON, "]"))
  data <- paste0("var data=[", paste0(JSONColumn$JSON, collapse = ",\n"), 
                 "]\n\n\n")
  if (!(colouroption %in% c("monochrome", "brewer", "manual"))) {
    warning("Invalid Colour Option!\n            Must be one of \"monochrome\", \"brewer\"or \"manual\".\n            Defaulting to monochrome)")
    colouroption <- "monochrome"
  }
  ToColour <- unique(df[, ColourBy])
  ToColour <- ToColour[ToColour != HighlightLab]
  if (colouroption == "monochrome") {
    colours <- paste0("var color = {'", HighlightLab, "':'", 
                      HighlightCol, "',", paste0("'", ToColour, "':'", 
                                                 monoChromeCol, "'", collapse = ","), "};\n\n")
  }
  if (colouroption == "brewer") {
    colours <- paste0("var color = {'", HighlightLab, " ':'", 
                      HighlightCol, "',", paste0("'", ToColour, "':'", 
                                                 RColorBrewer::brewer.pal(n = length(ToColour), 
                                                                          name = BrewerPalette), "'", collapse = ","), 
                      "};\n\n")
  }
  if (colouroption == "manual") {
    colours <- paste0("var color = {'", HighlightLab, "':'", 
                      HighlightCol, "',", paste0("'", names(NamedColourVector), 
                                                 "':'", NamedColourVector, "'", collapse = ","), 
                      "};\n\n")
  }
  SetUp <- paste0(" src=\"vizjs.js\"\n\n\n  var svg = d3.select(\"body\")\n                 .append(\"svg\").attr(\"width\",", 
                  MainFigSize[1], ").attr(\"height\", ", MainFigSize[2], 
                  ");")
  if ((mp[2] * mp[1]) > (ncol(df) - 2)) {
    warning("Making too many facets. Are you sure mp is set ok?")
  }
  if ((mp[2] * mp[1]) < (ncol(df) - 2)) {
    warning("Making too few facets. Guessing you want 1 row")
    mp[2] <- ncol(df) - 2
  }
  if (Orientation == "horizontal" & all(mp != c(1, 1))) {
    warning("Horizontal mode not very effective with multiple facets yet")
  }
  if (is.null(MainFigSize)) {
    MainFigSize <- c(mp[2] * 700, mp[1] * 700)
  }
  if (is.null(BoxLabPos)) {
    BoxLabPos <- (c(max(stringr::str_length(df[, 1])), max(stringr::str_length(df[, 
                                                                                  2]))) * 1.1) + 20
  }
  if (is.null(PercPos)) {
    PercPos <- (BoxLabPos) * 5 + c(5, 20)
  }
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
  if (Orientation == "horizontal") {
    IndivFigSize <- rev(IndivFigSize)
  }
  FigureFacets <- ""
  for (i in 1:(ncol(df) - 2)) {
    BaseFigure <- paste0("var g", i, " = svg.append(\"g\").attr(\"transform\",\"translate(", 
                         ColPos[i], ",", RowPos[i], ")\");\n                         var bp", 
                         i, "=viz.bP()\n                         .data(data)\n                         .value(d=>d[", 
                         i + 1, "])\n                         .min(", MinWidth, 
                         ")\n                         .pad(", Pad, ")\n                         .height(", 
                         IndivFigSize[2], ")\n                         .width(", 
                         IndivFigSize[1], ")\n                         .barSize(", 
                         BarSize, ")\n                         .fill(d=>color[d.", 
                         c("primary", "secondary")[ColourBy], "])", if (EdgeMode == 
                                                                        "straight") {
                           "\n.edgeMode(\"straight\")\n"
                         }
                         else {
                           "\n"
                         }, if (!is.null(SortSecondary)) {
                           paste0(".sortSecondary(sort([\"", paste0(SortSecondary, 
                                                                    collapse = "\",\""), "\"]))\n")
                         }
                         else {
                           ""
                         }, if (!is.null(SortPrimary)) {
                           paste0(".sortPrimary(sort([\"", paste0(SortPrimary, 
                                                                  collapse = "\",\""), "\"]))\n")
                         }
                         else {
                           ""
                         }, ".orient(\"", Orientation, "\");\n\ng", i, ".call(bp", 
                         i, ");")
    if (is.null(AxisLabels)) {
      AxisLabels <- colnames(df)[1:2]
    }
    if (is.null(FigureLabel)) {
      FigureLabel <- colnames(df)[-c(1, 2)]
    }
    if (Orientation == "vertical") {
      Labelling <- paste0("g", i, ".append(\"text\")\n                        .attr(\"x\",-50).attr(\"y\",-8)\n                        .style(\"text-anchor\",\"middle\")\n                        .text(\"", 
                          AxisLabels[1], "\");\n                        g", 
                          i, ".append(\"text\")\n                        .attr(\"x\", 250)\n                        .attr(\"y\",-8).style(\"text-anchor\",\"middle\")\n                        .text(\"", 
                          AxisLabels[2], "\");\n                        g", 
                          i, ".append(\"text\")\n                        .attr(\"x\",100).attr(\"y\",-25)\n                        .style(\"text-anchor\",\"middle\")\n                        .attr(\"class\",\"header\")\n                        .text(\"", 
                          FigureLabel[i], "\");")
    }
    else {
      Labelling <- paste0("g", i, ".append(\"text\")\n                        .attr(\"x\",0).attr(\"y\",-10)\n                        .style(\"text-anchor\",\"middle\")\n                        .attr(\"class\",\"header\")\n                        .text(\"", 
                          FigureLabel[i], "\");")
    }
      if (Orientation == "vertical") {
      BoxLabels <- paste0("\n\n g", i, ".selectAll(\".mainBars\").append(\"text\").attr(\"class\",\"label\")\n                        .attr(\"x\",d=>(d.part==\"primary\"? -", 
                          BoxLabPos[1], ":", BoxLabPos[2], "))\n                        .attr(\"y\",d=>+6)\n                        .text(d=>d.key)\n                        .attr(\"text-anchor\",d=>(d.part==\"primary\"? \"end\": \"start\"));")
      if (IncludePerc) {
        BoxPerc <- paste0("\n\n g", i, ".selectAll(\".mainBars\").append(\"text\").attr(\"class\",\"perc\")\n                        .attr(\"x\",d=>(d.part==\"primary\"? -", 
                          PercPos[1], ":", PercPos[2], "))\n                        .attr(\"y\",d=>+6)\n                        .text(function(d){ return d3.format(\"0.", 
                          PercentageDecimals, "%\")(d.percent)})\n                        .attr(\"text-anchor\",d=>(d.part==\"primary\"? \"end\": \"start\")); ")
      }
      else {
        BoxPerc <- ""
      }
    }
    if (Orientation == "horizontal") {
      BoxLabels <- paste0("\n\n g", i, ".selectAll(\".mainBars\").append(\"text\").attr(\"class\",\"label\")\n                        .attr(\"x\",d=>(d.part==\"primary\"? -", 
                          0, ":", 0, "))\n                        .attr(\"y\",d=>(d.part==\"primary\"? -", 
                          BarSize, ":", BarSize, "))\n                        .text(d=>d.key)\n                        .attr(\"text-anchor\",d=>(d.part==\"primary\"? \"middle \": \"middle \"));")
      if (IncludePerc) {
        BoxPerc <- paste0("\n\n g", i, ".selectAll(\".mainBars\").append(\"text\").attr(\"class\",\"perc\")\n                        .attr(\"x\",d=>(d.part==\"primary\"? -", 
                          0, ":", 0, "))\n                        .attr(\"y\",d=>(d.part==\"primary\"? -", 
                          BarSize + 15, ":", BarSize + 15, "))\n                        .text(function(d){ return d3.format(\"0.", 
                          PercentageDecimals, "%\")(d.percent)})\n                        .attr(\"text-anchor\",d=>(d.part==\"primary\"? \"middle \": \"middle \")); ")
      }
      else {
        BoxPerc <- ""
      }
    }
    FigureFacets <- paste0(FigureFacets, "\n\n\n", BaseFigure, 
                           Labelling, BoxLabels, BoxPerc)
  }
  is <- 1:(ncol(df) - 2)
  Output <- paste0(data, "\n function sort(sortOrder){\n                    return function(a,b){ return d3.ascending(sortOrder.indexOf(a),sortOrder.indexOf(b)) }\n                  }\n", 
                   colours, FigureFacets, sep = "\n\n")
  writeLines(Output, paste0(filename, ".js"))
  writeLines(" .mainBars{\n    shape-rendering: auto;\n    fill-opacity: 1;\n    stroke-width: 0.5px;\n    stroke: rgb(0, 0, 0);\n    stroke-opacity: 0;\n  }\n    .subBars{\n    shape-rendering:crispEdges;\n    }\n    .edges{\n    stroke:none;\n    fill-opacity:0.3;\n    }\n    .label{\n    color:#000000;\n    }", 
             paste0(filename, ".css"))
  if (PRINT) {
    cat(Output)
  }
}

LoadVisJS <- function () 
{
  if (!any(list.files() == "vizjs.js")) {
    try({
      Hash <- downloader::sha_url("http://vizjs.org/viz.v1.1.0.min.js")
      if (Hash == "295e915d475fdf1c0d7db13668b6b0b526a8e910") {
        downloader::download("http://vizjs.org/viz.v1.1.0.min.js", 
                             "vizjs.js")
      }
      else {
        warning("Hash of http://vizjs.org/viz.v1.1.0.min.js doesn't match stored hash, so not downloading in case it has been modified.\n                Please save an up-to-date version of the viz.js library as vizjs.js library in the working directory")
      }
    })
  }
}


bipartiteD3 <- function (data, filename = "bipartiteD3Script", PrimaryLab = "Primary", 
          SecondaryLab = "Secondary", SiteNames = NULL, colouroption = c("monochrome", 
                                                                         "brewer", "manual")[1], HighlightLab = "Unlinked", HighlightCol = "#3366CC", 
          monoChromeCol = "rgb(56,43,61)", ColourBy = c(1, 2)[2], BrewerPalette = "Accent", 
          NamedColourVector, MainFigSize = NULL, SortPrimary = NULL, 
          SortSecondary = NULL, mp = c(1, 1), MinWidth = 10, Pad = 1, 
          IndivFigSize = c(200, 400), BarSize = 35, Orientation = c("vertical", 
                                                                    "horizontal")[1], EdgeMode = c("straight", "smooth")[2], 
          BoxLabPos = NULL, IncludePerc = TRUE, PercentageDecimals = 0, 
          PercPos = NULL, CSS_Output_Supress = FALSE, PRINT = FALSE) 
{
  df <- NULL
  if (tibble::is_tibble(data)) {
    data <- as.data.frame(data)
  }
  if (is.data.frame(data)) {
    df <- data
  }
  else {
    if (is.list(data)) {
      df <- List2DF(data, PrimaryLab, SecondaryLab, SiteNames)
    }
    else {
      dimensions <- length(dim(data))
      if (dimensions == 3) {
        df <- Array2DF(data, PrimaryLab, SecondaryLab, 
                       SiteNames)
      }
      if (dimensions == 2) {
        df <- Matrix2DF(data, PrimaryLab, SecondaryLab, 
                        SiteNames)
      }
    }
  }
  if (is.null(df)) {
    stop("invalid data input. Valid forms are data frame, bipartite style matrix, list or array")
  }
  if ((mp[2] * mp[1]) > (ncol(df) - 2)) {
    warning("Making too many facets. Are you sure mp is set ok?")
  }
  if ((mp[2] * mp[1]) < (ncol(df) - 2)) {
    warning("Making too few facets. Guessing you want 1 row")
    mp[2] <- ncol(df) - 2
  }
  if (is.null(MainFigSize)) {
    MainFigSize <- c(mp[2] * 700, mp[1] * 700)
  }
  BP_JS_Writer(df = df, filename = filename, colouroption = colouroption, 
               HighlightLab = HighlightLab, HighlightCol = HighlightCol, 
               monoChromeCol = monoChromeCol, ColourBy = ColourBy, BrewerPalette = BrewerPalette, 
               NamedColourVector = NamedColourVector, MainFigSize = MainFigSize, 
               SortPrimary = SortPrimary, SortSecondary = SortSecondary, 
               mp = mp, MinWidth = MinWidth, Pad = Pad, IndivFigSize = IndivFigSize, 
               BarSize = BarSize, Orientation = Orientation, EdgeMode = EdgeMode, 
               AxisLabels = c(PrimaryLab, SecondaryLab), FigureLabel = SiteNames, 
               BoxLabPos = BoxLabPos, IncludePerc = IncludePerc, PercentageDecimals = PercentageDecimals, 
               PercPos = PercPos, CSS_Output_Supress = CSS_Output_Supress, 
               PRINT = PRINT)
  LoadVisJS()
  r2d3::r2d3(data = NA, script = paste0(filename, ".js"), height = MainFigSize[2], 
             width = MainFigSize[1], d3_version = "5", dependencies = "vizjs.js")
}
 

