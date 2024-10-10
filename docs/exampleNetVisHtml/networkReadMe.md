Documenting the network visualization process - the complete process is accomplished by makeNetVis.R

  Starts by loading gbifNameParsing.R, which itself loads intersectShapes.R

  This gets the data including monument and national forest, then uses the rgbif package to match bee and plant names to the species names in the data

It loads required packages, as well as the modified bipartite D3 script: bipartite_D3_plus_save.r

This creates a function - bipartite_D3m, which does several things. 

  The first section of it uses provided arguments and to do some basic math that calculates numerical sizes of positions of text, or figure size etc. 

  The next section applies the function BP_JS_Writer to the input, using provided arguments as well as defaults in BP_JS_Writer 
    (BP_JS_Writer will be described below, but essentially uses the numbers created above and the input data to write a custom javascript that produces code that r2d3 can turn into a visualization)

BP_JS_Writer uses the arguments passed to bipartite_d3m to write a javascript bit by bit using text.
this should be replaced eventually but currently writes the data, the order/colour into the script

  It then calls a function called loadvisjs, which loads a complex javascript, named vizjs.js here, which is available online and meant for creating sankey diagrams (should add link to it here)

  The function then calls r2d3, which is used to make a d3 visualization, using the custom javascript and visjs as a dependency

Normally this would be the end: an html wdiget is created

  This widget can be viewed in the R studio viewer, saved as an html, or in theory screenshotted using a headless browser (phantom js is suggested but doesn't work)

What I have added to bipartite_D3 is a couple functions from the package r2d3svg (https://github.com/hafen/r2d3svg)

  These allow the svg within the html to be saved as a pdf.

  This works as follows: the first function is save_d3_svg

  this function creates a temporary file on the computer, creates a chromote (chrome headless browser) session using the input figure size, 
  uses an r2d3 function (save_d3_html) to save an html, then extracts the svg, and saves it as another non temporary file

  Then save_d3_pdf calls save_d3_svg, but instead of writing an svg to file, it writes it to a temporary file, then uses rsvg_pdf to save that temporary svg as a pdf, using the css file (from bp_js_writer) 

  This save process is optional, using an if statement based on a function argument

Now makeNetVis.R has loaded all of this, it is ready to actually make visualizations using a big function that combines all the above. 

  It first adds a function - toBipd3, which converts normal species occurrence data (1 obs per row, to a data frame where the first column is bee name, the second is plant name (or in this case plant genus), 
  and the third row is the number of times that bee-plant pair was observed interacting

Then, the main function: makeNetVis 

  It takes the data as an argument, as well as options for a subset (this is currently the name of a national forest or national monument),
  plus T/F controls of whether that subset is a national monument or national forest,
  then whether the output is a static network (i.e., a pdf), or an html widget
  it also provides a fileNm option for the pdf, and has an option to set the figure title if this is not a national monument or forest

Inside makenetvis, it sets the colours for the bee families, then subsets the data if the national for./mon. arguments to subset to are present, including removing the geometry
  it then creates a named colour vector, and an ordered vector of bee names, these are used to arrange bees by family and genus, and colour them by family
  next it creates the data needed for these visualizations (using tobipd3) as described above

  what happens next depends on the static argument. (I need to have a look, but may be attempting to eliminate bp_js_writer, so differences between 2 and 4 don't matter)
  either way, these writer functions are altered to produce counts instead of percentages
  
  if an html is desired (static = False), bp_js_writer_2 is loaded, then bipartite_d3m is called, with savepdf = F
    
  If a pdf is desired (static = true), the function loads bp_js_writer_4
  it also adds save pdf = T to bipartite_d3m, and uses the provided file name

  right now it doesn't know how to scale with size, but working on it
  

  
  
  

  

  
  



  



