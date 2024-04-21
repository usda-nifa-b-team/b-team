// edited bipartite script 

// see the section below var data

// eventually add function (beeData, FigureLabel, MainFigSizeX, MainFigSizeY, IndivFigSizeX, IndivFigSizeY, color, sortOrder)  { }
var data=[["Agapostemon femoratus","Angelica",2], // this will just become var data = beeData ?
["Agapostemon femoratus","Arnica",5],
["Agapostemon femoratus","Chrysothamnus",6],
["Agapostemon femoratus","Ericameria",1],
["Agapostemon femoratus","Eriogonum",1],
["Agapostemon femoratus","Helianthus",3],
["Agapostemon femoratus","Hymenoxys",4],
["Agapostemon femoratus","Potentilla",3],
["Agapostemon femoratus","Senecio",7],
["Agapostemon texanus","Helianthus",2],
["Agapostemon virescens","Geranium",1],
["Andrena","Calyptridium",3],
["Andrena","Ceanothus",1],
["Andrena","Chrysothamnus",1],
["Andrena","Hackelia",3],
["Andrena","Lomatium",2],
["Andrena","Potentilla",8],
["Andrena","Senecio",1],
["Andrena prunorum","Helianthus",1],
["Anthidium manicatum","Cirsium",1],
["Anthidium mormonum","Phacelia",1],
["Anthidium tenuiflorae","Phacelia",1],
["Anthophora bomboides","Cirsium",2],
["Anthophora bomboides","Monardella",1],
["Anthophora exigua","Stephanomeria",1],
["Anthophora terminalis","Agastache",2],
["Anthophora terminalis","Senecio",1],
["Anthophora urbana","Arnica",4],
["Anthophora urbana","Chrysothamnus",1],
["Anthophora urbana","Cirsium",6],
["Anthophora urbana","Ericameria",2],
["Anthophora urbana","Helianthus",1],
["Anthophora urbana","Phacelia",3],
["Ashmeadiella","Stephanomeria",4],
["Ashmeadiella californica","Cirsium",4],
["Ashmeadiella californica","Dieteria",1],
["Ashmeadiella californica","Erigeron",1],
["Ashmeadiella californica","Stephanomeria",4],
["Ashmeadiella difugita","Stephanomeria",24],
["Atoposmia copelandica","Phacelia",3],
["Bombus appositus","Gentiana",1],
["Bombus appositus","Lupinus",1],
["Bombus appositus","Potentilla",1],
["Bombus centralis","Agastache",3],
["Bombus centralis","Arnica",2],
["Bombus centralis","Cirsium",10],
["Bombus centralis","Holodiscus",1],
["Bombus centralis","Lupinus",1],
["Bombus centralis","Monardella",2],
["Bombus centralis","Phacelia",4],
["Bombus centralis","Potentilla",2],
["Bombus centralis","Senecio",2],
["Bombus fervidus","Angelica",2],
["Bombus fervidus","Artemisia",1],
["Bombus fervidus","Cirsium",57],
["Bombus fervidus","Helianthus",2],
["Bombus fervidus","Potentilla",1],
["Bombus fervidus","Stephanomeria",1],
["Bombus flavifrons","Cirsium",4],
["Bombus huntii","Cirsium",1],
["Bombus huntii","Helianthus",1],
["Bombus huntii","Senecio",1],
["Bombus insularis","Erigeron",1],
["Bombus insularis","Senecio",1],
["Bombus mixtus","Arnica",1],
["Bombus mixtus","Cirsium",1],
["Bombus nevadensis","Cirsium",7],
["Bombus nevadensis","Potentilla",1],
["Bombus occidentalis","Arnica",1],
["Bombus occidentalis","Ceanothus",1],
["Bombus sylvicola","Angelica",2],
["Bombus sylvicola","Cirsium",1],
["Bombus sylvicola","Erigeron",2],
["Bombus sylvicola","Hymenoxys",2],
["Bombus sylvicola","Penstemon",1],
["Bombus sylvicola","Phacelia",3],
["Bombus sylvicola","Potentilla",1],
["Bombus sylvicola","Senecio",3],
["Bombus vancouverensis","Angelica",5],
["Bombus vancouverensis","Arnica",2],
["Bombus vancouverensis","Hymenoxys",3],
["Bombus vancouverensis","Lupinus",2],
["Bombus vancouverensis","Monardella",1],
["Bombus vancouverensis","Penstemon",1],
["Bombus vancouverensis","Phacelia",2],
["Bombus vancouverensis","Potentilla",6],
["Bombus vancouverensis","Senecio",4],
["Ceratina acantha","Cirsium",1],
["Chelostoma phaceliae","Phacelia",1],
["Colletes","Chrysothamnus",1],
["Colletes","Eriogonum",1],
["Diadasia diminuta","Cirsium",1],
["Dianthidium ulkei","Stephanomeria",1],
["Dufourea","Hymenoxys",1],
["Halictus confusus","Chrysothamnus",2],
["Halictus confusus","Phacelia",1],
["Halictus farinosus","Arnica",2],
["Halictus farinosus","Potentilla",1],
["Halictus rubicundus","Chrysothamnus",1],
["Halictus rubicundus","Helianthus",1],
["Halictus tripartitus","Calochortus",2],
["Hoplitis fulgida","Angelica",6],
["Hoplitis fulgida","Hackelia",1],
["Hoplitis fulgida","Phacelia",1],
["Hoplitis louisae","Phacelia",1],
["Hoplitis louisae","Potentilla",1],
["Hoplitis plagiostoma","Potentilla",1],
["Hylaeus","Dasiphora",1],
["Hylaeus","Potentilla",2],
["Hylaeus","Senecio",3],
["Hylaeus","Taraxia",2],
["Hylaeus mesillae","Cirsium",1],
["Lasioglossum","Arnica",1],
["Lasioglossum","Dasiphora",4],
["Lasioglossum","Ericameria",6],
["Lasioglossum","Eriogonum",1],
["Lasioglossum","Helianthus",1],
["Lasioglossum","Hymenoxys",1],
["Lasioglossum","Monardella",1],
["Lasioglossum","Phacelia",1],
["Lasioglossum","Potentilla",5],
["Lasioglossum","Stephanomeria",2],
["Lasioglossum incompletum","Helianthus",1],
["Lasioglossum inconditum","Chrysothamnus",1],
["Lasioglossum inconditum","Symphyotrichum",1],
["Lasioglossum ovaliceps","Helianthus",1],
["Lasioglossum ovaliceps","Stephanomeria",1],
["Lasioglossum ruidosense","Angelica",1],
["Lasioglossum ruidosense","Arnica",1],
["Lasioglossum sisymbrii","Arnica",1],
["Lasioglossum sisymbrii","Cirsium",1],
["Lasioglossum sisymbrii","Dasiphora",4],
["Lasioglossum sisymbrii","Holodiscus",1],
["Megachile inermis","Angelica",2],
["Megachile mellitarsis","Arnica",1],
["Megachile parallela","Helianthus",1],
["Megachile perihirta","Geranium",1],
["Melissodes","Cirsium",13],
["Melissodes","Ericameria",5],
["Melissodes lupinus","Ericameria",2],
["Melissodes lupinus","Hymenoxys",1],
["Melissodes lustrus","Ericameria",1],
["Melissodes microstictus","Senecio",1],
["Melissodes rivalis","Cirsium",1],
["Melissodes rivalis","Potentilla",1],
["Nomada","Hackelia",1],
["Nomada","Lupinus",1],
["Nomada","Senecio",1],
["Osmia","Agastache",1],
["Osmia","Erythranthe",3],
["Osmia","Lupinus",2],
["Osmia","Potentilla",1],
["Osmia atrocyanea","Agastache",1],
["Osmia brevis","Agastache",1],
["Osmia brevis","Potentilla",1],
["Osmia bucephala","Hackelia",1],
["Osmia calla","Potentilla",1],
["Osmia montana","Hymenoxys",1],
["Panurginus","Dasiphora",1],
["Panurginus","Potentilla",8],
["Sphecodes","Dasiphora",1],
["Sphecodes","Eriogonum",1],
["Sphecodes","Potentilla",1],
["Stelis","Arnica",1],
["Stelis","Phacelia",1],
["Stelis","Potentilla",1]];

// this group can be updated to take external input (or for figure size base itself somehow on var data?) but currently is not ready 
var FigureLabel = "Steens Mountain CMPA"; // here will become FigureLabel = FigureLabel
var MainFigSizeX = 1100; // here will become = MainFigSizeX
var MainFigSizeY = 2000; // here will become = MainFigSizeY
var IndivFigSizeX = 400; // here will become = IndivFigSizeX
var IndivFigSizeY = 1000; // here will become = IndivFigSizeY

// this group of variables should always be the same 
var ColourBy  = 1;
var colouroption = "manual";
var BarSize = 35;
var MinWidth = 10;
var Pad = 2.25;
var IncludePerc = 1;

var Orientation = "vertical";
var PrimaryLab =  "Bees" ;
var SecondaryLab = "Plants";

var mpA = 1;
var mpB = 1;

// below are the calculations that are now done here instead of in R

var lengMaxPlant = 0;
for (i = 0; i < data.length; i++) {
    if(data[i][1].length>lengMaxPlant){ 
    lengMaxPlant = data[i][1].length;
    }
}

var lengMaxBee = 0;
for (i = 0; i < data.length; i++) {
    if(data[i][0].length>lengMaxBee){ 
    lengMaxBee = data[i][0].length;
    }
}

var BoxLabPosBee = ((lengMaxBee*1.2)+20) 
var BoxLabPosPlant = ((lengMaxPlant*1.2)+20) 

// original - BoxLabPos <- (c(max(stringr::str_length(df[, 1])), max(stringr::str_length(df[, 2]))) * 1.2) + 20

//original - PercPos <- (BoxLabPos) * 7 + c(-5, 20)
var PercPosBee = (BoxLabPosBee * 6) + -5
var PercPosPlant = (BoxLabPosPlant * 6) + 20

//original - LeftSidePadding = 20 + BoxLabPos[bee] + IncludePerc * PercPos[bee] - change include perc to 1
var LeftSidePadding = 20 + BoxLabPosBee + IncludePerc * PercPosBee; //- change include perc to 1

var RightSidePadding = 20 + BoxLabPosPlant + IncludePerc * PercPosPlant;
//original RightSidePadding = 20 + BoxLabPos[2] + IncludePerc * PercPos[2]


var TotalNeededSidePadding = 20 + BoxLabPosBee + BoxLabPosPlant + IncludePerc * (PercPosPlant + PercPosBee) + BarSize + IndivFigSizeX
//original -TotalNeededSidePadding = sum(20 + BoxLabPos + IncludePerc *  PercPos) + BarSize + IndivFigSize[1]

var WPerPlot = (MainFigSizeX - LeftSidePadding)/mpB
//original - WPerPlot <- (MainFigSize[x] - LeftSidePadding)/mp[2]

var ColPos = LeftSidePadding // simplified from original, will need work if wanting to more than one facet
//original - ColPos <- rep(floor(seq(from = LeftSidePadding, by = WPerPlot,
//                        length = mp[2])), mp[a])

var HPerPlot = (MainFigSizeY - 100)/mpA
//original - HPerPlot <- (MainFigSize[2] - 100)/mp[a]
var RowPos = 50 // simplified for now, another multiple facet problem 
//RowPos <- rep(floor(seq(from = 50, by = HPerPlot, length = mp[a])),
//              each = mp[2])


 function sort(sortOrder){

    return function(a,b){ 
      return d3.ascending(sortOrder.indexOf(a),sortOrder.indexOf(b))
      }
}

// This colour vector will need to come in from outside, making this list from the existing var data is not possible
// maybe a master list? It's always going to be the same, and didn't cause errors if an extra species is added 
//  will become var color = beeColourMaster  ??? tbd
var color = {'Agapostemon femoratus':'#984EA3','Agapostemon texanus':'#984EA3','Agapostemon virescens':'#984EA3','Andrena':'#E41A1C','Andrena prunorum':'#E41A1C','Anthidium manicatum':'#FF7F00','Anthidium mormonum':'#FF7F00','Anthidium tenuiflorae':'#FF7F00','Anthophora bomboides':'#377EB8','Anthophora exigua':'#377EB8','Anthophora terminalis':'#377EB8','Anthophora urbana':'#377EB8','Ashmeadiella':'#FF7F00','Ashmeadiella californica':'#FF7F00','Ashmeadiella difugita':'#FF7F00','Atoposmia copelandica':'#FF7F00','Bombus appositus':'#377EB8','Bombus centralis':'#377EB8','Bombus fervidus':'#377EB8','Bombus flavifrons':'#377EB8','Bombus huntii':'#377EB8','Bombus insularis':'#377EB8','Bombus mixtus':'#377EB8','Bombus nevadensis':'#377EB8','Bombus occidentalis':'#377EB8','Bombus sylvicola':'#377EB8','Bombus vancouverensis':'#377EB8','Ceratina acantha':'#377EB8','Chelostoma phaceliae':'#FF7F00','Colletes':'#4DAF4A','Diadasia diminuta':'#377EB8','Dianthidium ulkei':'#FF7F00','Dufourea':'#984EA3','Halictus confusus':'#984EA3','Halictus farinosus':'#984EA3','Halictus rubicundus':'#984EA3','Halictus tripartitus':'#984EA3','Hoplitis fulgida':'#FF7F00','Hoplitis louisae':'#FF7F00','Hoplitis plagiostoma':'#FF7F00','Hylaeus':'#4DAF4A','Hylaeus mesillae':'#4DAF4A','Lasioglossum':'#984EA3','Lasioglossum incompletum':'#984EA3','Lasioglossum inconditum':'#984EA3','Lasioglossum ovaliceps':'#984EA3','Lasioglossum ruidosense':'#984EA3','Lasioglossum sisymbrii':'#984EA3','Megachile inermis':'#FF7F00','Megachile mellitarsis':'#FF7F00','Megachile parallela':'#FF7F00','Megachile perihirta':'#FF7F00','Melissodes':'#377EB8','Melissodes lupinus':'#377EB8','Melissodes lustrus':'#377EB8','Melissodes microstictus':'#377EB8','Melissodes rivalis':'#377EB8','Nomada':'#377EB8','Osmia':'#FF7F00','Osmia atrocyanea':'#FF7F00','Osmia brevis':'#FF7F00','Osmia bucephala':'#FF7F00','Osmia calla':'#FF7F00','Osmia montana':'#FF7F00','Panurginus':'#E41A1C','Sphecodes':'#984EA3','Stelis':'#FF7F00'};


function transValue(ColPos, RowPos){ var tranString = "translate" + "(" + ColPos + "," + RowPos +")"
return tranString}

var transformColRow = transValue(ColPos, RowPos);

// function(figHeight, figWidth, translateValu = c(ColPos[i], ",", RowPos[i]))
var g1 = svg.append("g").attr("transform", test ); //replace the translate with variables - doesn't seem to work

                         var bp1=viz.bP()
 
                         .data(data)
    
                         .value(d=>d[2])
   
                         .min(MinWidth)  // standard
  
                         .pad(Pad)  // standard

                         .height(MainFigSizeY) 

                         .width(MainFigSizeX) // "^ same"

                         .barSize(BarSize) // this is standard

                         .fill(d=>color[d.primary]) // this too (assuming we are colouring by bees)
                         
                         .orient(Orientation)
                         
                         .sortPrimary(sort(["Andrena","Andrena prunorum","Panurginus","Anthophora bomboides","Anthophora exigua","Anthophora terminalis","Anthophora urbana","Bombus appositus","Bombus centralis","Bombus fervidus","Bombus flavifrons","Bombus huntii","Bombus insularis","Bombus mixtus","Bombus nevadensis","Bombus occidentalis","Bombus sylvicola","Bombus vancouverensis","Ceratina acantha","Diadasia diminuta","Melissodes","Melissodes lupinus","Melissodes lustrus","Melissodes microstictus","Melissodes rivalis","Nomada","Colletes","Hylaeus","Hylaeus mesillae","Agapostemon femoratus","Agapostemon texanus","Agapostemon virescens","Dufourea","Halictus confusus","Halictus farinosus","Halictus rubicundus","Halictus tripartitus","Lasioglossum","Lasioglossum incompletum","Lasioglossum inconditum","Lasioglossum ovaliceps","Lasioglossum ruidosense","Lasioglossum sisymbrii","Sphecodes","Anthidium manicatum","Anthidium mormonum","Anthidium tenuiflorae","Ashmeadiella","Ashmeadiella californica","Ashmeadiella difugita","Atoposmia copelandica","Chelostoma phaceliae","Dianthidium ulkei","Hoplitis fulgida","Hoplitis louisae","Hoplitis plagiostoma","Megachile inermis","Megachile mellitarsis","Megachile parallela","Megachile perihirta","Osmia","Osmia atrocyanea","Osmia brevis","Osmia bucephala","Osmia calla","Osmia montana","Stelis", "Fake Bee"])) 
  // this order is produced by sorting on bee family, then genus within family, then species (all alphabetically)
  // similarly to color, this is not possible to make given the current var data
  // will become sort (sortedBeeNames)
  
;

g1.call(bp1); g1.append("text")

                          .attr("x",17.5).attr("y",-8)

                          .style("text-anchor","middle")
 
                          .text(PrimaryLab); 
                          
                          
                          g1.append("text")
 
                          .attr("x",(MainFigSizeX-17.5)) // 382.5 comes from IndivFigSize[1]-17.5

                          .attr("y",-8).style("text-anchor","middle")

                          .text(SecondaryLab);
                          
 
                          g1.append("text")

                          .attr("x",((MainFigSizeX-17.5)/2)).attr("y",-25) // comes from (IndivFigSize[1]-17.5)/2
  
                          .style("text-anchor","middle")

                          .attr("class","header")

                          .text(FigureLabel); // comes from subset name - only applicable sometimes

 g1.selectAll(".mainBars") // no changes
 
                        .on("mouseover",mouseover)
      
                        .on("mouseout",mouseout);

 g1.selectAll(".mainBars").append("text").attr("class","label")
 
                          .attr("x",d=>(d.part=="primary"? -BoxLabPosBee:BoxLabPosPlant)) // replace with BoxLabPos[1], ":", BoxLabPos[2] - done

                          .attr("y",d=>+6)
 
                          .text(d=>d.key)
    
                          .attr("text-anchor",d=>(d.part=="primary"? "end": "start"));

 g1.selectAll(".mainBars").append("text").attr("class","lab")

                          .attr("x",d=>(d.part=="primary"? - PercPosBee:PercPosPlant)) // replace with PercPos[1], ":", PercPos[2] - done

                          .attr("y",d=>+6)
 
                          .text(function(d){return (d.value)}) 
                          .attr("text-anchor",d=>(d.part=="primary"? "end": "start")); 

// none of below needs modification either unless we get into more than one facet
function mouseover(d){
bp1.mouseover(d);
 
  g1.selectAll(".mainBars")

  .select(".lab")
 .text(function(d){ return (d.value)});
}

 
  function mouseout(d){
bp1.mouseout(d);
 
         g1.selectAll(".mainBars")
  
         .select(".lab")

         .text(function(d){ return (d.value)});
}


