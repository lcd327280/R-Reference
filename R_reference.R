##==============================================================================##
## R: 3.1.2
## Author : Chade
## Create data: 07/12/2014
## Description: Put all notes and functions that I have been used as reference
##==============================================================================##


################## Set up library path and enable packages ############
#setup library path
.libPaths("C:\\Users\\cli388\\Documents\\R Lib")

#setup workbook path
setwd("C:\\Users\\cli388\\Documents\\R projects")

################## Enable packages ############
library(ggplot2)
library(RODBC)
library(gcookbook)
library(plyr)
library(hexbin)
library(reshape2)
library(scales)
library(MASS)
library(grid)
library(RColorBrewer)
library(corrplot)
library(igraph)
library(rgl)
library(maps)
library(vcd)
library(mapproj)
library(maptools)
library(rgdal)
library(extrafont)
library(riverplot)
library(grDevices)
library(rmarkdown)
library(gmodels)
library(dplyr)
library(sqldf)
library(XLConnect)
library(networkD3)
library(curl)
library(knitr)
library(R2HTML)
library(googleVis)
library(akima)
library(chron)
library(mcmc)
library(spdep)
library(spatstat)
library(tree)
library(devtools)
library(arcdiagram)
library(tm)
library(jsonlite)
library(downloader)
library(Quandl)
library(sunburstR)
library(tidyr)
library(hash)
library(zoom)
library(fImport)
library(data.table)
library(h2o)
library(rscproxy)
library(car)
################## install packages #########################
#install.packages(c("ggplot2","gcookbook", "plyr", "hexbin", "corrplot", "igraph", "rgl", "maps", "vcd", "mapproj", "maptools", "rgdal", "extrafont", "riverplot", "rmarkdown", "gmodels", "dplyr", "RODBC", "sqldf", "XLConnect", "networkD3", "curl", "R2HTML", "googleVis"))

#install packages individuelly
install.packages("ggplot2")
install.packages("gcookbook")
install.packages("plyr")
install.packages("hexbin")
install.packages("reshape2")
install.packages("corrplot")
install.packages("igraph")
install.packages("rgl")
install.packages("maps")
install.packages("vcd")
install.packages("mapproj")
install.packages("maptools")
install.packages("rgdal")
install.packages("extrafont")
install.packages("riverplot")
install.packages("rmarkdown")
install.packages("gmodels")
install.packages("dplyr")
install.packages("RODBC")
install.packages("sqldf")
install.packages("XLConnect")
install.packages("networkD3")
install.packages("curl")
install.packages("R2HTML")
install.packages("googleVis")
install.packages("akima")
install.packages("chron")
install.packages("mcmc")
install.packages("spdep")
install.packages("spatstat")
install.packages("tree")
install.packages("devtools")
install_github('arcdiagram', username='gastonstat')
install.packages("tm")
install.packages("jsonlite")
install.packages("downloader")
install.packages("Quandl")
devtools::install_github("timelyportfolio/sunburstR")
install.packages("tidyr")
install.packages("hash")
install.packages("zoom")
install.packages("playwith")
install.packages("akima")
install.packages("chron")
install.packages("Ime4")
install.packages("mcmc")
install.packages("odesolve")
install.packages("spdep")
install.packages("spatstat")
install.packages("tree")
install.packages("fImport")
install.packages("data.table")
install.packages("h2o")
install.packages("rscproxy")
install.packages("car")
#install the package if not installed
if (!require("RODBC")) install.packages("RODBC")

################## Loop with out break and print error ####
for (i in 1:10) {
  tryCatch({
    print(i)
    if (i==7) stop("Enough!")
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}
################## create folder if not exist ##################

mkdirs <- function(fp) {
  if(!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp)
  }
} 

mkdirs("C:\\Users\\cli388\\Desktop\\RCreatedirtest")
################## package RColorBrewer ################
#Color from ColorBrewer
display.brewer.all()
################## package corrplot ###########
#correlation matrix
corrplot(mcor)
#Correlation matrix with colored squares and black, rotated labels
corrplot(mcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45)
################## Font of package extrafont ################
# Find and save information about fonts installed on your system
font_import()
# List the fonts
fonts()
# Register the fonts with R
loadfonts()
################## convert column and rows ##################
df <- data.frame(t(df))
################## Gather columns into key-value pairs. ################## 
library(dplyr)
# From http://stackoverflow.com/questions/1181060
stocks <- data.frame(
  time = as.Date('2009-01-01') + 0:9,
  X = rnorm(10, 0, 1),
  Y = rnorm(10, 0, 2),
  Z = rnorm(10, 0, 4)
)

gather(stocks, stock, price, -time)
stocks %>% gather(stock, price, -time)
################## RODBC connection and load data from database ##################
  require(RODBC)
  #Connect from odbc adminstrator
  channel <- odbcDriverConnect(connection = "dsn=630472-OXDB11;
                               Database=CarlyleNightly;
                               uid=pwcamtax\\cli388;
                               Trusted_Connection=yes;")
  
  #Connect directly to database
  channel <- odbcDriverConnect(connection = "Driver={SQL Server Native Client 11.0};
                                             Server=630472-OXDB11;
                                             Database=DEV-TRACKinternational-Nils;
                                             uid=pwcamtax\\cli388;
                                             Trusted_Connection=yes;")
  SQLstring <- "SELECT *  FROM [DEV-TRACKinternational-Nils].[dbo].[rptComprehensiveK1]
                Where EntityID = 4264"
  
  ex.query <- sqlQuery(channel,SQLstring)
  
  odbcClose(channel)
  dim(ex.queru)
  
################## Riverplot #####################
  #enable packages
  library(riverplot)
  library(grDevices)
  #riverplot example 
  plot(riverplot.example())
#basic command

#Color interpolation
  colorRampPaletteAlpha
  colorRampAlpha
  #Description
  #These functions are replacements for colorRamp and colorRampPalette from the package grDevices,
  #the only difference being that they also interpolate the alpha channel (i.e. transparency).
  #Usage
  #colorRampPaletteAlpha(colors, ...)
  #colorRampAlpha(colors, bias = 1, interpolate = c("linear", "spline"))
  #Examples
  colorRampPaletteAlpha( c( "#FF000033", "#00FF0099" ) )( 5 )

#Draw a curved segment
  curveseg
  #Description
  #Draws a curved segment from point (x0,y0) to (x1,y1). The segment is a framgent of a sinusoid,
  #has a defined width and can either have a single color or a color gradient.
  #Usage
  #curveseg(x0, x1, y0, y1, width = 1, nsteps = 50, col = "#ffcc0066",
  #         grad = NULL, lty = 1, form = c("sin", "line"))
  
  #Examples
  # a DNA strand
  plot.new()
  par( usr= c( 0, 4, -2.5, 2.5 ) )
  w <- 0.4
  cols <- c( "blue", "green" )
  init <- c( -0.8, -0.5 )
  pos <- c( 1, -1 )
  step <- 0.5
  for( i in rep( rep( c( 1, 2 ), each= 2 ), 5 ) ) {
    curveseg( init[i], init[i] + step, pos[1], pos[2], width= w, col= cols[i] )
    init[i] <- init[i] + step
    pos <- pos * -1
  }
  
# Create a new riverplot object
  #makeRiver
  #Description
  #Create a new riverplot object
  #Usage
  #makeRiver(nodes, edges, node_labels = NULL, node_xpos = NULL,
  #          node_ypos = NULL, node_styles = NULL, edge_styles = NULL,
  #          default_style = NULL)
  
# Structure of the riverplot objects
  # nodes   A data frame specifying the nodes, containing at least the columns "ID" and "x" (horizontal
  #         position of the node). Optionally, it can also contain columns "labels" (the labels to display)
  #         and "y" (vertical position of the node on the plot)#
  # edges   A data frame specifying the edges and graph topology, containing at least the columns "ID",
  #         "N1", "N2" and "Value", specifying, respectively, the ID of the edge, the parent node, the child
  #         node, and the size of the edge.
  # styles  A named list of styles. Names of this list are the node or edge IDs. Values are styles
  #         specifying the style of the given node or edge (see below).
  
  #Examples
  nodes <- c( LETTERS[1:3] )
  edges <- list( A= list( C= 5 ), B= list( C= 10 ) )
  r <- makeRiver( nodes, edges, node_xpos= c( 1,1,2 ),
                  node_labels= c( A= "Node A", B= "Node B", C= "Node C" ),
                  node_styles= list( A= list( col= "yellow" )) )
  plot( r )
  
  
  # equivalent form:
  nodes <- data.frame( ID= LETTERS[1:3],
                       x= c( 1, 1, 2 ),
                       col= c( "yellow", NA, NA ),
                       labels= c( "Node A", "Node B", "Node C" ),
                       stringsAsFactors= FALSE )
  r <- makeRiver( nodes, edges )
  plot( r )
  # all nodes but "A" will be red:
  r <- makeRiver( nodes, edges, default_style= list( col="red" ) )
  plot( r )
  # overwrite the node information from "nodes":
  r <- makeRiver( nodes, edges, node_styles= list( A=list( col="red" ) ) )
  plot( r )
  
#Minard Napoleon Russian campaign data
  minard
  #Description
  #The data set used by Charles Joseph Minard to generate the famous graph. The example below
  #shows how to recreate the main panel of the graph using riverplot from the provided data.
  #First, node and edge data frames must get new column names (see makeRiver function for details).
  #Then, based on the direction of the Napoleon army, style information (right and left edge color style
  #                                                                      for each node) is entered in the nodes variable. Then, a riverplot object is generated from the nodes
  #and edges data frames.
  #To use the same color coding as Minard, the direction variable is converted to color codes in the col
  #column of the edges object.
  #Finally, a plot is created using lty=1 and a style in which nodes are not shown, and the edges are
  #straight (like in the original Minard plot) rather than curved.
  
  #Examples
  data( minard )
  nodes <- minard$nodes
  edges <- minard$edges
  colnames( nodes ) <- c( "ID", "x", "y" )
  colnames( edges ) <- c( "N1", "N2", "Value", "direction" )
  # color the edges by troop movement direction
  edges$col <- c( "#e5cbaa", "black" )[ factor( edges$direction ) ]
  # color edges by their color rather than by gradient between the nodes
  edges$edgecol <- "col"
  # generate the riverplot object and a style
  river <- makeRiver( nodes, edges )
  style <- list( edgestyle= "straight", nodestyle= "invisible" )
  #plot the generated object
  plot( river, lty= 1, default_style= style )
  # Add cities
  with( minard$cities, points( Longitude, Latitude, pch= 19 ) )
  with( minard$cities, text( Longitude, Latitude, Name, adj= c( 0, 0 ) ) )
  
#Create a Sankey plot
  plot.riverplot
  #Description
  #Create a Sankey plot
  #Usage
  ### S3 method for class 'riverplot'
  #plot(x, ...)
  #riverplot(x, lty = 0, srt = NULL, default_style = NULL, gravity = "top",
  #          node_margin = 0.1, nodewidth = 1.5, plot_area = 0.5, nsteps = 50,
  #          add_mid_points = NULL, yscale = "auto")
  
  #Examples
  x <- riverplot.example()
  plot(x)
  plot(x, srt=90, lty=1)
  
#Riverplot styles
  riverplot-styles
  #Description
  #Riverplot styles
  #Usage
  #default.style()
  #updateRiverplotStyle(style, master)
  #Examples
  # To view the default style specification, type
  default.style()
  ex <- riverplot.example()
  ds <- default.style()
  plot( ex, default_style= ds )
  # nodes with unspecified style will now be semi-transparent red:
  ds[["col"]] <- "#FF000099"
  plot( ex, default_style= ds )
  
#Generate an example for riverplot
  riverplot.example
  #Description
  #Generate an example for riverplot
  #Usage
  #riverplot.example()
  #Details
  #The plotting functions in the riverplot package work on an object of the riverplot class. This function
  #returns an object of the riverplot class to demonstrate how such an object (which is actually a simple list) can be created.
  ex <- riverplot.example()
  plot(ex)
  
  
  
################## Riverplot sankey ###########
  #lib
  
  library(riverplot)
  help("riverplot")
  #create a more complex test dataset
  df <- data.frame(cbind(src=sample(1:6,500,replace=TRUE),target=sample(1:6,500,replace=TRUE)))
  
  #edges as a coerced dataset from a table
  edges <- data.frame(table(df$src,df$target),stringsAsFactors=F)
  edges$Value <- as.numeric(edges$Value)
  colnames(edges) <-c("N1","N2","Value")
  
  #define nodes, dont ever do a cbind for that
  nodes <- data.frame(ID=c(1:6),x=c(1,1,2,3,3,3),labels=c("label1","label2","label3","label4","label5","label6"))                 
  
  #check that its just factors and numerics
  str(edges)
  str(nodes)
  
  #create the riverplot object
  river <- makeRiver(nodes, edges)
  
  #plot it with basic grey
  riverplot(river)
  
  #edit the styles
  node_colors <- list("1"=list(col="blue"),"2"=list(col="green"))
  
  river <- makeRiver(nodes, edges, node_styles=node_colors )
  riverplot(river,srt = NULL, default_style = NULL, gravity = "top",node_margin = 0.1, nodewidth = 2, plot_area = 0.8, nsteps = 100,add_mid_points = NULL, yscale = "auto")
################## R markdown ##############
  
  #website:rmarkdown.rstudio.com
  
  #When you click the **Knit** button a document will be generated that includes both content 
  #as well as the output of any embedded R code chunks within the document. 
  
  #You can embed an R code chunk like this:
  #  
  #  ```{r}
  #summary(cars)
  #```
  
  #You can also embed plots, for example:
  #  
  #  ```{r, echo=FALSE}
  #plot(cars)
  #```
  
  #Note that the `echo = FALSE` parameter was added to the code chunk to 
  #prevent printing of the R code that generated the plot.
  
  #example : C:\Users\cli388\Documents\R projects\RmarkDownExample.Rmd
  
  
  
  
################## Network D3 ##########
  #Network
  library(networkD3)
  data(MisLinks, MisNodes)
  head(MisLinks,5)
  head(MisNodes,5)
  forceNetwork(Links = MisLinks, Nodes = MisNodes, Source = "source",
               Target = "target", Value = "value", NodeID = "name",
               Group = "group", opacity = 0.4)
  help("forceNetwork")
  
  #sankey
  URL <- paste0(
    "https://cdn.rawgit.com/christophergandrud/networkD3/",
    "master/JSONdata/energy.json")
  Energy <- jsonlite::fromJSON(URL)
  sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
                Target = "target", Value = "value", NodeID = "name",
                 fontSize = 12, nodeWidth = 30)
  help("sankeyNetwork")
  help("sankeyNetworkOutput")
  
  #radialNetwork
  URL <- paste0(
    "https://cdn.rawgit.com/christophergandrud/networkD3/",
    "master/JSONdata//flare.json")
  
  ## Convert to list format
  Flare <- jsonlite::fromJSON(URL, simplifyDataFrame = FALSE)
  
  # Use subset of data for more readable diagram
  Flare$children = Flare$children[1:3]
  radialNetwork(List = Flare, fontSize = 10, opacity = 0.9)
  
  #diagonalNetwork(tree map)
  diagonalNetwork(List = Flare, fontSize = 10, opacity = 0.9)
  
  #dendroNetwork (angel tree map)
  hc <- hclust(dist(USArrests), "ave")
  
  dendroNetwork(hc, height = 600)
  
################## Layer ###########
  #website:https://github.com/yixuan/Layer
  help(layer)
  
layer(1, CNK1.sankey.pos)
################## leaflet ##################

install.packages('leaflet')
library(leaflet)
help("leaflet")

m = leaflet() %>% addTiles()
m  # a map with the default OSM tile layer
# set bounds
m %>% fitBounds(0, 40, 10, 50)

# move the center to Snedecor Hall
m = m %>% setView(-93.65, 42.0285, zoom = 17)
m

# popup
m %>% addPopups(-93.65, 42.0285, 'Here is the <b>Department of Statistics</b>, ISU')
rand_lng = function(n = 10) rnorm(n, -93.65, .01)
rand_lat = function(n = 10) rnorm(n, 42.0285, .01)

# use automatic bounds derived from lng/lat data
m = m %>% clearBounds()

# popup
m %>% addPopups(rand_lng(), rand_lat(), 'Random popups')

# marker
m %>% addMarkers(rand_lng(), rand_lat())
m %>% addMarkers(
  rand_lng(), rand_lat(), popup = paste('A random letter', sample(LETTERS, 10))
)

Rlogo = file.path(R.home('doc'), 'html', 'logo.jpg')
m %>% addMarkers(
  174.7690922, -36.8523071, icon = list(
    iconUrl = Rlogo, iconSize = c(100, 76)
  ), popup = 'R was born here!'
)

m %>% addMarkers(rnorm(30, 175), rnorm(30, -37), icon = list(
  iconUrl = Rlogo, iconSize = c(25, 19)
))

m %>% addMarkers(
  c(-71.0382679, -122.1217866), c(42.3489054, 47.6763144), icon = list(
    iconUrl = 'http://www.rstudio.com/wp-content/uploads/2014/03/blue-125.png'
  ), popup = c('RStudio @ Boston', 'RStudio @ Seattle')
)


# circle (units in metres)
m %>% addCircles(rand_lng(50), rand_lat(50), radius = runif(50, 50, 150))

# circle marker (units in pixels)
m %>% addCircleMarkers(rand_lng(50), rand_lat(50), color = '#ff0000')
m %>% addCircleMarkers(rand_lng(100), rand_lat(100), radius = runif(100, 5, 15))

# rectangle
m %>% addRectangles(
  rand_lng(), rand_lat(), rand_lng(), rand_lat(),
  color = 'red', fill = FALSE, dashArray = '5,5', weight = 3
)

# polyline
m %>% addPolylines(rand_lng(50), rand_lat(50))

# polygon
m %>% addPolygons(rand_lng(), rand_lat(), layerId = 'foo')

# geoJSON
seattle_geojson = list(
  type = "Feature",
  geometry = list(
    type = "MultiPolygon",
    coordinates = list(list(list(
      c(-122.36075812146,  47.6759920119894),
      c(-122.360781646764, 47.6668890126755),
      c(-122.360782108665,  47.6614990696722),
      c(-122.366199035722, 47.6614990696722),
      c(-122.366199035722,  47.6592874248973),
      c(-122.364582509469, 47.6576254522105),
      c(-122.363887331445,  47.6569107302038),
      c(-122.360865528129, 47.6538418253251),
      c(-122.360866157644,  47.6535254473167),
      c(-122.360866581103, 47.6533126275176),
      c(-122.362526540691,  47.6541872926348),
      c(-122.364442114483, 47.6551892850798),
      c(-122.366077719797,  47.6560733960606),
      c(-122.368818463838, 47.6579742346694),
      c(-122.370115159943,  47.6588730808334),
      c(-122.372295967029, 47.6604350102328),
      c(-122.37381369088,  47.660582362063),
      c(-122.375522972109, 47.6606413027949),
      c(-122.376079703095,  47.6608793094619),
      c(-122.376206315662, 47.6609242364243),
      c(-122.377610811371,  47.6606160735197),
      c(-122.379857378879, 47.6610306942278),
      c(-122.382454873022,  47.6627496239169),
      c(-122.385357955057, 47.6638573778241),
      c(-122.386007328104,  47.6640865692306),
      c(-122.387186331506, 47.6654326177161),
      c(-122.387802656231,  47.6661492860294),
      c(-122.388108244121, 47.6664548739202),
      c(-122.389177800763,  47.6663784774359),
      c(-122.390582858689, 47.6665072251861),
      c(-122.390793942299,  47.6659699214511),
      c(-122.391507906234, 47.6659200946229),
      c(-122.392883050767,  47.6664166747017),
      c(-122.392847210144, 47.6678696739431),
      c(-122.392904778401,  47.6709016021624),
      c(-122.39296705153, 47.6732047491624),
      c(-122.393000803496,  47.6759322346303),
      c(-122.37666945305, 47.6759896300663),
      c(-122.376486363943,  47.6759891899754),
      c(-122.366078869215, 47.6759641734893),
      c(-122.36075812146,  47.6759920119894)
    )))
  ),
  properties = list(
    name = "Ballard",
    population = 48000,
    # You can inline styles if you want
    style = list(
      fillColor = "yellow",
      weight = 2,
      color = "#000000"
    )
  ),
  id = "ballard"
)
m %>% setView(-122.36075812146, 47.6759920119894, zoom = 13) %>% addGeoJSON(seattle_geojson)


# use the Dark Matter layer from CartoDB
leaflet() %>% addTiles('http://{s}.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png',
                       attribution = paste(
                         '&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors',
                         '&copy; <a href="http://cartodb.com/attributions">CartoDB</a>'
                       )
) %>% setView(-122.36, 47.67, zoom = 10)

# provide a data frame to leaflet()
categories = LETTERS[1:10]
df = data.frame(
  lat = rand_lat(100), lng = rand_lng(100), size = runif(100, 5, 20),
  category = factor(sample(categories, 100, replace = TRUE), levels = categories),
  value = rnorm(100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(~lng, ~lat, radius = ~size)
m %>% addCircleMarkers(~lng, ~lat, radius = runif(100, 4, 10), color = c('red'))

# Discrete colors using the "RdYlBu" colorbrewer palette, mapped to categories
RdYlBu = colorFactor("RdYlBu", domain = categories)
m %>% addCircleMarkers(~lng, ~lat, radius = ~size,
                       color = ~RdYlBu(category), fillOpacity = 0.5)

# Continuous colors using the "Greens" colorbrewer palette, mapped to value
greens = colorNumeric("Greens", domain = NULL)
m %>% addCircleMarkers(~lng, ~lat, radius = ~size,
                       color = ~greens(value), fillOpacity = 0.5)
################## arcdiagram reference ##########
# location of 'gml' file
mis_file = "C:\\Users\\cli388\\Documents\\R Lib\\arcdiagram\\exampleData.txt"
# read 'gml' file
mis_graph = read.graph(mis_file, format="gml")

# get edgelist
edgelist = get.edgelist(mis_graph)

# get vertex labels
vlabels = get.vertex.attribute(mis_graph, "label")

# get vertex groups
vgroups = get.vertex.attribute(mis_graph, "group")

# get vertex fill color
vfill = get.vertex.attribute(mis_graph, "fill")

# get vertex border color
vborders = get.vertex.attribute(mis_graph, "border")

# get vertex degree
degrees = degree(mis_graph)

# get edges value
values = get.edge.attribute(mis_graph, "value")

# load reshape
library(reshape)

# data frame with vgroups, degree, vlabels and ind
x = data.frame(vgroups, degrees, vlabels, ind=1:vcount(mis_graph))

# arranging by vgroups and degrees
y = arrange(x, desc(vgroups), desc(degrees))

# get ordering 'ind'
new_ord = y$ind

# plot arc diagram
arcplot(edgelist, ordering=new_ord, labels=vlabels, cex.labels=0.8,
        show.nodes=TRUE, col.nodes=vborders, bg.nodes=vfill,
        cex.nodes = log(degrees)+0.5, pch.nodes=21,
        lwd.nodes = 2, line=-0.5,
        col.arcs = hsv(0, 0, 0.2, 0.25), lwd.arcs = 1.5 * values)
################## select a list from another list ######
Final.table <- Tar.table[Org.Table$Column1 %in% list.Table | Org.Table$Column2 %in% list.Table,]
################## Quandl (the database) ########
 #website: https://www.quandl.com/
library(Quandl)
 #example: get oil prices from OPEC
mydata = Quandl("OPEC/ORB")
# sign the key
Quandl.api_key('TyAZyRfCgvfckewQK8uo') # got this by regist account
# get the first data
# collapse= "daily", "weekly", "monthly", "quarterly", "annual"
CNY <- data.frame(FromCountry = "CNY" ,Quandl("CURRFX/USDCNY", collapse="daily", start_date="2015-01-01"))
CNY <- NULL
################## quantmod (the database) ########
if (!require("quantmod")) install.packages("quantmod")
library(quantmod)
help("quantmod")
help("getSymbols")

getSymbols("USD/CNY",src="oanda",from="2012-01-01") 
################## error handling #############
#!/usr/bin/env Rscript
# tryCatch.Rscript -- experiments with tryCatch

# Get any arguments
arguments <- commandArgs(trailingOnly=TRUE)
a <- arguments[1]

# Define a division function that can issue warnings and errors
myDivide <- function(d, a) {
  if (a == 'warning') {
    return_value <- 'myDivide warning result'
    warning("myDivide warning message")
  } else if (a == 'error') {
    return_value <- 'myDivide error result'
    stop("myDivide error message")
  } else {
    return_value = d / as.numeric(a)
  }
  return(return_value)
}

# Evalute the desired series of expressions inside of tryCatch
result <- tryCatch({
  
  b <- 2
  c <- b^2
  d <- c+2
  if (a == 'suppress-warnings') {
    e <- suppressWarnings(myDivide(d,a))
  } else {
    e <- myDivide(d,a) # 6/a
  }
  f <- e + 100
  
}, warning = function(war) {
  
  # warning handler picks up where error was generated
  print(paste("MY_WARNING:  ",war))
  b <- "changing 'b' inside the warning handler has no effect"
  e <- myDivide(d,0.1) # =60
  f <- e + 100
  return(f)
  
}, error = function(err) {
  
  # error handler picks up where error was generated
  print(paste("MY_ERROR:� ",err))
  b <- "changing 'b' inside the error handler has no effect"
  e <- myDivide(d,0.01) # =600
  f <- e + 100
  return(f)
  
}, finally = {
  
  print(paste("a =",a))
  print(paste("b =",b))
  print(paste("c =",c))
  print(paste("d =",d))
  # NOTE:� Finally is evaluated in the context of of the inital
  # NOTE:� tryCatch block and 'e' will not exist if a warning
  # NOTE:� or error occurred.
  #print(paste("e =",e))
  
}) # END tryCatch

print(paste("result =",result))
################## Sunburstr #####
 #website: https://github.com/timelyportfolio/sunburstR
#install 
devtools::install_github.
devtools::install_github("timelyportfolio/sunburstR")

 #example
library(sunburstR)

# read in sample visit-sequences.csv data provided in source
#   https://gist.github.com/kerryrodden/7090426#file-visit-sequences-csv
sequence_data <- read.csv("https://gist.githubusercontent.com/kerryrodden/7090426/raw/ad00fcf422541f19b70af5a8a4c5e1460254e6be/visit-sequences.csv"
         ,header=F
         ,stringsAsFactors = FALSE)
# head(sequence_data,5)
sunburst(sequence_data)
################## ggsunburst (fail to install rPython) ##########
#website http://genome.crg.es/~didac/ggsunburst/ggsunburst.html


 #require Python 2.7 or higher

 #install rPython for win
# https://github.com/cjgb/rPython-win
install("C:/Users/cli388/Documents/R books/R packages/rPython")


if (!require("ggplot2")) install.packages("ggplot2")
#if (!require("rPython")) install.packages("rPython") # for linux system
install.packages("http://genome.crg.es/~didac/ggsunburst/ggsunburst_0.0.2.tar.gz", repos=NULL, type="source")
library(ggsunburst)
################## remove empty rows ########
data[rowSums(is.na(data)) != ncol(data),]
#same as 
data <- data[complete.cases(data),]
################## remove rows with NA ######
data[rowSums(is.na(data)) == 0,]
################## clean number function ######
clean <- function(ttt){
  as.numeric( gsub('[^a-zA-Z0-9.]', '', ttt))
}

help(gsub)
#deploy the function to dataset
data <- data.frame(sapply(data, clean))
################## tidyr ####
library(tidyr)
library(dplyr)

messy <- data.frame(
  name = c("Wilbur", "Petunia", "Gregory"),
  a = c(67, 80, 64),
  b = c(56, 90, 50)
)
messy

messy %>%  gather(drug, heartrate, a:b)
################## hash #####
h <- hash(dat)
keys(h)
names(h)
################## Get different value between two column ####
DiffValue <- data.frame(TargetTable[!TargetTable$targetCol %in% listTable$listCol,])
################## Get overlap value between two column ####
OverLapValue <- data.frame(TargetTable[TargetTable$targetCol %in% listTable$listCol,])
################## Get the difference between two column ####
table <- data.frame(diff(table$column1,table$column2))
################## concat in R #########
table <- data.frame(paste(table, sep = ","))
table <- data.frame(paste(table, collapse = ","))
help(paste)
################## Zoom function on plot ####
zoom(graph)
move.to.click.zoom(graph)
navigation.zoom(graph)
################## Playwith (allow play with plot) ####
autoplay(on = NA, lattice.on = on, base.on = on, grid.on = on, ask = FALSE)
 #playSelectData Let playwith user select data points
if (interactive()) {
  library(lattice)
  playwith(xyplot(Sepal.Width ~ Petal.Width | Species, data = iris))
  playSelectData()
}

 #playwith An interactive plot GUI
if (interactive()) {
  options(device.ask.default = FALSE)
  ## Scatterplot (Lattice graphics).
  ## Labels are taken from rownames of data.
  ## Right-click on the plot to identify points.
  playwith(xyplot(Income ~ log(Population / Area),
                  data = data.frame(state.x77), groups = state.region,
                  type = c("p", "smooth"), span = 1, auto.key = TRUE,
                  xlab = "Population density, 1974 (log scale)",
                  ylab = "Income per capita, 1974"))
  ## Scatterplot (base graphics); similar.
  ## Note that label style can be set from a menu item.
  urbAss <- USArrests[,c("UrbanPop", "Assault")]
  playwith(plot(urbAss, panel.first = lines(lowess(urbAss)),
                col = "blue", main = "Assault vs urbanisation",
                xlab = "Percent urban population, 1973",
                ylab = "Assault arrests per 100k, 1973"))
  ## Time series plot (Lattice).
  ## Date-time range can be entered directly in "time mode"
  ## (supports numeric, Date, POSIXct, yearmon and yearqtr).
  ## Click and drag to zoom in, holding Shift to constrain;
  ## or use the scrollbar to move along the x-axis.
  library(zoo)
  playwith(xyplot(sunspots ~ yearmon(time(sunspots)),
                  xlim = c(1900, 1930), type = "l"),
           time.mode = TRUE)
  ## Time series plot (base graphics); similar.
  ## Custom labels are passed directly to playwith.
  tt <- time(treering)
  treeyears <- paste(abs(tt) + (tt <= 0),
                     ifelse(tt > 0, "CE", "BCE"))
  playwith(plot(treering, xlim = c(1000, 1300)),
           labels = treeyears, time.mode = TRUE)
  ## Multi-panel Lattice plot.
  ## Need subscripts = TRUE to correctly identify points.
  ## Scales are "same" so zooming applies to all panels.
  ## Use the 'Panel' tool to expand a single panel, then use
  ## the vertical scrollbar to change pages.
  Depth <- equal.count(quakes$depth, number = 3, overlap = 0.1)
  playwith(xyplot(lat ~ long | Depth, data = quakes,
                  subscripts = TRUE, aspect = "iso", pch = ".", cex = 2),
           labels = paste("mag", quakes$mag))
  ## Spin and brush for a 3D Lattice plot.
  ## Drag on the plot to rotate in 3D (can be confusing).
  ## Brushing is linked to the previous xyplot (if still open).
  ## Note, brushing 'cloud' requires a recent version of Lattice.
  playwith(cloud(-depth ~ long * lat, quakes, zlab = "altitude"),
           new = TRUE, link.to = playDevCur(), click.mode = "Brush")
  ## Set brushed points according to a logical condition.
  playSetIDs(value = which(quakes$mag >= 6))
  ## Interactive control of a parameter with a slider.
  xx <- rnorm(50)
  playwith(plot(density(xx, bw = bandwidth), panel.last = rug(xx)),
           parameters = list(bandwidth = seq(0.05, 1, by = 0.01)))
  ## The same with a spinbutton (use I() to force spinbutton).
  ## Initial value is set as the first in the vector of values.
  ## This also shows a combobox for selecting text options.
  xx <- rnorm(50)
  kernels <- c("gaussian", "epanechnikov", "rectangular",
               "triangular", "biweight", "cosine", "optcosine")
  playwith(plot(density(xx, bw = bandwidth, kern = kernel), lty = lty),
           parameters = list(bandwidth = I(c(0.1, 1:50/50)),
                             kernel = kernels, lty = 1:6))
  ## More parameters (logical, numeric, text).
  playwith(stripplot(yield ~ site, data = barley,
                     jitter = TRUE, type = c("p", "a"),
                     aspect = aspect, groups = barley[[groups]],
                     scales = list(abbreviate = abbrev),
                     par.settings = list(plot.line = list(col = linecol))),
           parameters = list(abbrev = FALSE, aspect = 0.5,
                             groups = c("none", "year", "variety"),
                             linecol = "red"))
  ## Looking through 100 time series and comparing to a reference;
  ## Use buttons to save the current series number or its mean value.
  dat <- ts(matrix(cumsum(rnorm(100*100)), ncol = 100), start = 1900)
  colnames(dat) <- paste("Series", 1:100)
  ref <- (dat[,3] + dat[,4]) / 2
  playwith(xyplot(cbind(dat[,i], ref = ref)),
           parameters = list(i = 1:100,
                             print_i = function(playState) print(playState$env$i),
                             print_mean = function(p) print(mean(dat[,p$env$i])),
                             save_to_ii = function(playState)
                               .GlobalEnv$ii <- playState$env$i,
                             append_to_ii = function(playState) {
                               if (!exists("ii")) ii <- c()
                               
                               .GlobalEnv$ii <- c(ii, playState$env$i)
                             })
  )
  ## Composite plot (base graphics).
  ## Adapted from an example in help("legend").
  ## In this case, the initial plot() call is detected correctly;
  ## in more complex cases may need e.g. main.function="plot".
  ## Here we also construct data points and labels manually.
  x <- seq(-4*pi, 4*pi, by = pi/24)
  pts <- data.frame(x = x, y = c(sin(x), cos(x), tan(x)))
  labs <- rep(c("sin", "cos", "tan"), each = length(x))
  labs <- paste(labs, round(180 * x / pi) %% 360)
  playwith( {
    plot(x, sin(x), type = "l", xlim = c(-pi, pi),
         ylim = c(-1.2, 1.8), col = 3, lty = 2)
    points(x, cos(x), pch = 3, col = 4)
    lines(x, tan(x), type = "b", lty = 1, pch = 4, col = 6)
    legend("topright", c("sin", "cos", "tan"), col = c(3,4,6),
           lty = c(2, -1, 1), pch = c(-1, 3, 4),
           merge = TRUE, bg = 'gray90')
  }, data.points = pts, labels = labs)
  ## A ggplot example.
  ## NOTE: only qplot()-based calls will work.
  ## Labels are taken from rownames of the data.
  if (require(ggplot2)) {
    playwith(qplot(qsec, wt, data = mtcars) + stat_smooth())
  }
  ## A minimalist grid plot.
  ## This shows how to get playwith to work with custom plots:
  ## accept xlim/ylim and pass "viewport" to enable zooming.
  myGridPlot <- function(x, y, xlim = NULL, ylim = NULL, ...)
  {
    if (is.null(xlim)) xlim <- extendrange(x)
    if (is.null(ylim)) ylim <- extendrange(y)
    grid.newpage()
    pushViewport(plotViewport())
    grid.rect()
    pushViewport(viewport(xscale = xlim, yscale = ylim,
                          name = "theData"))
    grid.points(x, y, ...)
    grid.xaxis()
    grid.yaxis()
    upViewport(0)
  }
  playwith(myGridPlot(1:10, 11:20, pch = 17), viewport = "theData")
  ## Presenting the window as a modal dialog box.
  ## When the window is closed, ask user to confirm.
  confirmClose <- function(playState) {
    if (gconfirm("Close window and report IDs?",
                 parent = playState$win)) {
      cat("Indices of identified data points:\n")
      print(playGetIDs(playState))
      return(FALSE) ## close
    } else TRUE ## don't close
  }
  xy <- data.frame(x = 1:20, y = rnorm(20),
                   row.names = letters[1:20])
  playwith(xyplot(y ~ x, xy, main = "Select points, then close"),
           width = 4, height = 3.5, show.toolbars = FALSE,
           on.close = confirmClose, modal = TRUE,
           click.mode = "Brush")
  ## Ask user to save plot to PNG when window is closed:
  saveOnClose <- function(playState) {
    playDevSet(playState)
    if (!gconfirm("Save plot to PNG file? (Cancel = no)")) return(FALSE)
    fname <- gfile("Save PNG file as:", type = "save")
    if (is.na(fname)) return(TRUE) ## cancel
    dev.off(dev.copy(Cairo_png, file = fname,
                     width = dev.size()[1], height = dev.size()[2]))
    FALSE
  }
  #playwith.options(on.close = saveOnClose)
  ## Demonstrate cacheing of objects in local environment.
  ## By default, only local variables in the plot call are stored.
  x_global <- rnorm(100)
  doLocalStuff <- function(...) {
    y_local <- rnorm(100)
    angle <- (atan2(y_local, x_global) / (2*pi)) + 0.5
    color <- hsv(h = angle, v = 0.75)
    doRays <- function(x, y, col) {
      segments(0, 0, x, y, col = col)
    }
    playwith(plot(x_global, y_local, pch = 8, col = color,
                  panel.first = doRays(x_global, y_local, color)),
             ...)
  }
  doLocalStuff(title = "locals only") ## eval.args = NA is default
  ## List objects that have been copied and stored:
  ## Note: if you rm(x_global) now, redraws will fail.
  ls(playDevCur()$env)
  ## Next: store all data objects (in a new window):
  doLocalStuff(title = "all stored", eval.args = TRUE, new = TRUE)
  ls(playDevCur()$env)
  ## Now there are two devices open:
  str(playDevList())
  playDevCur()
  playDevOff()
  playDevCur()
  ## Not run:
  ## Big data example, do not try to guess labels or time.mode.
  gc()
  bigobj <- rpois(5000000, 1)
  print(object.size(bigobj), units = "Mb")
  gc()
  playwith(qqmath(~ bigobj, f.value = ppoints(500)),
           data.points = NA, labels = NA, time.mode = FALSE)
  playDevOff()
  gc()
  ## or generate the trellis object first:
  trel <- qqmath(~ bigobj, f.value = ppoints(500))
  playwith(trel)
  rm(trel)
  ## in this case, it is much better to compute the sample first:
  subobj <- quantile(bigobj, ppoints(500), na.rm = TRUE)
  playwith(qqmath(~ subobj))
  rm(subobj)
  rm(bigobj)
  ## End(Not run)
  ## See demo(package = "playwith") for examples of new tools.
}
  
################## fImport (get data from Yahoo, Oanda, and Federal Reserve) ####
library(fImport)
help("fredImport")
fredImport(query = "DEXUSEU", file = "C:\\Users\\cli388\\Downloads\\tempfile", source = NULL, frequency = "daily", 
           from = NULL, to = Sys.timeDate(), nDaysBack = NULL,
           save = FALSE, sep = ";", try = TRUE) 

help("fredSeries")
fredSeries(symbols = "DEXUSEU", from = NULL, to = Sys.timeDate(), nDaysBack = 366)
################## Check duplications and remove duplicate #####
 #count duplicate
anyDuplicated(df)

 #check what is duplicate
df[duplicated(df),]
 
 #remove duplicate
df[!duplicated(df),]
 #same as above
unique(df)

 # Show unique repeat entries (row names may differ, but values are the same)
unique(df[duplicated(df),])
################## combine columns into one #####
df <- data.frame(1,1:7)
df.test <- data.frame(paste("insert into ('",df$X1,"','",df$X1.7,"','",df$X1,"')", sep = "")) #no space is spe = ""
################## write to txt file #####
fileconn <- file("C:\\Users\\cli388\\Desktop\\Field.txt")
writeLines(as.character(table), fileconn)
################## write csv file #######
write.csv(file = "C:\\Users\\cli388\\Desktop\\ComprehensiveK1_data_Example(All).csv", x = CNK1.All)
################## H2O ##################
 # create a connection and start H2O
  conn <- h2o.init(max_mem_size = '10g')
 #shutdown h20
  h2o.shutdown(conn = conn)
  
#===============================
# R books
#===============================
################## R Graphics Cookbook ##############################

#make a scatter plot use plot() and pass it a vector of x values followed by a vector of y values
plot(mtcars$wt, mtcars$mpg)

#make a scatter plot use ggplot2 package
library(ggplot2)
qplot(mtcars$wt, mtcars$mpg)

#make a scatter plot use ggplot2 package
qplot(wt, mpg, data=mtcars)
# This is equivalent to:
ggplot(mtcars, aes(x=wt, y=mpg)) + 
  geom_point()

#make a line graph using plot()
plot(pressure$temperature, pressure$pressure,  type="l")

#add points and/or multiple lines
plot(pressure$temperature, pressure$pressure, type="l")

points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col="red")

points(pressure$temperature, pressure$pressure/2, col="red")

#add points and/or multiple lines using ggplot2 (geom="line" function)
library(ggplot2)
qplot(temperature, pressure, data=pressure, geom="line")

# This is equivalent to:
ggplot(pressure, aes(x=temperature, y=pressure)) +
  geom_line()

# Lines and points together
qplot(temperature, pressure,  data=pressure, geom=c("line","point"))
# Equivalent to:
ggplot(pressure, aes(x=temperature, y=pressure)) +
  geom_line() +
  geom_point()

#create a bar graph
barplot(BOD$demand, names.arg=BOD$Time)

# Generate a table of counts
barplot(table(mtcars$cyl))

#create a bar graph using ggplot2
library(ggplot2)
qplot(BOD$Time, BOD$demand, geom="bar", stat="identity")

# Convert the x variable to a factor, so that it is treated as discrete
qplot(factor(BOD$Time), BOD$demand, geom="bar", stat="identity")

# cyl is continuous here
qplot(mtcars$cyl)
# Treat cyl as discrete
qplot(factor(mtcars$cyl))

######If the vector is in a data frame, you can use the following syntax:
# Bar graph of values. This uses the BOD data frame, with the
#"Time" column for x values and the "demand" column for y values.
qplot(Time, demand,  data=BOD, geom="bar",stat="identity")
# This is equivalent to:
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_bar(stat="identity")
# Bar graph of counts
qplot(factor(cyl), data=mtcars)
# This is equivalent to:
ggplot(mtcars, aes(x=factor(cyl))) +
  geom_bar()

#create a histogram
hist(mtcars$mpg)
# Specify approximate number of bins with breaks
hist(mtcars$mpg, breaks=10)

#create a histogram using ggplot2
library(ggplot2)
qplot(mtcars$mpg)

#If the vector is in a data frame, you can use the following syntax:
library(ggplot2)
qplot(mpg, data=mtcars, binwidth=4)
# This is equivalent to:
ggplot(mtcars, aes(x=mpg)) +
  geom_histogram(binwidth=4)

#create a box plot
plot(ToothGrowth$supp, ToothGrowth$len)
# Formula syntax
boxplot(len ~ supp,  data = ToothGrowth)
# Put interaction of two variables on x-axis
boxplot(len ~ supp + dose, data = ToothGrowth)

#create a box plot use ggplot2
library(ggplot2)
qplot(ToothGrowth$supp, ToothGrowth$len, geom="boxplot")

qplot(supp, len, data=ToothGrowth, geom="boxplot")
# This is equivalent to:
ggplot(ToothGrowth,  aes(x=supp, y=len))+ 
  geom_boxplot()

# Using three separate vectors
qplot(interaction(ToothGrowth$supp, ToothGrowth$dose),  ToothGrowth$len, geom="boxplot")
# Alternatively, get the columns from the data frame
qplot(interaction(supp, dose), len, data=ToothGrowth, geom="boxplot")
# This is equivalent to:
ggplot(ToothGrowth, aes(x=interaction(supp, dose), y=len)) +
  geom_boxplot()

#Plotting a Function Curve
curve(x^3 - 5*x, from=-4, to=4)

# Plot a user-defined function
myfun <- function(xvar) {
  1/(1 + exp(-xvar + 10))
}
curve(myfun(x), from=0, to=20)
# Add a line:
curve(1-myfun(x),  add = TRUE, col = "red")

#Plotting a Function Curve use ggplot2
library(ggplot2)
# This sets the x range from 0 to 20
qplot(c(0,20), fun=myfun,  stat="function", geom="line")
# This is equivalent to:
ggplot(data.frame(x=c(0, 20)), aes(x=x)) +
  stat_function(fun=myfun, geom="line")

#Making a Basic Bar Graph
library(gcookbook) # For the data set
ggplot(pg_mean, aes(x=group, y=weight)) + 
  geom_bar(stat="identity")


ggplot(BOD, aes(x=Time, y=demand)) + 
  geom_bar(stat="identity")
# Convert Time to a discrete (categorical) variable with factor()
ggplot(BOD,aes(x=factor(Time), y=demand)) +
  geom_bar(stat="identity")

ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", fill="lightblue", colour="black")

#Grouping Bars Together
#dataset
cabbage_exp

library(gcookbook)

ggplot(cabbage_exp, aes(x=Date,  y=Weight, fill=Cultivar)) +
  geom_bar(position="dodge", stat="identity")

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(position="dodge", colour="black", stat="identity") +
  scale_fill_brewer(palette="Pastel1")

# Copy the data without last row
ce <- cabbage_exp[1:5, ] 
ce
ggplot(ce, aes(x=Date,  y=Weight,  fill=Cultivar)) +
  geom_bar(position="dodge",  colour="black", stat="identity") +
  scale_fill_brewer(palette="Pastel1")

#Making a Bar Graph of Counts
#Data set
diamonds
# Equivalent to using geom_bar(stat="bin")
ggplot(diamonds, aes(x=cut)) +
  geom_bar()


#binwidth defaulted to range/30
ggplot(diamonds, aes(x=carat)) +
  geom_bar(binwidth = 0.01)

#Using Colors in a Bar Graph
library(gcookbook) # For the data set
upc <- subset(uspopchange, rank(Change)>40)
upc
ggplot(upc, aes(x=Abb, y=Change, fill=State)) +
  geom_bar(stat="identity")

#add more setting using scale_fill_brewer() or scale_fill_manual().
ggplot(upc, aes(x=reorder(Abb, Change), y=Change, fill=Region)) +
  geom_bar(stat="identity",colour="black") +
  scale_fill_manual(values=c("#669933", "#FFCC66")) +
  xlab("State")

#Coloring Negative and Positive Bars Differently
library(gcookbook) # For the data set
csub <- subset(climate, Source=="Berkeley" & Year >= 1900)
csub$pos <- csub$Anomaly10y >= 0
csub
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity")

#add more setting
ggplot(csub, aes(x=Year, y=Anomaly10y, fill=pos)) +
  geom_bar(stat="identity", position="identity", colour="black", size=0.25) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE)

#Adjusting Bar Width and Spacing
#for standard-width bars:
library(gcookbook) # For the data set
ggplot(pg_mean, aes(x=group, y=weight)) + 
  geom_bar(stat="identity")
#For narrower bars:
ggplot(pg_mean, aes(x=group, y=weight)) +
  geom_bar(stat="identity", width=0.5)
#for wider bars (these have the maximum width of 1):
ggplot(pg_mean, aes(x=group,y=weight)) +
  geom_bar(stat="identity", width=1)

#For a grouped bar graph with narrow bars:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5,position="dodge")
#And with some space between the bars:
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity", width=0.5, position=position_dodge(0.7))
#same as 
geom_bar(position="dodge") 
geom_bar(width=0.9, position=position_dodge())
geom_bar(position=position_dodge(0.9))
geom_bar(width=0.9, position=position_dodge(width=0.9))

#Making a Stacked Bar Graph
library(gcookbook) # For the data set

#data set
cabbage_exp

#making a stacked bar graph
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity")

#same as

ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(stat="identity") +
  guides(fill=guide_legend(reverse=TRUE))


library(plyr) # Needed for desc()
library(ggplot2)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar, order=desc(Cultivar))) +
  geom_bar(stat="identity")
#more settings  
ggplot(cabbage_exp, aes(x=Date,y=Weight,fill=Cultivar)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_brewer(palette="Pastel1")

#Making a Proportional Stacked Bar Graph
library(gcookbook) # For the data set
library(plyr)
# Do a group-wise transform(), splitting on "Date"
ce <- ddply(cabbage_exp, "Date", transform, percent_weight = Weight / sum(Weight) * 100)

ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity")

#Do a percentage of total for table  
ddply(cabbage_exp, "Date", transform, percent_weight = Weight / sum(Weight) * 100)

#making the graph by percentage column  
ggplot(ce, aes(x=Date, y=percent_weight, fill=Cultivar)) +
  geom_bar(stat="identity", colour="black") +
  guides(fill=guide_legend(reverse=TRUE)) +
  scale_fill_brewer(palette="Pastel1")

#Adding Labels to a Bar Graph
# Below the top
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white")

# Above the top
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar),  y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2)

# Adjust y limits to be a little higher
ggplot(cabbage_exp,  aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=Weight), vjust=-0.2) +
  ylim(0, max(cabbage_exp$Weight) * 1.05)

# Map y positions slightly above bar top - y range of plot will auto-adjust
ggplot(cabbage_exp, aes(x=interaction(Date, Cultivar), y=Weight)) +
  geom_bar(stat="identity") +
  geom_text(aes(y=Weight+0.1, label=Weight))

#add some color
ggplot(cabbage_exp,  aes(x=Date, y=Weight,fill=Cultivar)) +
  geom_bar(stat="identity", position="dodge") +
  geom_text(aes(label=Weight), vjust=1.5, colour="white", position=position_dodge(.9), size=3)

# Sort by the day and sex columns
ce <- arrange(cabbage_exp, Date, Cultivar)
# Get the cumulative sum
ce <- ddply(ce, "Date",  transform, label_y=cumsum(Weight))
ce

#making graph by the table
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat="identity")  + 
  geom_text(aes(y=label_y, label=Weight), vjust=1.5, colour="white")

ce <- arrange(cabbage_exp,  Date, Cultivar)

# Calculate y position, placing it in the middle
ce <- ddply(ce,  "Date",  transform, label_y=cumsum(Weight)-0.5*Weight)

ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat="identity") + 
  geom_text(aes(y=label_y, label=Weight), colour="white")

#More setting
ggplot(ce, aes(x=Date, y=Weight, fill=Cultivar)) + 
  geom_bar(stat="identity", colour="black") + 
  geom_text(aes(y=label_y, label=paste(format(Weight, nsmall=2), "kg")), size=4) + 
  guides(fill=guide_legend(reverse=TRUE)) + 
  scale_fill_brewer(palette="Pastel1")

#Making a Cleveland Dot Plot
library(gcookbook) # For the data set
tophit <- tophitters2001[1:25, ] # Take the top 25 from the tophitters data set

ggplot(tophit, aes(x=avg, y=name)) + 
  geom_point()

tophit[, c("name", "lg", "avg")]

#more setting with large dot

ggplot(tophit, aes(x=avg, y=reorder(name, avg))) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(colour="grey60", linetype="dashed"))

#text labels by 60 degrees
ggplot(tophit, aes(x=reorder(name, avg), y=avg)) +
  geom_point(size=3) + # Use a larger dot
  theme_bw() +
  theme(axis.text.x = element_text(angle=60, hjust=1),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey60", linetype="dashed"))

# Get the names, sorted first by lg, then by avg
nameorder <- tophit$name[order(tophit$lg, tophit$avg)]
# Turn name into a factor, with levels in the order of nameorder
tophit$name <- factor(tophit$name, levels=nameorder)

ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=lg)) +
  scale_colour_brewer(palette="Set1", limits=c("NL","AL")) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank(), # No horizontal grid lines
        legend.position=c(1, 0.55), # Put legend inside plot area
        legend.justification=c(1, 0.5))

#Another way to separate the groups
ggplot(tophit, aes(x=avg, y=name)) +
  geom_segment(aes(yend=name), xend=0, colour="grey50") +
  geom_point(size=3, aes(colour=lg)) +
  scale_colour_brewer(palette="Set1", limits=c("NL","AL"), guide=FALSE) +
  theme_bw() +
  theme(panel.grid.major.y = element_blank()) +
  facet_grid(lg ~ ., scales="free_y", space="free_y")

#Making a Basic Line Graph
ggplot(BOD, aes(x=Time, y=demand)) + geom_line()

BOD1 <- BOD # Make a copy of the data
BOD1$Time <- factor(BOD1$Time)

#change X 
ggplot(BOD1, aes(x=Time, y=demand, group=1)) + geom_line()

# Change Y 
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + ylim(0, max(BOD$demand))
#same as above
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + expand_limits(y=0)

#Adding Points to a Line Graph
ggplot(BOD, aes(x=Time, y=demand)) + geom_line() + geom_point()

library(gcookbook) # For the data set
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point()
# Same with a log y-axis
ggplot(worldpop, aes(x=Year, y=Population)) + geom_line() + geom_point() +
  scale_y_log10()

#Making a Line Graph with Multiple Lines
# Load plyr so we can use ddply() to create the example data set
library(plyr)
# Summarize the ToothGrowth data
#mean() is sum of its data values divided by data count
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
# Map supp to colour
ggplot(tg, aes(x=dose, y=length, colour=supp)) + geom_line()
# Map supp to linetype
ggplot(tg, aes(x=dose, y=length, linetype=supp)) + geom_line()

#data set
tg
str(tg)

#Without this statement, ggplot() won’t know how to group the data together to draw the lines
ggplot(tg, aes(x=factor(dose), y=length, colour=supp)) + geom_line()
#there are multiple data points at each y location, and ggplot() thinks they’re all in one group.
ggplot(tg, aes(x=dose, y=length)) + geom_line()

#map variables to properties of the points, such as shape and fill
# Make the points a little larger
ggplot(tg, aes(x=dose, y=length, shape=supp)) + geom_line() +
  geom_point(size=4) 
# Also use a point with a color fill
ggplot(tg, aes(x=dose, y=length, fill=supp)) + geom_line() +
  geom_point(size=4, shape=21) 

#fix the overlap
ggplot(tg, aes(x=dose, y=length, shape=supp)) +
  geom_line(position=position_dodge(0.2)) + # Dodge lines by 0.2
  geom_point(position=position_dodge(0.2), size=4) # Dodge points by 0.2

#Changing the Appearance of Lines
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line(linetype="dashed", size=1, colour="blue")

# Load plyr so we can use ddply() to create the example data set
library(plyr)
# Summarize the ToothGrowth data
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
ggplot(tg, aes(x=dose, y=length, colour=supp)) +
  geom_line() +
  scale_colour_brewer(palette="Set1")

# If both lines have the same properties, you need to specify a variable to
# use for grouping
ggplot(tg, aes(x=dose, y=length, group=supp)) +
  geom_line(colour="darkgreen", size=1.5)
# Since supp is mapped to colour, it will automatically be used for grouping
ggplot(tg, aes(x=dose, y=length, colour=supp)) +
  geom_line(linetype="dashed") +
  geom_point(shape=22, size=3, fill="white")

#Changing the Appearance of Points  
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line() +
  geom_point(size=4, shape=22, colour="darkred", fill="pink")

#Changing the Appearance of Points 
ggplot(BOD, aes(x=Time, y=demand)) +
  geom_line() +
  geom_point(size=4, shape=21, fill="white") #The fill color is relevant only for some point shapes (numbered 21–25)

# Load plyr so we can use ddply() to create the example data set
library(plyr)
# Summarize the ToothGrowth data
tg <- ddply(ToothGrowth, c("supp", "dose"), summarise, length=mean(len))
# Save the position_dodge specification because we'll use it multiple times
pd <- position_dodge(0.2)
ggplot(tg, aes(x=dose, y=length, fill=supp)) +
  geom_line(position=pd) +
  geom_point(shape=21, size=3, position=pd) +
  scale_fill_manual(values=c("black","white"))

#Making a Graph with a Shaded Area
# Convert the sunspot.year data set into a data frame for this example
sunspotyear <- data.frame (
  Year = as.numeric(time(sunspot.year)),
  Sunspots = as.numeric(sunspot.year)
)
#making the shaded graph
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) + 
  geom_area()
#more setting
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) +
  geom_area(colour="black", fill="blue", alpha=.2)
#same as above
ggplot(sunspotyear, aes(x=Year, y=Sunspots)) +
  geom_area(fill="blue", alpha=.2) +
  geom_line() #adding an outline

#Making a Stacked Area Graph
library(gcookbook) # For the data set
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
#data set
uspopage
#filled areas semitransparent (alpha=.4)
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))

#reverse the stacking order
library(plyr) # For the desc() function
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup))) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues")

#remove the side line
ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup, order=desc(AgeGroup))) +
  geom_area(colour=NA, alpha=.4) +
  scale_fill_brewer(palette="Blues") +
  geom_line(position="stack", size=.2)

#Making a Proportional Stacked Area Graph
library(gcookbook) # For the data set
library(plyr) # For the ddply() function
# Convert Thousands to Percent
uspopage_prop <- ddply(uspopage, 
                       "Year", 
                       transform,
                       Percent = Thousands / sum(Thousands) * 100)

ggplot(uspopage_prop, aes(x=Year, y=Percent, fill=AgeGroup)) +
  geom_area(colour="black", size=.2, alpha=.4) +
  scale_fill_brewer(palette="Blues", breaks=rev(levels(uspopage$AgeGroup)))

#data set
uspopage
#add percentage to data set
uspopage_prop <- ddply(uspopage, 
                       "Year", 
                       transform,
                       Percent = Thousands / sum(Thousands) * 100)

#Adding a Confidence Region
library(gcookbook) # For the data set
# Grab a subset of the climate data
clim <- subset(climate, Source == "Berkeley",
               select=c("Year", "Anomaly10y", "Unc10y"))
clim

# Shaded region
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_ribbon(aes(ymin=Anomaly10y-Unc10y, ymax=Anomaly10y+Unc10y), alpha=0.2) +
  geom_line()

# With a dotted line for upper and lower bounds
ggplot(clim, aes(x=Year, y=Anomaly10y)) +
  geom_line(aes(y=Anomaly10y-Unc10y), colour="grey50", linetype="dotted") +
  geom_line(aes(y=Anomaly10y+Unc10y), colour="grey50", linetype="dotted") +
  geom_line()

#Making a Basic Scatter Plot
library(gcookbook) # For the data set
# List the two columns we'll use
heightweight[, c("ageYear", "heightIn")]
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
#different type of point
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=21)
#different config of point
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(size=1.5)

#Grouping Data Points by a Variable Using Shape or Color
library(gcookbook) # For the data set
# Show the three columns we'll use
heightweight[, c("sex", "ageYear", "heightIn")]
#change the color of point
ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex)) + geom_point()
#change the shape of point
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex)) + geom_point()
#change color and shap
ggplot(heightweight, aes(x=ageYear, y=heightIn, color=sex, shape=sex)) + geom_point()
#detail the points, Other shapes can be used withscale_shape_manual(), and other colors can be used with scale_colour_brewer() or scale_colour_manual().
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) +
  geom_point() +
  scale_shape_manual(values=c(1,2)) +
  scale_colour_brewer(palette="Set1")

#Using Different Point Shapes (0-25, *, ., "o", "O", "+", "-", "|", "%", "#", "0")
library(gcookbook) # For the data set
ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point(shape=3)
# Use slightly larger points and use a shape scale with custom values
ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex)) +
  geom_point(size=3) + scale_shape_manual(values=c(1, 4))

# Make a copy of the data
hw <- heightweight
# Categorize into <100 and >=100 groups
hw$weightGroup <- cut(hw$weightLb, 
                      breaks=c(-Inf, 100, Inf),
                      labels=c("< 100", ">= 100"))

# Use shapes with fill and color, and use colors that are empty (NA) and
# filled
ggplot(hw, aes(x=ageYear, y=heightIn, shape=sex, fill=weightGroup)) +
  geom_point(size=2.5) +
  scale_shape_manual(values=c(21, 24)) +
  scale_fill_manual(values=c(NA, "black"),
                    guide=guide_legend(override.aes=list(shape=21)))

#Mapping a Continuous Variable to Color or Size  
library(gcookbook) # For the data set
# List the four columns we'll use
heightweight[, c("sex", "ageYear", "heightIn", "weightLb")]

ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) + geom_point()
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb)) + geom_point()

ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) +
  geom_point(shape=21, size=2.5) +
  scale_fill_gradient(low="blue", high="white")

# Using guide_legend() will result in a discrete legend instead of a colorbar
ggplot(heightweight, aes(x=weightLb, y=heightIn, fill=ageYear)) +
  geom_point(shape=21, size=2.5) +
  scale_fill_gradient(low="red", high="white",
                      breaks=12:17,
                      guide=guide_legend())
#points 50% transparent (alpha=.5), make the area of the points proportional to the value (scale_size_area())
ggplot(heightweight, aes(x=ageYear, y=heightIn, size=weightLb, colour=sex)) +
  geom_point(alpha=.5) +
  scale_size_area() + # Make area proportional to numeric value
  scale_colour_brewer(palette="Set1")

#Dealing with Overplotting
#data set
sp <- ggplot(diamonds, aes(x=carat, y=price))
sp + geom_point()
#change the transparent
sp + geom_point(alpha=.1)
sp + geom_point(alpha=.01)
#change the color
sp + stat_bin2d()
sp + stat_bin2d(bins=50) +
  scale_fill_gradient(low="lightblue", 
                      high="red", 
                      limits=c(0, 6000))
#using stat_binhex() with package hexbin  
library(hexbin)
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", 
                      high="red",
                      limits=c(0, 8000))
#configer the breaks   
sp + stat_binhex() +
  scale_fill_gradient(low="lightblue", 
                      high="red",
                      breaks=c(0, 250, 500, 1000, 2000, 4000, 6000),
                      limits=c(0, 6000))
#randomly jitter the points with position_jitter() to avoide overlap
#data set
sp1 <- ggplot(ChickWeight, aes(x=Time, y=weight))
#with overlap
sp1 + geom_point()
#use position_jitter() to avoid overlap
sp1 + geom_point(position="jitter")
# Could also use geom_jitter(), which is equivalent
sp1 + geom_point(position=position_jitter(width=.5, height=0))

sp1 + geom_boxplot(aes(group=Time))

#Adding Fitted Regression Model Lines
library(gcookbook) # For the data set
# The base plot
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn))
sp + geom_point() + stat_smooth(method=lm)
# 99% confidence region
sp + geom_point() + stat_smooth(method=lm, level=0.99)
# No confidence region
sp + geom_point() + stat_smooth(method=lm, se=FALSE)
#change the point color
sp + geom_point(colour="grey60") +
  stat_smooth(method=lm, se=FALSE, colour="black")
#add stat_smooth() without specifying the method 
sp + geom_point(colour="grey60") + stat_smooth(method=loess)

library(MASS) # For the data set
b <- biopsy
b$classn[b$class=="benign"] <- 0
b$classn[b$class=="malignant"] <- 1
#jitter the points and make them semitransparent(alpha=0.4), hollow (shape=21), and slightly smaller (size=1.5)
#add a fitted logistic regression line by telling stat_smooth() to use the glm() function with the option family=binomial
ggplot(b, aes(x=V1, y=classn)) +
  geom_point(position=position_jitter(width=0.3, height=0.06), alpha=0.4,
             shape=21, size=1.5) +
  stat_smooth(method=glm, family=binomial)

sps <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
  geom_point() +
  scale_colour_brewer(palette="Set1")
sps + geom_smooth()
#more setting
sps + geom_smooth(method=lm, se=FALSE, fullrange=TRUE)

#Adding Fitted Lines from an Existing Model
library(gcookbook) # For the data set
model <- lm(heightIn ~ ageYear + I(ageYear^2), heightweight)
model
# Create a data frame with ageYear column, interpolating across range
xmin <- min(heightweight$ageYear)
xmax <- max(heightweight$ageYear)
predicted <- data.frame(ageYear=seq(xmin, xmax, length.out=100))
# Calculate predicted values of heightIn
predicted$heightIn <- predict(model, predicted)
predicted

sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) +
  geom_point(colour="grey40")
sp + geom_line(data=predicted, size=1)

# Given a model, predict values of yvar from xvar
# This supports one predictor and one predicted variable
# xrange: If NULL, determine the x range from the model object. If a vector with
# two numbers, use those as the min and max of the prediction range.
# samples: Number of samples across the x range.
# ...: Further arguments to be passed to predict()
predictvals <- function(model, xvar, yvar, xrange=NULL, samples=100, ...) {
  # If xrange isn't passed in, determine xrange from the models.
  # Different ways of extracting the x range, depending on model type
  if (is.null(xrange)) {
    if (any(class(model) %in% c("lm", "glm")))
      xrange <- range(model$model[[xvar]])
    else if (any(class(model) %in% "loess"))
      xrange <- range(model$x)
  }
  newdata <- data.frame(x = seq(xrange[1], xrange[2], length.out = samples))
  names(newdata) <- xvar
  newdata[[yvar]] <- predict(model, newdata = newdata, ...)
  newdata
}
#set data set
modlinear <- lm(heightIn ~ ageYear, heightweight)
modloess <- loess(heightIn ~ ageYear, heightweight)
#set data set
lm_predicted <- predictvals(modlinear, "ageYear", "heightIn")
loess_predicted <- predictvals(modloess, "ageYear", "heightIn")

sp + geom_line(data=lm_predicted, colour="red", size=.8) +
  geom_line(data=loess_predicted, colour="blue", size=.8)

library(MASS) # For the data set
#data set
b <- biopsy
b$classn[b$class=="benign"] <- 0
b$classn[b$class=="malignant"] <- 1
#perform the logistic regression
fitlogistic <- glm(classn ~ V1, b, family=binomial)
# Get predicted values
glm_predicted <- predictvals(fitlogistic, "V1", "classn", type="response")
ggplot(b, aes(x=V1, y=classn)) +
  geom_point(position=position_jitter(width=.3, height=.08), alpha=0.4,
             shape=21, size=1.5) +
  geom_line(data=glm_predicted, colour="#1177FF", size=1)

#Adding Fitted Lines from Multiple Existing Models
make_model <- function(data) {
  lm(heightIn ~ ageYear, data)
}
#data set
library(gcookbook) # For the data set
library(plyr)
models <- dlply(heightweight, "sex", .fun = make_model)
# Print out the list of two lm objects, f and m
models

predvals <- ldply(models, .fun=predictvals, xvar="ageYear", yvar="heightIn")
predvals

#plot the data with the predicted values
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
  geom_point() + geom_line(data=predvals)

#pass in xrange for same x range across all groups
predvals <- ldply(models, .fun=predictvals, xvar="ageYear", yvar="heightIn", xrange=range(heightweight$ageYear))
#plot the data with the predicted values
ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
  geom_point() + geom_line(data=predvals)

#Adding Annotations with Model Coefficients
library(gcookbook) # For the data set
model <- lm(heightIn ~ ageYear, heightweight)
summary(model)
# First generate prediction data
pred <- predictvals(model, "ageYear", "heightIn")
sp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point() +
  geom_line(data=pred)
sp + annotate("text", label="r^2=0.42", x=16.5, y=52)
sp + annotate("text", label="r^2 == 0.42", parse = TRUE, x=16.5, y=52)

expression(r^2 == 0.42) # Valid
expression(r^2 = 0.42) # Not valid

#some parse example
eqn <- as.character(as.expression(
  substitute(italic(y) == a + b * italic(x) * "," ~~ italic(r)^2 ~ "=" ~ r2,
             list(a = format(coef(model)[1], digits=3),
                  b = format(coef(model)[2], digits=3),
                  r2 = format(summary(model)$r.squared, digits=2)
             ))))
eqn

parse(text=eqn) # Parsing turns it into an expression
#add the parse label
sp + annotate("text", label=eqn, parse=TRUE, x=Inf, y=-Inf, hjust=1.1, vjust=-.5)

#Adding Marginal Rugs to a Scatter Plot
ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() + 
  geom_rug()

ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point() +
  geom_rug(position="jitter", size=.2)

#Labeling Points in a Scatter Plot
library(gcookbook) # For the data set
subset(countries, Year==2009 & healthexp>2000)
#storge the graph
sp <- ggplot(subset(countries, Year==2009 & healthexp>2000),
             aes(x=healthexp, y=infmortality)) +
  geom_point()
#add special labels to graph
sp + annotate("text", x=4350, y=5.4, label="Canada") +
  annotate("text", x=7400, y=6.8, label="USA")
#add all labels to graph
sp + geom_text(aes(label=Name), size=4)
#add vertical adjust of adding label
sp + geom_text(aes(label=Name), size=4, vjust=0)
# Add a little extra to y
sp + geom_text(aes(y=infmortality+.1, label=Name), size=4, vjust=0)
#add horizontal adjust of adding label
sp + geom_text(aes(label=Name), size=4, hjust=0)
# Add a little extra to x
sp + geom_text(aes(x=healthexp+100, label=Name), size=4, hjust=0)

#filter the data set
cdat <- subset(countries, Year==2009 & healthexp>2000)
cdat$Name1 <- cdat$Name

idx <- cdat$Name1 %in% c("Canada", "Ireland", "United Kingdom", "United States",
                         "New Zealand", "Iceland", "Japan", "Luxembourg",
                         "Netherlands", "Switzerland")
idx
#use that Boolean vector to overwrite all the other entries in Name1 with NA
cdat$Name1[!idx] <- NA
#deploy the filtered data for graph
ggplot(cdat, aes(x=healthexp, y=infmortality)) +
  geom_point() +
  geom_text(aes(x=healthexp+100, label=Name1), size=4, hjust=0) +
  xlim(2000, 10000)

#Creating a Balloon Plot
library(gcookbook) # For the data set
cdat <- subset(countries, Year==2009 &
                 Name %in% c("Canada", "Ireland", "United Kingdom", "United States",
                             "New Zealand", "Iceland", "Japan", "Luxembourg",
                             "Netherlands", "Switzerland"))
cdat

p <- ggplot(cdat, aes(x=healthexp, y=infmortality, size=GDP)) +
  geom_point(shape=21, colour="black", fill="cornsilk")
# GDP mapped to radius (default with scale_size_continuous)
p
# GDP mapped to area instead, and larger circles
p + scale_size_area(max_size=15)

# Add up counts for male and female
hec <- HairEyeColor[,,"Male"] + HairEyeColor[,,"Female"]
# Convert to long format
library(reshape2)
hec <- melt(hec, value.name="count")
ggplot(hec, aes(x=Eye, y=Hair)) +
  geom_point(aes(size=count), shape=21, colour="black", fill="cornsilk") +
  scale_size_area(max_size=20, guide=FALSE) +
  geom_text(aes(y=as.numeric(Hair)-sqrt(count)/22, label=count), vjust=1,
            colour="grey60", size=4)

#Making a Scatter Plot Matrix  
library(gcookbook) # For the data set
#data set
c2009 <- subset(countries, Year==2009,
                select=c(Name, GDP, laborrate, healthexp, infmortality))
c2009
#make the scatter plot matrix
pairs(c2009[,2:5]) #We didn’t use ggplot2 here because it doesn’t make scatter plot matrices (at least, not well).

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y, use="complete.obs"))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * (1 + r) / 2)
}
#show histograms of each variable along the diagonal, we’ll define panel.hist
panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
#use panel.smooth for the lower panels, which makes a scatter plot and adds a LOWESS smoothed line,
pairs(c2009[,2:5], upper.panel = panel.cor,
      diag.panel = panel.hist,
      lower.panel = panel.smooth)

#store the function
panel.lm <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                      cex = 1, col.smooth = "black", ...) {
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  abline(stats::lm(y ~ x), col = col.smooth, ...)
}
#deploy the function
pairs(c2009[,2:5], pch=".",
      upper.panel = panel.cor,
      diag.panel = panel.hist,
      lower.panel = panel.lm)

#Making a Basic Histogram
ggplot(faithful, aes(x=waiting)) + geom_histogram()
#data set
faithful
# Store the values in a simple vector
w <- faithful$waiting
ggplot(NULL, aes(x=w)) + geom_histogram()

# Set the width of each bin to 5
ggplot(faithful, aes(x=waiting)) +
  geom_histogram(binwidth=5, fill="white", colour="black")
# Divide the x range into 15 bins
binsize <- diff(range(faithful$waiting))/15
ggplot(faithful, aes(x=waiting)) +
  geom_histogram(binwidth=binsize, fill="white", colour="black")

# Save the base object for reuse
h <- ggplot(faithful, aes(x=waiting)) 
h + geom_histogram(binwidth=8, fill="white", colour="black", origin=31)
h + geom_histogram(binwidth=8, fill="white", colour="black", origin=35)

#Making Multiple Histograms from Grouped Data
library(MASS) # For the data set
# Use smoke as the faceting variable
ggplot(birthwt, aes(x=bwt)) + 
  geom_histogram(fill="white", colour="black") +
  facet_grid(smoke ~ .)

#data set
birthwt

birthwt1 <- birthwt # Make a copy of the data
# Convert smoke to a factor
birthwt1$smoke <- factor(birthwt1$smoke)
levels(birthwt1$smoke)

library(plyr) # For the revalue() function
birthwt1$smoke <- revalue(birthwt1$smoke, c("0"="No Smoke", "1"="Smoke"))
#Now when we plot it again, it shows the new labels
ggplot(birthwt1, aes(x=bwt)) + 
  geom_histogram(fill="white", colour="black") +
  facet_grid(smoke ~ .)

ggplot(birthwt, aes(x=bwt)) + 
  geom_histogram(fill="white", colour="black") +
  facet_grid(race ~ .)

ggplot(birthwt, aes(x=bwt)) + 
  geom_histogram(fill="white", colour="black") +
  facet_grid(race ~ ., scales="free")

# Convert smoke to a factor
birthwt1$smoke <- factor(birthwt1$smoke)
# Map smoke to fill, make the bars NOT stacked, and make them semitransparent
ggplot(birthwt1, aes(x=bwt, fill=smoke)) +
  geom_histogram(position="identity", alpha=0.4)

#Making a Density Curve
ggplot(faithful, aes(x=waiting)) + geom_density()
# The expand_limits() increases the y range to include the value 0
ggplot(faithful, aes(x=waiting)) + 
  geom_line(stat="density") +
  expand_limits(y=0)
#data set
faithful
# Store the values in a simple vector
w <- faithful$waiting
ggplot(NULL, aes(x=w)) + geom_density()
#add more lines with different setting
ggplot(faithful, aes(x=waiting)) +
  geom_line(stat="density", adjust=.25, colour="red") +
  geom_line(stat="density") +
  geom_line(stat="density", adjust=2, colour="blue")
#add shadow
ggplot(faithful, aes(x=waiting)) +
  geom_density(fill="blue", alpha=.2) +
  xlim(35, 105)
# This draws a blue polygon with geom_density(), then adds a line on top
ggplot(faithful, aes(x=waiting)) +
  geom_density(fill="blue", colour=NA, alpha=.2) +
  geom_line(stat="density") +
  xlim(35, 105)
#histogram with line
ggplot(faithful, aes(x=waiting, y=..density..)) +
  geom_histogram(fill="cornsilk", colour="grey60", size=.2) +
  geom_density() +
  xlim(35, 105)

#Making Multiple Density Curves from Grouped Data
library(MASS) # For the data set
# Make a copy of the data
birthwt1 <- birthwt
# Convert smoke to a factor
birthwt1$smoke <- factor(birthwt1$smoke)
# Map smoke to colour
ggplot(birthwt1, aes(x=bwt, colour=smoke)) + geom_density()
# Map smoke to fill and make the fill semitransparent by setting alpha
ggplot(birthwt1, aes(x=bwt, fill=smoke)) + geom_density(alpha=.3)
#data set
birthwt
ggplot(birthwt1, aes(x=bwt)) + geom_density() + facet_grid(smoke ~ .)
#add levels to data set
levels(birthwt1$smoke)
library(plyr) # For the revalue function
birthwt1$smoke <- revalue(birthwt1$smoke, c("0"="No Smoke", "1"="Smoke"))
#Now when we plot it again, it shows the new labels (Figure 6-12, right):
ggplot(birthwt1, aes(x=bwt)) + geom_density() + facet_grid(smoke ~ .)
#add histogram
ggplot(birthwt1, aes(x=bwt, y=..density..)) +
  geom_histogram(binwidth=200, fill="cornsilk", colour="grey60", size=.2) +
  geom_density() +
  facet_grid(smoke ~ .)

#Making a Frequency Polygon
#Use geom_freqpoly()
ggplot(faithful, aes(x=waiting)) + geom_freqpoly()
#more settings
ggplot(faithful, aes(x=waiting)) + geom_freqpoly(binwidth=4)

# Use 15 bins
binsize <- diff(range(faithful$waiting))/15
ggplot(faithful, aes(x=waiting)) + geom_freqpoly(binwidth=binsize)

#Making a Basic Box Plot
library(MASS) # For the data set
# Use factor() to convert numeric variable to discrete
ggplot(birthwt, aes(x=factor(race), y=bwt)) + 
  geom_boxplot()
#data set
birthwt
#change the width of the boxes
ggplot(birthwt, aes(x=factor(race), y=bwt)) + 
  geom_boxplot(width=.5)
#use smaller points, and hollow circles
ggplot(birthwt, aes(x=factor(race), y=bwt)) +
  geom_boxplot(outlier.size=1.5, outlier.shape=21)
#set arbitrary value for x and remove the x-axis tick markers and label
ggplot(birthwt, aes(x=1, y=bwt)) + geom_boxplot() +
  scale_x_continuous(breaks=NULL) +
  theme(axis.title.x = element_blank())

#Adding Notches to a Box Plot
library(MASS) # For the data set
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot(notch=TRUE)

#Adding Means to a Box Plot
library(MASS) # For the data set
#Use stat_summary()
ggplot(birthwt, aes(x=factor(race), y=bwt)) + geom_boxplot() +
  stat_summary(fun.y="mean", geom="point", shape=23, size=3, fill="white")

#Making a Violin Plot
library(gcookbook) # For the data set
# Base plot
p <- ggplot(heightweight, aes(x=sex, y=heightIn))
p + geom_violin()
#add box plot
p + geom_violin() + geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
  stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)
#keep the violin tails
p + geom_violin(trim=FALSE)

# Scaled area proportional to number of observations
p + geom_violin(scale="count")
# More smoothing
p + geom_violin(adjust=2)
# Less smoothing
p + geom_violin(adjust=.5)

#Making a Dot Plot
library(gcookbook) # For the data set
countries2009 <- subset(countries, Year==2009 & healthexp>2000)
p <- ggplot(countries2009, aes(x=infmortality))
#Use geom_dotplot()
p + geom_dotplot()
#more setting
p + geom_dotplot(binwidth=.25) + geom_rug() +
  scale_y_continuous(breaks=NULL) + # Remove tick markers
  theme(axis.title.y=element_blank()) # Remove axis label
#use bins that are arranged with a fixed, regular spacing, like a histogram, use method="histodot".
p + geom_dotplot(method="histodot", binwidth=.25) + geom_rug() +
  scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())
#stacked centered
p + geom_dotplot(binwidth=.25, stackdir="center")
scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())
#entered in such a way that stacks with even and odd quantities stay aligned.
p + geom_dotplot(binwidth=.25, stackdir="centerwhole")
scale_y_continuous(breaks=NULL) + theme(axis.title.y=element_blank())

#Making Multiple Dot Plots for Grouped Data
library(gcookbook) # For the data set
ggplot(heightweight, aes(x=sex, y=heightIn)) +
  geom_dotplot(binaxis="y", binwidth=.5, stackdir="center")
#config the dots and add box
ggplot(heightweight, aes(x=sex, y=heightIn)) +
  geom_boxplot(outlier.colour=NA, width=.4) +
  geom_dotplot(binaxis="y", binwidth=.5, stackdir="center", fill=NA)
#Dot plot next to box plot
ggplot(heightweight, aes(x=sex, y=heightIn)) +
  geom_boxplot(aes(x=as.numeric(sex) + .2, group=sex), width=.25) +
  geom_dotplot(aes(x=as.numeric(sex) - .2, group=sex), binaxis="y",
               binwidth=.5, stackdir="center") +
  scale_x_continuous(breaks=1:nlevels(heightweight$sex),
                     labels=levels(heightweight$sex))

#Making a Density Plot of Two-Dimensional Data
# The base plot
p <- ggplot(faithful, aes(x=eruptions, y=waiting))
p + geom_point() + stat_density2d()

# Contour lines, with "height" mapped to color
p + stat_density2d(aes(colour=..level..))
# Map density estimate to fill color
p + stat_density2d(aes(fill=..density..), geom="raster", contour=FALSE)
# With points, and map density estimate to alpha
p + geom_point() +
  stat_density2d(aes(alpha=..density..), geom="tile", contour=FALSE)
#Marginal rug added to a scatter plot
p + stat_density2d(aes(fill=..density..), geom="raster",
                   contour=FALSE, h=c(.5,5))

#Adding Text Annotations
#Use annotate() and a text geom
p <- ggplot(faithful, aes(x=eruptions, y=waiting)) + geom_point()
p + annotate("text", x=3, y=48, label="Group 1") +
  annotate("text", x=4.5, y=66, label="Group 2")
#more setting of label
p + annotate("text", x=3, y=48, label="Group 1", family="serif",
             fontface="italic", colour="darkred", size=3) +
  annotate("text", x=4.5, y=66, label="Group 2", family="serif",
           fontface="italic", colour="darkred", size=3)
#other setting of label
p + annotate("text", x=3, y=48, label="Group 1", alpha=.1) + # Normal
  geom_text(x=4.5, y=66, label="Group 2", alpha=.1) # Overplotted

#Using Mathematical Expressions in Annotations
# A normal curve
p <- ggplot(data.frame(x=c(-3,3)), aes(x=x)) + stat_function(fun = dnorm)
#Annotation with mathematical expressions
p + annotate("text", x=2, y=0.3, parse=TRUE,
             label="frac(1, sqrt(2 * pi)) * e ^ {-x^2 / 2}")
#Moving the Annotation
p + annotate("text", x=0, y=0.05, parse=TRUE, size=4,
             label="'Function: ' * y==frac(1, sqrt(2*pi)) * e^{-x^2/2}")

#Adding Lines
library(gcookbook) # For the data set
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
# Add horizontal and vertical lines
p + geom_hline(yintercept=60) + geom_vline(xintercept=14)
# Add angled line
p + geom_abline(intercept=37.4, slope=1.75)

library(plyr) # For the ddply() function
hw_means <- ddply(heightweight, "sex", summarise, heightIn=mean(heightIn))
hw_means
#Multiple lines, drawn at the mean of each group
p + geom_hline(aes(yintercept=heightIn, colour=sex), data=hw_means,
               linetype="dashed", size=1)

pg <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_point()
#Lines with a discrete axis
pg + geom_vline(xintercept = 2)
pg + geom_vline(xintercept = which(levels(PlantGrowth$group)=="ctrl"))

#Adding Line Segments and Arrows
library(gcookbook) # For the data set
p <- ggplot(subset(climate, Source=="Berkeley"), aes(x=Year, y=Anomaly10y)) +
  geom_line()
p + annotate("segment", x=1950, xend=1980, y=-.25, yend=-.25)
#add the arrow
library(grid)
p + annotate("segment", x=1850, xend=1820, y=-.8, yend=-.95, colour="blue",
             size=2, arrow=arrow()) +
  annotate("segment", x=1950, xend=1980, y=-.25, yend=-.25,
           arrow=arrow(ends="both", angle=90, length=unit(.2,"cm")))

#Adding a Shaded Rectangle
library(gcookbook) # For the data set
p <- ggplot(subset(climate, Source=="Berkeley"), aes(x=Year, y=Anomaly10y)) +
  geom_line()

p + annotate("rect", xmin=1950, xmax=1980, ymin=-1, ymax=1, alpha=.1,
             fill="blue")

#Highlighting an Item
pg <- PlantGrowth # Make a copy of the PlantGrowth data
pg$hl <- "no" # Set all to "no"
pg$hl[pg$group=="trt2"] <- "yes" # If group is "trt2", set to "yes"
#Highlighting one item
ggplot(pg, aes(x=group, y=weight, fill=hl)) + geom_boxplot() +
  scale_fill_manual(values=c("grey85", "#FFDDCC"), guide=FALSE)
#same as above
ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot() +
  scale_fill_manual(values=c("grey85", "grey85", "#FFDDCC"), guide=FALSE)

#Adding Error Bars
library(gcookbook) # For the data set
# Take a subset of the cabbage_exp data for this example
ce <- subset(cabbage_exp, Cultivar == "c39")
# With a bar graph
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_bar(fill="white", colour="black", stat="identity") +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2)
# With a line graph
ggplot(ce, aes(x=Date, y=Weight)) +
  geom_line(aes(group=1)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se), width=.2)
#data set
ce
cabbage_exp

# Bad: dodge width not specified
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se),
                position="dodge", width=.2)
# Good: dodge width set to same as bar width (0.9)
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(position="dodge", stat="identity") +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se),
                position=position_dodge(0.9), width=.2)

# Save the dodge spec because we use it repeatedly
pd <- position_dodge(.3) 
#Error bars on a line graph, dodged so they don’t overlap
#Thinner error bar lines with size=0.25, and larger points with size=2.5
ggplot(cabbage_exp, aes(x=Date, y=Weight, colour=Cultivar, group=Cultivar)) +
  geom_errorbar(aes(ymin=Weight-se, ymax=Weight+se),
                width=.2, size=0.25, colour="black", position=pd) +
  geom_line(position=pd) +
  geom_point(position=pd, size=2.5)

#Adding Annotations to Individual Facets
# The base plot
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point() + facet_grid(. ~ drv)
# A data frame with labels for each facet
f_labels <- data.frame(drv = c("4", "f", "r"), label = c("4wd", "Front", "Rear"))
p + geom_text(x=6, y=40, aes(label=label), data=f_labels)
# If you use annotate(), the label will appear in all facets
p + annotate("text", x=6, y=42, label="label text")

# This function returns a data frame with strings representing the regression
# equation, and the r^2 value
# These strings will be treated as R math expressions
lm_labels <- function(dat) {
  mod <- lm(hwy ~ displ, data=dat)
  formula <- sprintf("italic(y) == %.2f %+.2f * italic(x)",
                     round(coef(mod)[1], 2), round(coef(mod)[2], 2))
  r <- cor(dat$displ, dat$hwy)
  r2 <- sprintf("italic(R^2) == %.2f", r^2)
  data.frame(formula=formula, r2=r2, stringsAsFactors=FALSE)
}
library(plyr) # For the ddply() function
labels <- ddply(mpg, "drv", lm_labels)
labels

# Plot with formula and R^2 values
p + geom_smooth(method=lm, se=FALSE) +
  geom_text(x=3, y=40, aes(label=formula), data=labels, parse=TRUE, hjust=0) +
  geom_text(x=3, y=35, aes(label=r2), data=labels, parse=TRUE, hjust=0)
# Find r^2 values for each group
#Annotations in each facet with information about the data
labels <- ddply(mpg, "drv", summarise, r2 = cor(displ, hwy)^2)
labels$r2 <- sprintf("italic(R^2) == %.2f", labels$r2)

#Swapping X- and Y-Axes
#Use coord_flip() to flip the axes 
ggplot(PlantGrowth, aes(x=group, y=weight)) + 
  geom_boxplot()

ggplot(PlantGrowth, aes(x=group, y=weight)) + 
  geom_boxplot() + 
  coord_flip()
#reversed by using scale_x_discrete() with limits=rev(levels(...)),
ggplot(PlantGrowth, aes(x=group, y=weight)) + 
  geom_boxplot() + 
  coord_flip() +
  scale_x_discrete(limits=rev(levels(PlantGrowth$group)))

#box plot with default range
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
p
#box plot with manually set range
p + ylim(0, max(PlantGrowth$weight))
#ylim() is shorthand for setting the limits with scale_y_continuous().
ylim(0, 10)
scale_y_continuous(limits=c(0, 10))
#only the second directive has any effect
p + ylim(0, 10) + scale_y_continuous(breaks=NULL)
p + scale_y_continuous(breaks=NULL) + ylim(0, 10)
#To make both changes work, get rid of ylim() and set both limits and breaks in scale_y_continuous():
p + scale_y_continuous(limits=c(0, 10), breaks=NULL)
#smaller y range using a scale (data has been dropped, so the box plots have changed shape);
p + scale_y_continuous(limits = c(5, 6.5)) # Same as using ylim()
#“zooming in” using a coordinate transform
p + coord_cartesian(ylim = c(5, 6.5))
#expand the range in one direction, using expand_limits()
#Box plot on which y range has been expanded to include 0
p + expand_limits(y=0)

#Reversing a Continuous Axis
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + scale_y_reverse()
# Similar effect by specifying limits in reversed order
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() + ylim(6.5, 3.5)
#reverse an axis and set its range within the scale_y_reverse() statement
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() +
  scale_y_reverse(limits=c(8, 0))

#Changing the Order of Items on a Categorical Axis.
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + 
  geom_boxplot()
#box plot with manually specified items on the x-axis;
p + scale_x_discrete(limits=c("trt1","ctrl","trt2"))
#box plot with only two items
p + scale_x_discrete(limits=c("ctrl","trt1"))
#Box plot with order reversed on the x-axis
p + scale_x_discrete(limits=rev(levels(PlantGrowth$group)))

#Setting the Scaling Ratio of the X- and Y-Axes
library(gcookbook) # For the data set
sp <- ggplot(marathon, aes(x=Half,y=Full)) + geom_point()
#scatter plot with equal scaling of axes
sp + coord_fixed()
#plot with tick marks at specified positions
sp + coord_fixed() +
  scale_y_continuous(breaks=seq(0, 420, 30)) +
  scale_x_continuous(breaks=seq(0, 420, 30))
#Scatter plot with a 1/2 scaling ratio for the axes
sp + coord_fixed(ratio=1/2) +
  scale_y_continuous(breaks=seq(0, 420, 30)) +
  scale_x_continuous(breaks=seq(0, 420, 15))

#Setting the Positions of Tick Marks
#box plot with automatic tick marks;
ggplot(PlantGrowth, aes(x=group, y=weight)) + 
  geom_boxplot()
#box plot with manually set tick marks
ggplot(PlantGrowth, aes(x=group, y=weight)) + 
  geom_boxplot() +
  scale_y_continuous(breaks=c(4, 4.25, 4.5, 5, 6, 8))
#use the seq() function or the : operator to generate vectors for tick marks
seq(4, 7, by=.5)
5:10
#For a discrete axis, setting limits reorders and removes items, and setting breaks controls which items have labels
# Set both breaks and labels for a discrete axis
ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() +
  scale_x_discrete(limits=c("trt2", "ctrl"), breaks="ctrl")

# Removing Tick Marks and Labels  
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
#no tick labels on y-axis;
p + theme(axis.text.y = element_blank())
#no tick marks and no tick labels on yaxis
p + theme(axis.ticks = element_blank(), axis.text.y = element_blank())
#with breaks=NULL
p + scale_y_continuous(breaks=NULL)

#Changing the Text of Tick Labels
library(gcookbook) # For the data set
hwp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) +
  geom_point()
hwp
#change the text of tick labels
hwp + scale_y_continuous(breaks=c(50, 56, 60, 66, 72),
                         labels=c("Tiny", "Really\nshort", "Short",
                                  "Medium", "Tallish"))
footinch_formatter <- function(x) {
  foot <- floor(x/12)
  inch <- x %% 12
  return(paste(foot, "'", inch, "\"", sep=""))
}

footinch_formatter(56:64)
#scatter plot with a formatter function
hwp + scale_y_continuous(labels=footinch_formatter)
#scatter plot with manually specified breaks on the y-axis
hwp + scale_y_continuous(breaks=seq(48, 72, 4), labels=footinch_formatter)
timeHMS_formatter <- function(x) {
  h <- floor(x/60)
  m <- floor(x %% 60)
  s <- round(60*(x %% 1)) # Round to nearest second
  lab <- sprintf("%02d:%02d:%02d", h, m, s) # Format the strings as HH:MM:SS
  lab <- gsub("^00:", "", lab) # Remove leading 00: if present
  lab <- gsub("^0", "", lab) # Remove leading 0 if present
  return(lab)
}

timeHMS_formatter(c(.33, 50, 51.25, 59.32, 60, 60.1, 130.23))

#Changing the Appearance of Tick Labels
bp <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot() +
  scale_x_discrete(breaks=c("ctrl", "trt1", "trt2"),
                   labels=c("Control", "Treatment 1", "Treatment 2"))
bp
#rotate the text 90 degrees counterclockwise 
bp + theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5))
#rotate the text 30 degrees counterclockwise 
bp + theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1))
#other text properties can be set with element_text()
bp + theme(axis.text.x = element_text(family="Times", face="italic",
                                      colour="darkred", size=rel(0.9)))

#Changing the Text of Axis Labels
library(gcookbook) # For the data set
hwp <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
  geom_point()
# With default axis labels
hwp
# Set the axis labels
hwp + xlab("Age in years") + ylab("Height in inches")
#Instead of xlab() and ylab(), you can use labs():
hwp + labs(x = "Age in years", y = "Height in inches")
#Another way of setting the axis labels is in the scale specification
hwp + scale_x_continuous(name="Age in years")
#You can also add line breaks with \n
hwp + scale_x_continuous(name="Age\n(years)")

#Removing Axis Labels
p <- ggplot(PlantGrowth, aes(x=group, y=weight)) + geom_boxplot()
p + theme(axis.title.x=element_blank())
#remove x label
p + xlab("")
#remove all labels
p + labs(x="", y = "")

# Changing the Appearance of Axis Labels  
library(gcookbook) # For the data set
hwp <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
hwp + theme(axis.title.x=element_text(face="italic", colour="darkred", size=14))
#The \n in the label represents a newline character
hwp + ylab("Height\n(inches)") +
  theme(axis.title.y=element_text(angle=0, face="italic", size=14))
#set y-axis label with angle=90 and color = darkred
hwp + ylab("Height\n(inches)") +
  theme(axis.title.y=element_text(angle=90, face="italic", colour="darkred",
                                  size=14))
#Showing Lines Along the Axes
library(gcookbook) # For the data set
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
#scatter plot with axis lines
p + theme(axis.line = element_line(colour="black"))
#scatter plot with theme_bw(), panel.border must also be made blank
p + theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(colour="black"))
# With thick lines, only half overlaps
p + theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(colour="black", size=4))
# Full overlap
p + theme_bw() +
  theme(panel.border = element_blank(),
        axis.line = element_line(colour="black", size=4, lineend="square"))

#Using a Logarithmic Axis  
library(MASS) # For the data set
# The base plot
p <- ggplot(Animals, aes(x=body, y=brain, label=rownames(Animals))) +
  geom_text(size=3)
p
# With logarithmic x and y scales
p + scale_x_log10() + scale_y_log10()

#data set
Animals
10^(0:3)
10^(-1:5)
#use above values as the breaks
p + scale_x_log10(breaks=10^(-1:5)) + scale_y_log10(breaks=10^(0:3))
#use the trans_format() function, from the scales package:
library(scales)
p + scale_x_log10(breaks=10^(-1:5),
                  labels=trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks=10^(0:3),
                labels=trans_format("log10", math_format(10^.x)))
#transform the data before mapping it to the x and y  coordinates
ggplot(Animals, aes(x=log10(body), y=log10(brain), label=rownames(Animals))) +
  geom_text(size=3)

library(scales)
# Use natural log on x, and log2 on y
p + scale_x_continuous(trans = log_trans(),
                       breaks = trans_breaks("log", function(x) exp(x)),
                       labels = trans_format("log", math_format(e^.x))) +
  scale_y_continuous(trans = log2_trans(),
                     breaks = trans_breaks("log2", function(x) 2^x),
                     labels = trans_format("log2", math_format(2^.x)))
#use a log axis for just one axis.
library(gcookbook) # For the data set
ggplot(aapl, aes(x=date,y=adj_price)) + geom_line()
ggplot(aapl, aes(x=date,y=adj_price)) + geom_line() +
  scale_y_log10(breaks=c(2,10,50,250))

#Adding Ticks for a Logarithmic Axis
#Use annotation_logticks() 
library(MASS) # For the data set
library(scales) # For the trans and format functions
ggplot(Animals, aes(x=body, y=brain, label=rownames(Animals))) +
  geom_text(size=3) +
  annotation_logticks() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)))
#manually set the scale’s minor_breaks (set them to log10(5*10^(minpow:maxpow), which reduces to log10(5) + minpow:max pow ())
ggplot(Animals, aes(x=body, y=brain, label=rownames(Animals))) +
  geom_text(size=3) +
  annotation_logticks() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                minor_breaks = log10(5) + -2:5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x)),
                minor_breaks = log10(5) + -1:3) +
  coord_fixed() +
  theme_bw()

#Making a Circular Graph
library(gcookbook) # For the data set
wind
#Circular Graph
ggplot(wind, aes(x=DirCat, fill=SpeedCat)) +
  geom_histogram(binwidth=15, origin=-7.5) +
  coord_polar() +
  scale_x_continuous(limits=c(0,360))
#make the plot a little prettier by reversing the legend, using a different palette, adding an outline, and setting the breaks to some more familiar numbers
ggplot(wind, aes(x=DirCat, fill=SpeedCat)) +
  geom_histogram(binwidth=15, origin=-7.5, colour="black", size=.25) +
  guides(fill=guide_legend(reverse=TRUE)) +
  coord_polar() +
  scale_x_continuous(limits=c(0,360), breaks=seq(0, 360, by=45),
                     minor_breaks=seq(0, 360, by=15)) +
  scale_fill_brewer()
#convert adjustment in degrees to radians
coord_polar(start=-45 * pi / 180)
# Put mdeaths time series data into a data frame
md <- data.frame(deaths = as.numeric(mdeaths),
                 month = as.numeric(cycle(mdeaths)))
# Calculate average number of deaths in each month
library(plyr) # For the ddply() function
md <- ddply(md, "month", summarise, deaths = mean(deaths))
md

# Make the base plot
p <- ggplot(md, aes(x=month, y=deaths)) + 
  geom_line() +
  scale_x_continuous(breaks=1:12)
# With coord_polar
p + coord_polar()
# With coord_polar and y (r) limits going to zero
p + coord_polar() + ylim(0, max(md$deaths))

p + coord_polar() + ylim(0, max(md$deaths)) + xlim(0, 12)
# Connect the lines by adding a value for 0 that is the same as 12
mdx <- md[md$month==12, ]
mdx$month <- 0
mdnew <- rbind(mdx, md)
# Make the same plot as before, but with the new data, by using %+%
p %+% mdnew + coord_polar() + ylim(0, max(md$deaths))

#Using Dates on an Axis
# Look at the structure
str(economics)
ggplot(economics, aes(x=date, y=psavert)) + geom_line()
# Take a subset of economics
econ <- subset(economics, date >= as.Date("1992-05-01") &
                 date < as.Date("1993-06-01"))
# Base plot - without specifying breaks
p <- ggplot(econ, aes(x=date, y=psavert)) + geom_line()
p
# Specify breaks as a Date vector
datebreaks <- seq(as.Date("1992-06-01"), as.Date("1993-06-01"), by="2 month")
# Use breaks, and rotate text labels
p + scale_x_date(breaks=datebreaks) +
  theme(axis.text.x = element_text(angle=30, hjust=1))
#specify the formatting by using the date_format() function from the scales package
library(scales)
p + scale_x_date(breaks=datebreaks, labels=date_format("%Y %b")) +
  theme(axis.text.x = element_text(angle=30, hjust=1))
# Mac and Linux
Sys.setlocale("LC_TIME", "it_IT.UTF-8")
# Windows
Sys.setlocale("LC_TIME", "italian")

#Using Relative Times on an Axis
# Convert WWWusage time-series object to data frame
www <- data.frame(minute = as.numeric(time(WWWusage)),
                  users = as.numeric(WWWusage))
# Define a formatter function - converts time in minutes to a string
timeHM_formatter <- function(x) {
  h <- floor(x/60)
  m <- floor(x %% 60)
  lab <- sprintf("%d:%02d", h, m) # Format the strings as HH:MM
  return(lab)
}
# Default x axis
ggplot(www, aes(x=minute, y=users)) + geom_line()
# With formatted times
ggplot(www, aes(x=minute, y=users)) + geom_line() +
  scale_x_continuous(name="time", breaks=seq(0, 100, by=10),
                     labels=timeHM_formatter)
#specify the breaks and labels manually
scale_x_continuous(breaks=c(0, 20, 40, 60, 80, 100),
                   labels=c("0:00", "0:20", "0:40", "1:00", "1:20", "1:40"))

timeHM_formatter(c(0, 50, 51, 59, 60, 130, 604))

timeHMS_formatter <- function(x) {
  h <- floor(x/3600)
  m <- floor((x/60) %% 60)
  s <- round(x %% 60) # Round to nearest second
  lab <- sprintf("%02d:%02d:%02d", h, m, s) # Format the strings as HH:MM:SS
  lab <- sub("^00:", "", lab) # Remove leading 00: if present
  lab <- sub("^0", "", lab) # Remove leading 0 if present
  return(lab)
}
#Running it on some sample numbers yields:
timeHMS_formatter(c(20, 3000, 3075, 3559.2, 3600, 3606, 7813.8))

# Setting the Title of a Graph 
#Set title with ggtitle(), as shown in Figure 9-1:
library(gcookbook) # For the data set
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
p + ggtitle("Age and Height of Schoolchildren")
# Use \n for a newline
p + ggtitle("Age and Height\nof Schoolchildren")
#ggtitle() is equivalent to using labs(title = "Title text").

# Move the title inside
p + ggtitle("Age and Height of Schoolchildren") +
  theme(plot.title=element_text(vjust = -2.5))
# Use a text annotation instead
p + annotate("text", x=mean(range(heightweight$ageYear)), y=Inf,
             label="Age and Height of Schoolchildren", vjust=1.5, size=6)

#Changing the Appearance of Text
library(gcookbook) # For the data set
# Base plot
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
# rel(1.5) means that the font will be 1.5 times the base font size of the theme.
# For theme elements, font size is in points.
# Controlling appearance of theme items
p + theme(axis.title.x=element_text(size=16, lineheight=.9, family="Times",
                                    face="bold.italic", colour="red"))
p + ggtitle("Age and Height\nof Schoolchildren") +
  theme(plot.title=element_text(size=rel(1.5), lineheight=.9, family="Times",
                                face="bold.italic", colour="red"))

# For text geoms, font size is in mm  
p + annotate("text", x=15, y=53, label="Some text", size = 7, family="Times",
             fontface="bold.italic", colour="red")
p + geom_text(aes(label=weightLb), size=4, family="Times", colour="red")

#Using Themes
#To use a premade theme, add theme_bw() or theme_grey()
library(gcookbook) # For the data set
# Base plot
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
# Grey theme (the default)
p + theme_grey()
# Black-and-white theme
p + theme_bw()

p + theme_grey(base_size=16, base_family="Times")
# Set default theme for current session
theme_set(theme_bw())
# This will use theme_bw()
p
# Reset the default theme back to theme_grey()
theme_set(theme_grey())

#Changing the Appearance of Theme Elements
library(gcookbook) # For the data set
# Base plot
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
# Options for the plotting area
p + theme(
  panel.grid.major = element_line(colour="red"),
  panel.grid.minor = element_line(colour="red", linetype="dashed", size=0.2),
  panel.background = element_rect(fill="lightblue"),
  panel.border = element_rect(colour="blue", fill=NA, size=2))
# Options for text items
p + ggtitle("Plot title here") +
  theme(
    axis.title.x = element_text(colour="red", size=14),
    axis.text.x = element_text(colour="blue"),
    axis.title.y = element_text(colour="red", size=14, angle = 90),
    axis.text.y = element_text(colour="blue"),
    plot.title = element_text(colour="red", size=20, face="bold"))
# Options for the legend
p + theme(
  legend.background = element_rect(fill="grey85", colour="red", size=1),
  legend.title = element_text(colour="blue", face="bold", size=14),
  legend.text = element_text(colour="red"),
  legend.key = element_rect(colour="blue", size=0.25))
# Options for facets
p + facet_grid(sex ~ .) + theme(
  strip.background = element_rect(fill="pink"),
  strip.text.y = element_text(size=14, angle=-90, face="bold"))  # strip.text.x is the same, but for horizontal facets
# theme() has no effect if before adding a complete theme
p + theme(axis.title.x = element_text(colour="red")) + theme_bw()
# theme() works if after a compete theme
p + theme_bw() + theme(axis.title.x = element_text(colour="red", size=12))

#Creating Your Own Themes
library(gcookbook) # For the data set
# Start with theme_bw() and modify a few things
mytheme <- theme_bw() +
  theme(text = element_text(colour="red"),
        axis.title = element_text(size = rel(1.25)))
# Base plot
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
# Plot with modified theme
p + mytheme

#Hiding Grid Lines
library(gcookbook) # For the data set
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn)) + geom_point()
p + theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
# Hide the vertical grid lines (which intersect with the x-axis)
p + theme(panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
# Hide the horizontal grid lines (which intersect with the y-axis)
p + theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank())

#Removing the Legend
# The base plot (with legend)
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
p
# Remove the legend for fill
p + guides(fill=FALSE)
# Remove the legend for fill
p + scale_fill_discrete(guide=FALSE)
#remove legends for all of aesthetic mapping with a legend (color and shape, for example),
p + theme(legend.position="none")

#Commonly used scales include:
#• scale_fill_discrete()
#• scale_fill_hue()
#• scale_fill_manual()
#• scale_fill_grey()
#• scale_fill_brewer()
#• scale_colour_discrete()
#• scale_colour_hue()
#• scale_colour_manual()
#• scale_colour_grey()
#• scale_colour_brewer()
#• scale_shape_manual()
#• scale_linetype()

#Changing the Position of a Legend
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot() +
  scale_fill_brewer(palette="Pastel2")
#legend on top
p + theme(legend.position="top")
#legend in bottom-right corner;
p + theme(legend.position=c(1,0), legend.justification=c(1,0))
#legend in top-right corner
p + theme(legend.position=c(1,1), legend.justification=c(1,1))
#add an opaque border
p + theme(legend.position=c(.85,.2)) +
  theme(legend.background=element_rect(fill="white", colour="black"))
#remove the border around its elements
p + theme(legend.position=c(.85,.2)) +
  theme(legend.background=element_blank()) + # Remove overall border
  theme(legend.key=element_blank()) # Remove border around each item

#Changing the Order of Items in a Legend
# The base plot
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
p
# Change the order of items
p + scale_fill_discrete(limits=c("trt1", "trt2", "ctrl"))
#grey palette
p + scale_fill_grey(start=.5, end=1, limits=c("trt1", "trt2", "ctrl"))
#use a palette from RColorBrewer
p + scale_fill_brewer(palette="Pastel2", limits=c("trt1", "trt2", "ctrl"))

#Commonly used scales include:
#• scale_fill_discrete()
#• scale_fill_hue()
#• scale_fill_manual()
#• scale_fill_grey()
#• scale_fill_brewer()
#• scale_colour_discrete()
#• scale_colour_hue()
#• scale_colour_manual()
#• scale_colour_grey()
#• scale_colour_brewer()
#• scale_shape_manual()
#• scale_linetype()

#Reversing the Order of Items in a Legend
# The base plot
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
p
# Reverse the legend order
p + guides(fill=guide_legend(reverse=TRUE))

#Changing a Legend Title
# The base plot
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
p
# Set the legend title to "Condition"
p + labs(fill="Condition")
#same as above
p + scale_fill_discrete(name="Condition")

library(gcookbook) # For the data set
# Make the base plot
hw <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
  geom_point(aes(size=weightLb)) + scale_size_continuous(range=c(1,4))
hw
# With new legend titles
hw + labs(colour="Male/Female", size="Weight\n(pounds)")
#default is to have a single legend that combines both
hw1 <- ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) +
  geom_point()
hw1

# Change just shape
hw1 + labs(shape="Male/Female")
# Change both shape and colour
hw1 + labs(shape="Male/Female", colour="Male/Female")

#control the legend title with the guides() function
p + guides(fill=guide_legend(title="Condition"))

#Removing a Legend Title
ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot() +
  guides(fill=guide_legend(title=NULL))
#control the legend title when specifying the scale
scale_fill_hue(guide = guide_legend(title=NULL))

#Changing the Labels in a Legend
library(gcookbook) # For the data set
# The base plot
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
# Change the legend labels
p + scale_fill_discrete(labels=c("Control", "Treatment 1", "Treatment 2"))
#using other fill scales
p + scale_fill_grey(start=.5, end=1,
                    labels=c("Control", "Treatment 1", "Treatment 2"))
#change the item order, and make sure to set the labels in the same order
p + scale_fill_discrete(limits=c("trt1", "trt2", "ctrl"),
                        labels=c("Treatment 1", "Treatment 2", "Control"))
# The base plot
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, shape=sex, colour=sex)) +
  geom_point()
p
# Change the labels for one scale
p + scale_shape_discrete(labels=c("Female", "Male"))
# Change the labels for both scales
p + scale_shape_discrete(labels=c("Female", "Male")) +
  scale_colour_discrete(labels=c("Female", "Male"))

#Other commonly used scales with legends include:
#• scale_fill_discrete()
#• scale_fill_hue()
#• scale_fill_manual()
#• scale_fill_grey()
#• scale_fill_brewer()
#• scale_colour_discrete()
#• scale_colour_hue()
#• scale_colour_manual()
#• scale_colour_grey()
#• scale_colour_brewer()
#• scale_shape_manual()
#• scale_linetype()

#Changing the Appearance of Legend Labels
#Use theme(legend.text=element_text())
# The base plot
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
# Change the legend label appearance
p + theme(legend.text=element_text(face="italic", family="Times", colour="red",
                                   size=14))
# Changes the legend title text for the fill legend
p + guides(fill=guide_legend(label.theme=
                               element_text(face="italic", family="Times", colour="red", size=14, angle=0)))

#Using Labels with Multiple Lines of Text  
p <- ggplot(PlantGrowth, aes(x=group, y=weight, fill=group)) + geom_boxplot()
# Labels that have more than one line
p + scale_fill_discrete(labels=c("Control", "Type 1\ntreatment","Type 2\ntreatment"))
#specify the height using the unit() function from the grid package
library(grid)
p + scale_fill_discrete(labels=c("Control", "Type 1\ntreatment",
                                 "Type 2\ntreatment")) +
  theme(legend.text=element_text(lineheight=.8),
        legend.key.height=unit(1, "cm"))

# Splitting Data into Subplots with Facets
# The base plot
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
# Faceted by drv, in vertically arranged subpanels
p + facet_grid(drv ~ .)
# Faceted by cyl, in horizontally arranged subpanels
p + facet_grid(. ~ cyl)
# Split by drv (vertical) and cyl (horizontal)
p + facet_grid(drv ~ cyl)

# Facet on class
# Note there is nothing before the tilde
p + facet_wrap( ~ class)

# These will have the same result: 2 rows and 4 cols
p + facet_wrap( ~ class, nrow=2)
p + facet_wrap( ~ class, ncol=4)

#Using Facets with Different Axes
# The base plot
p <- ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()
# With free y scales
p + facet_grid(drv ~ cyl, scales="free_y")
# With free x and y scales
p + facet_grid(drv ~ cyl, scales="free")

#Changing the Text of Facet Labels
mpg2 <- mpg # Make a copy of the original data
# Rename 4 to 4wd, f to Front, r to Rear
levels(mpg2$drv)[levels(mpg2$drv)=="4"] <- "4wd"
levels(mpg2$drv)[levels(mpg2$drv)=="f"] <- "Front"
levels(mpg2$drv)[levels(mpg2$drv)=="r"] <- "Rear"
# Plot the new data
ggplot(mpg2, aes(x=displ, y=hwy)) + geom_point() + facet_grid(drv ~ .)
#use a labeller function to set the labels. label_both() will print out both the  name of the variable and the value of the variable in each facet
ggplot(mpg2, aes(x=displ, y=hwy)) + geom_point() +
  facet_grid(drv ~ ., labeller = label_both)

#label_parsed(), which takes strings and treats them as R math expressions
mpg3 <- mpg
levels(mpg3$drv)[levels(mpg3$drv)=="4"] <- "4^{wd}"
levels(mpg3$drv)[levels(mpg3$drv)=="f"] <- "- Front %.% e^{pi * i}"
levels(mpg3$drv)[levels(mpg3$drv)=="r"] <- "4^{wd} - Front"
ggplot(mpg3, aes(x=displ, y=hwy)) + geom_point() +
  facet_grid(drv ~ ., labeller = label_parsed)

#Changing the Appearance of Facet Labels and Headers  
library(gcookbook) # For the data set
ggplot(cabbage_exp, aes(x=Cultivar, y=Weight)) + geom_bar(stat="identity") +
  facet_grid(. ~ Date) +
  theme(strip.text = element_text(face="bold", size=rel(1.5)),
        strip.background = element_rect(fill="lightblue", colour="black",
                                        size=1))

#Setting the Colors of Objects
library(MASS) # For the data set

#setting colour for points
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point(colour="red")
#setting fill and colour
ggplot(birthwt, aes(x=bwt)) + geom_histogram(fill="red", colour="black")

#Mapping Variables to Colors
library(gcookbook) # For the data set
# These both have the same effect
ggplot(cabbage_exp, aes(x=Date, y=Weight, fill=Cultivar)) +
  geom_bar(colour="black", position="dodge", stat="identity")

ggplot(cabbage_exp, aes(x=Date, y=Weight)) +
  geom_bar(aes(fill=Cultivar), colour="black", position="dodge", stat="identity")
# These both have the same effect
ggplot(mtcars, aes(x=wt, y=mpg, colour=cyl)) + geom_point()
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point(aes(colour=cyl))

#data set
str(cabbage_exp)
# Convert to factor in call to ggplot()
ggplot(mtcars, aes(x=wt, y=mpg, colour=factor(cyl))) + geom_point()
# Another method: Convert to factor in the data
m <- mtcars # Make a copy of mtcars
m$cyl <- factor(m$cyl) # Convert cyl to a factor
ggplot(m, aes(x=wt, y=mpg, colour=cyl)) + geom_point()

#Using a Different Palette for a Discrete Variable
# page  Table 12-1
library(gcookbook) # For the data set
# Base plot
p <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
# These three have the same effect
p
p + scale_fill_discrete()
p + scale_fill_hue()
# ColorBrewer palette
p + scale_fill_brewer()

# Basic scatter plot
h <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) +
  geom_point()
# Default lightness = 65
h
# Slightly darker
h + scale_colour_hue(l=45) #scale_fill_hue(), the colors are taken from around the color wheel in the HCL (hue-chroma-lightness) color
#use another color
p + scale_fill_brewer(palette="Oranges")

#using the default grey palette;
p + scale_fill_grey()
# Reverse the direction and use a different range of greys
p + scale_fill_grey(start=0.7, end=0)

#Using a Manually Defined Palette for a Discrete Variable    
library(gcookbook) # For the data set
# Base plot
h <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=sex)) + geom_point()
# Using color names
h + scale_colour_manual(values=c("red", "blue"))
# Using RGB values
h + scale_colour_manual(values=c("#CC6666", "#7777DD")) #For fill scales, use scale_fill_manual() instead.

#data set
levels(heightweight$sex)

h + scale_colour_manual(values=c(m="blue", f="red"))

#  Using a Colorblind-Friendly Palette 
library(gcookbook) # For the data set
# Base plot
p <- ggplot(uspopage, aes(x=Year, y=Thousands, fill=AgeGroup)) + geom_area()
# The palette with grey:
cb_palette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442",
                "#0072B2", "#D55E00", "#CC79A7")
# Add it to the plot
p + scale_fill_manual(values=cb_palette)
#set color set
c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
  "#CC79A7")

#Using a Manually Defined Palette for a Continuous Variable  
library(gcookbook) # For the data set
# Base plot
p <- ggplot(heightweight, aes(x=ageYear, y=heightIn, colour=weightLb)) +
  geom_point(size=3)
p
# With a gradient between two colors
p + scale_colour_gradient(low="black", high="white")
# A gradient with a white midpoint
library(scales)
p + scale_colour_gradient2(low=muted("red"), mid="white", high=muted("blue"),
                           midpoint=110)
# A gradient of n colors
p + scale_colour_gradientn(colours = c("darkred", "orange", "yellow", "white"))

# Page 263 Table 12-2 

#Coloring a Shaded Region Based on Value
library(gcookbook) # For the data set
cb <- subset(climate, Source=="Berkeley")
cb$valence[cb$Anomaly10y >= 0] <- "pos"
cb$valence[cb$Anomaly10y < 0] <- "neg"
cb
#Mapping valence to fill color—notice the red area under the zero line  around 1950
ggplot(cb, aes(x=Year, y=Anomaly10y)) +
  geom_area(aes(fill=valence)) +
  geom_line() +
  geom_hline(yintercept=0)

# approx() returns a list with x and y vectors
interp <- approx(cb$Year, cb$Anomaly10y, n=1000)
# Put in a data frame and recalculate valence
cbi <- data.frame(Year=interp$x, Anomaly10y=interp$y)
cbi$valence[cbi$Anomaly10y >= 0] <- "pos"
cbi$valence[cbi$Anomaly10y < 0] <- "neg"
#Shaded regions with interpolated data
ggplot(cbi, aes(x=Year, y=Anomaly10y)) +
  geom_area(aes(fill=valence), alpha = .4) +
  geom_line() +
  geom_hline(yintercept=0) +
  scale_fill_manual(values=c("#CCEEFF", "#FFDDDD"), guide=FALSE) +
  scale_x_continuous(expand=c(0, 0))

#Making a Correlation Matrix
#data set
mtcars

mcor <- cor(mtcars)
# Print mcor and round to 2 digits
round(mcor, digits=2)

#using package corrplot
library(corrplot)
corrplot(mcor)
# Generate a lighter palette
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(mcor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black", addcolorlabel="no", order="AOE")
# Page 270 Table 13-1. Options for corrplot() 

#Plotting a Function
# The data frame is only used for setting the range
p <- ggplot(data.frame(x=c(-3,3)), aes(x=x))
#the normal distribution
p + stat_function(fun = dnorm)
#the t-distribution with df=2
p + stat_function(fun=dt, args=list(df=2))

myfun <- function(xvar) {
  1/(1 + exp(-xvar + 10))
}
ggplot(data.frame(x=c(0, 20)), aes(x=x)) + stat_function(fun=myfun)

#Shading a Subregion Under a Function Curve
# Return dnorm(x) for 0 < x < 2, and NA for all other x
dnorm_limit <- function(x) {
  y <- dnorm(x)
  y[x < 0 | x > 2] <- NA
  return(y)
}
# ggplot() with dummy data
p <- ggplot(data.frame(x=c(-3, 3)), aes(x=x))
p + stat_function(fun=dnorm_limit, geom="area", fill="blue", alpha=0.2) +
  stat_function(fun=dnorm)

#store a function
limitRange <- function(fun, min, max) {
  function(x) {
    y <- fun(x)
    y[x < min | x > max] <- NA
    return(y)
  }
}

# This returns a function
dlimit <- limitRange(dnorm, 0, 2)
# Now we'll try out the new function -- it only returns values for inputs between 0 and 2
dlimit(-2:4)
#use limitRange() to create a function that is passed to stat_function()
p + stat_function(fun = dnorm) +
  stat_function(fun = limitRange(dnorm, 0, 2),
                geom="area", fill="blue", alpha=0.2)

#  Creating a Network Graph  
# May need to install first, with install.packages("igraph")
library(igraph)
# Specify edges for a directed graph
gd <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6))
plot(gd)
# For an undirected graph
gu <- graph(c(1,2, 2,3, 2,4, 1,4, 5,5, 3,6), directed=FALSE)
# No labels
plot(gu, vertex.label=NA)

#data set
str(gd)
str(gu)

set.seed(229)
plot(gu)

library(gcookbook) # For the data set
#data set
madmen2
# Create a graph object from the data set
g <- graph.data.frame(madmen2, directed=TRUE)
# Remove unnecessary margins
par(mar=c(0,0,0,0))
plot(g, layout=layout.fruchterman.reingold, vertex.size=8, edge.arrow.size=0.5,
     vertex.label=NA)

g <- graph.data.frame(madmen, directed=FALSE)
par(mar=c(0,0,0,0)) # Remove unnecessary margins
plot(g, layout=layout.circle, vertex.size=8, vertex.label=NA)

#Using Text Labels in a Network Graph
library(igraph)
library(gcookbook) # For the data set
# Copy madmen and drop every other row
m <- madmen[1:nrow(madmen) %% 2 == 1, ]
g <- graph.data.frame(m, directed=FALSE)
# Print out the names of each vertex
V(g)$name

plot(g, layout=layout.fruchterman.reingold,
     vertex.size = 4, # Smaller nodes
     vertex.label = V(g)$name, # Set the labels
     vertex.label.cex = 0.8, # Slightly smaller font
     vertex.label.dist = 0.4, # Offset the labels
     vertex.label.color = "black")
# This is equivalent to the preceding code
V(g)$size <- 4
V(g)$label <- V(g)$name
V(g)$label.cex <- 0.8
V(g)$label.dist <- 0.4
V(g)$label.color <- "black"
# Set a property of the entire graph
g$layout <- layout.fruchterman.reingold
plot(g)

# View the edges
E(g)
# Set some of the labels to "M"
E(g)[c(2,11,19)]$label <- "M"
# Set color of all to grey, and then color a few red
E(g)$color <- "grey70"
E(g)[c(2,11,19)]$color <- "red"
plot(g)

#Creating a Heat Map
#data set
presidents
str(presidents)

pres_rating <- data.frame(
  rating = as.numeric(presidents),
  year = as.numeric(floor(time(presidents))),
  quarter = as.numeric(cycle(presidents))
)
pres_rating

# Base plot
p <- ggplot(pres_rating, aes(x=year, y=quarter, fill=rating))
# Using geom_tile()
p + geom_tile()
# Using geom_raster() - looks the same, but a little more efficient
p + geom_raster()
#A heat map with customized appearance
p + geom_tile() +
  scale_x_continuous(breaks = seq(1940, 1976, by = 4)) +
  scale_y_reverse() +
  scale_fill_gradient2(midpoint=50, mid="grey70", limits=c(0,100))

#Creating a Three-Dimensional Scatter Plot 
# You may need to install first, with install.packages("rgl")
library(rgl)
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, type="s", size=0.75, lit=FALSE)

# Function to interleave the elements of two vectors
interleave <- function(v1, v2) as.vector(rbind(v1,v2))
# Plot the points
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,
       xlab="Weight", ylab="Displacement", zlab="MPG",
       size=.75, type="s", lit=FALSE)
# Add the segments
segments3d(interleave(mtcars$wt, mtcars$wt),
           interleave(mtcars$disp, mtcars$disp),
           interleave(mtcars$mpg, min(mtcars$mpg)),
           alpha=0.4, col="blue")

# Make plot without axis ticks or labels
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,
       xlab = "", ylab = "", zlab = "",
       axes = FALSE,
       size=.75, type="s", lit=FALSE)
segments3d(interleave(mtcars$wt, mtcars$wt),
           interleave(mtcars$disp, mtcars$disp),
           interleave(mtcars$mpg, min(mtcars$mpg)),
           alpha = 0.4, col = "blue")
# Draw the box.
rgl.bbox(color="grey50", # grey60 surface and black text
         emission="grey50", # emission color is grey50
         xlen=0, ylen=0, zlen=0) # Don't add tick marks
# Set default color of future objects to black
rgl.material(color="black")
# Add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++".
axes3d(edges=c("x--", "y+-", "z--"),
       ntick=6, # Attempt 6 tick marks on each side
       cex=.75) # Smaller font
# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d("Weight", edge="x--", line=2)
mtext3d("Displacement", edge="y+-", line=3)
mtext3d("MPG", edge="z--", line=3)

#Adding a Prediction Surface to a Three-Dimensional Plot  
# Given a model, predict zvar from xvar and yvar
# Defaults to range of x and y variables, and a 16x16 grid
predictgrid <- function(model, xvar, yvar, zvar, res = 16, type = NULL) {
  # Find the range of the predictor variable. This works for lm and glm
  # and some others, but may require customization for others.
  xrange <- range(model$model[[xvar]])
  yrange <- range(model$model[[yvar]])
  newdata <- expand.grid(x = seq(xrange[1], xrange[2], length.out = res),
                         y = seq(yrange[1], yrange[2], length.out = res))
  names(newdata) <- c(xvar, yvar)
  newdata[[zvar]] <- predict(model, newdata = newdata, type = type)
  newdata
}
# Convert long-style data frame with x, y, and z vars into a list
# with x and y as row/column values, and z as a matrix.
df2mat <- function(p, xvar = NULL, yvar = NULL, zvar = NULL) {
  if (is.null(xvar)) xvar <- names(p)[1]
  if (is.null(yvar)) yvar <- names(p)[2]
  if (is.null(zvar)) zvar <- names(p)[3]
  x <- unique(p[[xvar]])
  y <- unique(p[[yvar]])
  z <- matrix(p[[zvar]], nrow = length(y), ncol = length(x))
  m <- list(x, y, z)
  names(m) <- c(xvar, yvar, zvar)
  m
}
# Function to interleave the elements of two vectors
interleave <- function(v1, v2) as.vector(rbind(v1,v2))

library(rgl)
# Make a copy of the data set
m <- mtcars
# Generate a linear model
mod <- lm(mpg ~ wt + disp + wt:disp, data = m)
# Get predicted values of mpg from wt and disp
m$pred_mpg <- predict(mod)
# Get predicted mpg from a grid of wt and disp
mpgrid_df <- predictgrid(mod, "wt", "disp", "mpg")
mpgrid_list <- df2mat(mpgrid_df)
# Make the plot with the data points
plot3d(m$wt, m$disp, m$mpg, type="s", size=0.5, lit=FALSE)
# Add the corresponding predicted points (smaller)
spheres3d(m$wt, m$disp, m$pred_mpg, alpha=0.4, type="s", size=0.5, lit=FALSE)
# Add line segments showing the error
segments3d(interleave(m$wt, m$wt),
           interleave(m$disp, m$disp),
           interleave(m$mpg, m$pred_mpg),
           alpha=0.4, col="red")
# Add the mesh of predicted values
surface3d(mpgrid_list$wt, mpgrid_list$disp, mpgrid_list$mpg,
          alpha=0.4, front="lines", back="lines")

#3D scatter plot with axis ticks and labels repositioned
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg,
       xlab = "", ylab = "", zlab = "",
       axes = FALSE,
       size=.5, type="s", lit=FALSE)
# Add the corresponding predicted points (smaller)
spheres3d(m$wt, m$disp, m$pred_mpg, alpha=0.4, type="s", size=0.5, lit=FALSE)
# Add line segments showing the error
segments3d(interleave(m$wt, m$wt),
           interleave(m$disp, m$disp),
           interleave(m$mpg, m$pred_mpg),
           alpha=0.4, col="red")
# Add the mesh of predicted values
surface3d(mpgrid_list$wt, mpgrid_list$disp, mpgrid_list$mpg,
          alpha=0.4, front="lines", back="lines")
# Draw the box
rgl.bbox(color="grey50", # grey60 surface and black text
         emission="grey50", # emission color is grey50
         xlen=0, ylen=0, zlen=0) # Don't add tick marks
# Set default color of future objects to black
rgl.material(color="black")
# Add axes to specific sides. Possible values are "x--", "x-+", "x+-", and "x++".
axes3d(edges=c("x--", "y+-", "z--"),
       ntick=6, # Attempt 6 tick marks on each side
       cex=.75) # Smaller font
# Add axis labels. 'line' specifies how far to set the label from the axis.
mtext3d("Weight", edge="x--", line=2)
mtext3d("Displacement", edge="y+-", line=3)
mtext3d("MPG", edge="z--", line=3)

#Saving a Three-Dimensional Plot
library(rgl)
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, type="s", size=0.75, lit=FALSE)
rgl.snapshot('3dplot.png', fmt='png')
# use rgl.postscript() to save a Postscript or PDF file:
rgl.postscript('figs/miscgraph/3dplot.pdf', fmt='pdf')
rgl.postscript('figs/miscgraph/3dplot.ps', fmt='ps')

# Save the current viewpoint
view <- par3d("userMatrix")
# Restore the saved viewpoint
par3d(userMatrix = view)

dput(view)
structure(c(0.907931625843048, 0.267511069774628, -0.322642296552658,
            0, -0.410978674888611, 0.417272746562958, -0.810543060302734,
            0, -0.0821993798017502, 0.868516683578491, 0.488796472549438,
            0, 0, 0, 0, 1), .Dim = c(4L, 4L))

view <- structure(c(0.907931625843048, 0.267511069774628, -0.322642296552658,
                    0, -0.410978674888611, 0.417272746562958, -0.810543060302734,
                    0, -0.0821993798017502, 0.868516683578491, 0.488796472549438,
                    0, 0, 0, 0, 1), .Dim = c(4L, 4L))
par3d(userMatrix = view)

#  Animating a Three-Dimensional Plot  
library(rgl)
plot3d(mtcars$wt, mtcars$disp, mtcars$mpg, type="s", size=0.75, lit=FALSE)
play3d(spin3d())
#change the rotation axis, rotation speed, and duration:
# Spin on x-axis, at 4 rpm, for 20 seconds
play3d(spin3d(axis=c(1,0,0), rpm=4), duration=20)
#spin the plot once in 15 seconds, at 50 frames per second:
# Spin on z axis, at 4 rpm, for 15 seconds
movie3d(spin3d(axis=c(0,0,1), rpm=4), duration=15, fps=50)

#Creating a Dendrogram
library(gcookbook) # For the data set
# Get data from year 2009
c2 <- subset(countries, Year==2009)
# Drop rows that have any NA values
c2 <- c2[complete.cases(c2), ]
# Pick out a random 25 countries
# (Set random seed to make this repeatable)
set.seed(201)
c2 <- c2[sample(1:nrow(c2), 25), ]
c2

rownames(c2) <- c2$Name
c2 <- c2[,4:7]
c2

c3 <- scale(c2)
c3

hc <- hclust(dist(c3))
# Make the dendrogram
plot(hc)
# With text aligned
plot(hc, hang = -1)

#  Creating a Vector Field  
library(gcookbook) # For the data set
isabel
islice <- subset(isabel, z == min(z))
ggplot(islice, aes(x=x, y=y)) +
  geom_segment(aes(xend = x + vx/50, yend = y + vy/50),
               size = 0.25) # Make the line segments 0.25 mm thick

# Take a slice where z is equal to the minimum value of z
islice <- subset(isabel, z == min(z))
# Keep 1 out of every 'by' values in vector x
every_n <- function(x, by = 2) {
  x <- sort(x)
  x[seq(1, length(x), by = by)]
}
# Keep 1 of every 4 values in x and y
keepx <- every_n(unique(isabel$x), by=4)
keepy <- every_n(unique(isabel$y), by=4)
# Keep only those rows where x value is in keepx and y value is in keepy
islicesub <- subset(islice, x %in% keepx & y %in% keepy)
# taken a subset of the data, we can plot it, with arrowheads, as shown in
# Need to load grid for arrow() function
library(grid)
# Make the plot with the subset, and use an arrowhead 0.1 cm long
ggplot(islicesub, aes(x=x, y=y)) +
  geom_segment(aes(xend = x+vx/50, yend = y+vy/50),
               arrow = arrow(length = unit(0.1, "cm")), size = 0.25)

# The existing 'speed' column includes the z component. We'll calculate
# speedxy, the horizontal speed.
islicesub$speedxy <- sqrt(islicesub$vx^2 + islicesub$vy^2)
# Map speed to alpha
ggplot(islicesub, aes(x=x, y=y)) +
  geom_segment(aes(xend = x+vx/50, yend = y+vy/50, alpha = speed),
               arrow = arrow(length = unit(0.1,"cm")), size = 0.6)


# Get USA map data
usa <- map_data("usa")
# Map speed to colour, and set go from "grey80" to "darkred"
ggplot(islicesub, aes(x=x, y=y)) +
  geom_segment(aes(xend = x+vx/50, yend = y+vy/50, colour = speed),
               arrow = arrow(length = unit(0.1,"cm")), size = 0.6) +
  scale_colour_continuous(low="grey80", high="darkred") +
  geom_path(aes(x=long, y=lat, group=group), data=usa) +
  coord_cartesian(xlim = range(islicesub$x), ylim = range(islicesub$y))

# Keep 1 out of every 5 values in x and y, and 1 in 2 values in z
keepx <- every_n(unique(isabel$x), by=5)
keepy <- every_n(unique(isabel$y), by=5)
keepz <- every_n(unique(isabel$z), by=2)
isub <- subset(isabel, x %in% keepx & y %in% keepy & z %in% keepz)
ggplot(isub, aes(x=x, y=y)) +
  geom_segment(aes(xend = x+vx/50, yend = y+vy/50, colour = speed),
               arrow = arrow(length = unit(0.1,"cm")), size = 0.5) +
  scale_colour_continuous(low="grey80", high="darkred") +
  facet_wrap( ~ z)

#Creating a QQ Plot
library(gcookbook) # For the data set
# QQ plot of height
qqnorm(heightweight$heightIn)
qqline(heightweight$heightIn)
# QQ plot of age
qqnorm(heightweight$ageYear)
qqline(heightweight$ageYear)

#Creating a Graph of an Empirical Cumulative Distribution Function   
#Use stat_ecdf() 
library(gcookbook) # For the data set
# ecdf of heightIn
ggplot(heightweight, aes(x=heightIn)) + stat_ecdf()
# ecdf of ageYear
ggplot(heightweight, aes(x=ageYear)) + stat_ecdf()

#Creating a Mosaic Plot
#data set
UCBAdmissions
# Print a "flat" contingency table
ftable(UCBAdmissions)
dimnames(UCBAdmissions)

# You may need to install first, with install.packages("vcd")
library(vcd)
# Split by Admit, then Gender, then Dept
mosaic( ~ Admit + Gender + Dept, data=UCBAdmissions)

mosaic( ~ Dept + Gender + Admit, data=UCBAdmissions,
        highlighting="Admit", highlighting_fill=c("lightblue", "pink"),
        direction=c("v","h","v"))

# Another possible set of splitting directions
mosaic( ~ Dept + Gender + Admit, data=UCBAdmissions,
        highlighting="Admit", highlighting_fill=c("lightblue", "pink"),
        direction=c("v", "v", "h"))
# This order makes it difficult to compare male and female
mosaic( ~ Dept + Gender + Admit, data=UCBAdmissions,
        highlighting="Admit", highlighting_fill=c("lightblue", "pink"),
        direction=c("v", "h", "h"))

#Creating a Pie Chart
library(MASS) # For the data set
# Get a table of how many cases are in each level of fold
fold <- table(survey$Fold)
fold
# Make the pie chart
pie(fold)

pie(c(99, 18, 120), labels=c("L on R", "Neither", "R on L"))

#Creating a Map
library(maps) # For map data
library(mapproj)
# Get map data for USA
states_map <- map_data("state")
ggplot(states_map, aes(x=long, y=lat, group=group)) +
  geom_polygon(fill="white", colour="black")
# geom_path (no fill) and Mercator projection
ggplot(states_map, aes(x=long, y=lat, group=group)) +
  geom_path() + coord_map("mercator")

# The map_data() function returns a data frame with the following columns:
# long
#    Longitude.
# lat
#    Latitude.
# group
#    This is a grouping variable for each polygon. A region or subregion might have
#    multiple polygons, for example, if it includes islands.
# order
#    The order to connect each point within a group.
# region
#    Roughly, the names of countries, although some other objects are present (such as some lakes).
# subregion
#    The names of subregions within a region, which can contain multiple groups. For
#    example, the Alaska subregion includes many islands, each with its own group.

# Get map data for world
world_map <- map_data("world")
world_map
sort(unique(world_map$region))
# Map region to fill color
east_asia <- map_data("world", region=c("Japan", "China", "North Korea",
                                        "South Korea"))

ggplot(east_asia, aes(x=long, y=lat, group=group, fill=region)) +
  geom_polygon(colour="black") +
  scale_fill_brewer(palette="Set2")

# Get New Zealand data from world map
nz1 <- map_data("world", region="New Zealand")
nz1 <- subset(nz1, long > 0 & lat > -48) # Trim off islands
ggplot(nz1, aes(x=long, y=lat, group=group)) + geom_path()
# Get New Zealand data from the nz map
nz2 <- map_data("nz")
ggplot(nz2, aes(x=long, y=lat, group=group)) + geom_path()

#Creating a Choropleth Map
# Transform the USArrests data set to the correct format
crimes <- data.frame(state = tolower(rownames(USArrests)), USArrests)
crimes

library(maps) # For map data
states_map <- map_data("state")

# Merge the data sets together
crime_map <- merge(states_map, crimes, by.x="region", by.y="state")
# After merging, the order has changed, which would lead to polygons drawn in
# the incorrect order. So, we sort the data.
head(crime_map)

library(plyr) # For arrange() function
# Sort by group, then order
crime_map <- arrange(crime_map, group, order)
head(crime_map)
#mapping one of  the columns with data values to fill
ggplot(crime_map, aes(x=long, y=lat, group=group, fill=Assault)) +
  geom_polygon(colour="black") +
  coord_map("polyconic")

#use scale_fill_gradient2(), as shown in Figure 13-36:
ggplot(crimes, aes(map_id = state, fill=Assault)) +
  geom_map(map = states_map, colour="black") +
  scale_fill_gradient2(low="#559999", mid="grey90", high="#BB650B",
                       midpoint=median(crimes$Assault)) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic")

# Find the quantile bounds
qa <- quantile(crimes$Assault, c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
qa
# Add a column of the quantile category
crimes$Assault_q <- cut(crimes$Assault, qa,
                        labels=c("0-20%", "20-40%", "40-60%", "60-80%", "80-100%"),
                        include.lowest=TRUE)
crimes
# Generate a discrete color palette with 5 values
pal <- colorRampPalette(c("#559999", "grey80", "#BB650B"))(5)
pal
#Choropleth map with discretized data
ggplot(crimes, aes(map_id = state, fill=Assault_q)) +
  geom_map(map = states_map, colour="black") +
  scale_fill_manual(values=pal) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic") +
  labs(fill="Assault Rate\nPercentile")

# The 'state' column in the crimes data is to be matched to the 'region' column
# in the states_map data
ggplot(crimes, aes(map_id = state, fill=Assault)) +
  geom_map(map = states_map) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic")

#Making a Map with a Clean Background
# Create a theme with many of the background elements removed
theme_clean <- function(base_size = 12) {
  require(grid) # Needed for unit() function
  theme_grey(base_size) %+replace%
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.background = element_blank(),
      panel.grid = element_blank(),
      axis.ticks.length = unit(0, "cm"),
      axis.ticks.margin = unit(0, "cm"),
      panel.margin = unit(0, "lines"),
      plot.margin = unit(c(0, 0, 0, 0), "lines"),
      complete = TRUE
    )
}
#add it to one of the choropleths  we created
ggplot(crimes, aes(map_id = state, fill=Assault_q)) +
  geom_map(map = states_map, colour="black") +
  scale_fill_manual(values=pal) +
  expand_limits(x = states_map$long, y = states_map$lat) +
  coord_map("polyconic") +
  labs(fill="Assault Rate\nPercentile") +
  theme_clean()

#Creating a Map from a Shapefile
library(maptools)
library(rgdal)
# Load the shapefile and convert to a data frame
taiwan_shp <- readShapePoly("C:\\Users\\cli388\\Documents\\R Lib\\maptools\\Maps\\gadm2.shp")
taiwan_map <- fortify(taiwan_shp)
ggplot(taiwan_map, aes(x = long, y = lat, group=group)) + geom_path()

taiwan_shp <- readShapePoly("TWN_adm/TWN_adm2.shp")
# Look at the structure of the object
str(taiwan_shp)

taiwan_map <- fortify(taiwan_shp)
taiwan_map

# Send the SpatialPolygonsDataFrame directly to ggplot()
ggplot(taiwan_shp, aes(x=long, y=lat, group=group)) + geom_path()

#Outputting to PDF Vector Files
# width and height are in inches
pdf("myplot.pdf", width=4, height=4)
# Make plots
plot(mtcars$wt, mtcars$mpg)
print(ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point())
dev.off()
# 8x8 cm
pdf("myplot.pdf", width=8/2.54, height=8/2.54)

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
# Default is inches, but you can specify unit
ggsave("myplot.pdf", width=8, height=8, units="cm")

#Outputting to SVG Vector Files
svg("myplot.svg", width=4, height=4)
plot(...)
dev.off()
# With ggsave()
ggsave("myplot.svg", width=8, height=8, units="cm")

# Outputting to WMF Vector Files  
win.metafile("myplot.wmf", width=4, height=4)
plot(...)
dev.off()
# With ggsave()
ggsave("myplot.wmf", width=8, height=8, units="cm")

#Editing a Vector Output File
pdf("myplot.pdf", width=4, height=4, useDingbats=FALSE)
# or
ggsave("myplot.pdf", width=4, height=4, useDingbats=FALSE)

# width and height are in pixels
png("myplot.png", width=400, height=400)
# Make plot
plot(mtcars$wt, mtcars$mpg)
dev.off()

# width and height are in pixels
png("myplot-%d.png", width=400, height=400)
plot(mtcars$wt, mtcars$mpg)
print(ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point())
dev.off()

ppi <- 300
# Calculate the height and width (in pixels) for a 4x4-inch image at 300 ppi
png("myplot.png", width=4*ppi, height=4*ppi, res=ppi)
plot(mtcars$wt, mtcars$mpg)
dev.off()

ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()
# Default dimensions are in inches, but you can specify the unit
ggsave("myplot.png", width=8, height=8, unit="cm", dpi=300)

install.packages("Cairo") # One-time installation
CairoPNG("myplot.png")
plot(...)
dev.off()

#Using Fonts in PDF Files
library(extrafont)
# Find and save information about fonts installed on your system
font_import()
# List the fonts
fonts()
# Register the fonts with R
loadfonts()
# On Windows, you may need to tell it where Ghostscript is installed
# (adjust the path to match your installation of Ghostscript)
Sys.setenv(R_GSCMD = "C:/Program Files/gs/gs9.05/bin/gswin32c.exe")

library(ggplot2)
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
  ggtitle("Title text goes here") +
  theme(text = element_text(size = 16, family="Impact"))
ggsave("myplot.pdf", width=4, height=4)
embed_fonts("myplot.pdf")

#Using Fonts in Windows Bitmap or Screen Output
library(extrafont)
# Register the fonts for Windows
loadfonts("win")
#Finally, you can create each output file or display graphs on screen
library(ggplot2)
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point() +
  ggtitle("Title text goes here") +
  theme(text = element_text(size = 16, family="Georgia", face="italic"))

ggsave("myplot.png", width=4, height=4, dpi=300)


#Getting Your Data into Shape
library(gcookbook) # For the data set
#data set
heightweight
str(heightweight)

#Creating a Data Frame
# Two starting vectors
g <- c("A", "B", "C")
x <- 1:3
dat <- data.frame(g, x)
dat
#convert the list to a data frame with the as.data  .frame() function- 
list(group = g, value = x) # A list of vectors
dat <- as.data.frame(lst)

#Getting Information About a Data Structure
str(ToothGrowth)
tg <- ToothGrowth
tg$supp <- as.character(tg$supp)
str(tg)

# Print out the columns by themselves
# From old data frame (factor)
ToothGrowth$supp
# From new data frame (character)
tg$supp

#Adding a Column to a Data Frame
#adds a column named newcol, filled with NA
data$newcol <- NA
#assign a vector to the new column
data$newcol <- vec

#Deleting a Column from a Data Frame
#Assign NULL to that column:
data$badcol <- NULL
# Return data without badcol
data <- subset(data, select = -badcol)
# Exclude badcol and othercol
data <- subset(data, select = c(-badcol, -othercol))

#Renaming Columns in a Data Frame
#Use the names(dat) <- function:
names(dat) <- c("name1", "name2", "name3")

#rename the columns by name:
library(gcookbook) # For the data set
names(anthoming) # Print the names of the columns

names(anthoming)[names(anthoming) == "ctrl"] <- c("Control")
names(anthoming)[names(anthoming) == "expt"] <- c("Experimental")
names(anthoming)

#renamed by numeric position:
names(anthoming)[1] <- "Angle"
names(anthoming)

# Reordering Columns in a Data Frame  
#reorder columns by their numeric position:
dat <- dat[c(1,3,2)]
#reorder by column name:
dat <- dat[c("col1", "col3", "col2")]
library(gcookbook) # For the data set
anthoming
anthoming[c(1,3,2)] # List-style indexing
# Putting nothing before the comma means to select all rows
anthoming[, c(1,3,2)] # Matrix-style indexing
anthoming[3] # List-style indexing
anthoming[, 3] # Matrix-style indexing
anthoming[, 3, drop=FALSE] # Matrix-style indexing with drop=FALSE

# Getting a Subset of a Data Frame  
#use the climate data set for the examples here:
library(gcookbook) # For the data set
climate

subset(climate, Source == "Berkeley", select = c(Year, Anomaly10y))
subset(climate, Source == "Berkeley" & Year >= 1900 & Year <= 2000,
       select = c(Year, Anomaly10y))

climate[climate$Source=="Berkeley" & climate$Year >= 1900 & climate$Year <= 2000,
        c("Year", "Anomaly10y")]
climate[climate$Source=="Berkeley" & climate$Year >= 1900 & climate$Year <= 2000,
        c("Year", "Anomaly10y"), drop=FALSE]
climate[1:100, c(2, 5)]

#Changing the Order of Factor Levels
# By default, levels are ordered alphabetically
sizes <- factor(c("small", "large", "large", "small", "medium"))
sizes
# Change the order of levels
sizes <- factor(sizes, levels = c("small", "medium", "large"))
sizes

factor(sizes, levels = rev(levels(sizes)))

#Changing the Order of Factor Levels Based on Data Values  
# Make a copy since we'll modify it
iss <- InsectSprays
iss$spray

iss$spray <- reorder(iss$spray, iss$count, FUN=mean)
iss$spray

# Changing the Names of Factor Levels  
sizes <- factor(c( "small", "large", "large", "small", "medium"))
sizes

levels(sizes)
# With revalue(), pass it a named vector with the mappings
sizes1 <- revalue(sizes, c(small="S", medium="M", large="L"))
sizes1

# Can also use quotes -- useful if there are spaces or other strange characters
revalue(sizes, c("small"="S", "medium"="M", "large"="L"))
# mapvalues() lets you use two separate vectors instead of a named vector
mapvalues(sizes, c("small", "medium", "large"), c("S", "M", "L"))

sizes <- factor(c( "small", "large", "large", "small", "medium"))
# Index into the levels and rename each one
levels(sizes)[levels(sizes)=="large"] <- "L"
levels(sizes)[levels(sizes)=="medium"] <- "M"
levels(sizes)[levels(sizes)=="small"] <- "S"
sizes

sizes <- factor(c("small", "large", "large", "small", "medium"))
levels(sizes) <- list(S="small", M="medium", L="large")
sizes

# By default, levels are ordered alphabetically
sizes <- factor(c("small", "large", "large", "small", "medium"))

levels(sizes)[1] <- "L"
sizes

# Rename all levels at once
levels(sizes) <- c("L", "M", "S")
sizes

#Removing Unused Levels from a Factor
sizes <- factor(c("small", "large", "large", "small", "medium"))
sizes <- sizes[1:3]
sizes
#To remove them, use droplevels():
sizes <- droplevels(sizes)
sizes

#Changing the Names of Items in a Character Vector
sizes <- c("small", "large", "large", "small", "medium")
sizes

# With revalue(), pass it a named vector with the mappings
sizes1 <- revalue(sizes, c(small="S", medium="M", large="L"))
sizes1

# Can also use quotes -- useful if there are spaces or other strange characters
revalue(sizes, c("small"="S", "medium"="M", "large"="L"))
# mapvalues() lets you use two separate vectors instead of a named vector
mapvalues(sizes, c("small", "medium", "large"), c("S", "M", "L"))

sizes <- c("small", "large", "large", "small", "medium")
sizes
sizes[sizes=="small"] <- "S"
sizes[sizes=="medium"] <- "M"
sizes[sizes=="large"] <- "L"
sizes

#Recoding a Categorical Variable to Another Categorical Variable  
# Work on a subset of the PlantGrowth data set
pg <- PlantGrowth[c(1,2,11,21,22), ]
pg
#If the old value was "ctrl", the new value will be "No", and if the old value was "trt1" or "trt2", the new value will be "Yes".
pg <- PlantGrowth
oldvals <- c("ctrl", "trt1", "trt2")
newvals <- factor(c("No", "Yes", "Yes"))
pg$treatment <- newvals[ match(pg$group, oldvals) ]
#can also be done (more awkwardly) by indexing in the vectors:
pg$treatment[pg$group == "ctrl"] <- "no"
pg$treatment[pg$group == "trt1"] <- "yes"
pg$treatment[pg$group == "trt2"] <- "yes"
# Convert to a factor
pg$treatment <- factor(pg$treatment)
pg
# coding criteria can also be based on values in multiple columns, by using the & and | operators
pg$newcol[pg$group == "ctrl" & pg$weight < 5] <- "no_small"
pg$newcol[pg$group == "ctrl" & pg$weight >= 5] <- "no_large"
pg$newcol[pg$group == "trt1"] <- "yes"
pg$newcol[pg$group == "trt2"] <- "yes"
pg$newcol <- factor(pg$newcol)
pg
pg$weighttrt <- interaction(pg$weightcat, pg$treatment)
pg

#Recoding a Continuous Variable to a Categorical Variable  
# Work on a subset of the PlantGrowth data set
pg <- PlantGrowth[c(1,2,11,21,22), ]
pg
#recode the continuous variable weight into a categorical variable, wtclass, using the cut() function:
pg$wtclass <- cut(pg$weight, breaks = c(0, 5, 6, Inf))
pg

#change the names of the levels, set the labels:
pg$wtclass <- cut(pg$weight, breaks = c(0, 5, 6, Inf),
                  labels = c("small", "medium", "large"))
pg

#categories to be closed on the left and open on the right, set right = FALSE:
cut(pg$weight, breaks = c(0, 5, 6, Inf), right = FALSE)

#Transforming Variables
library(gcookbook) # For the data set
# Make a copy of the data
hw <- heightweight
hw
#convert heightIn to centimeters and store it in a new column, heightCm:
hw$heightCm <- hw$heightIn * 2.54
hw
#slightly easier-to-read code, you can use transform() or mutate() from the plyr package
hw <- transform(hw, heightCm = heightIn * 2.54, weightKg = weightLb / 2.204)
library(plyr)
hw <- mutate(hw, heightCm = heightIn * 2.54, weightKg = weightLb / 2.204)
hw
#calculate a new variable based on multiple variables:
# These all have the same effect:
hw <- transform(hw, bmi = weightKg / (heightCm / 100)^2)
hw <- mutate(hw, bmi = weightKg / (heightCm / 100)^2)
hw$bmi <- hw$weightKg / (hw$heightCm/100)^2
hw
#calculate all in one go
hw <- heightweight
hw <- mutate(hw,
             heightCm = heightIn * 2.54,
             weightKg = weightLb / 2.204,
             bmi = weightKg / (heightCm / 100)^2)

#Transforming Variables by Group
library(MASS) # For the data set
library(plyr)
cb <- ddply(cabbages, "Cult", transform, DevWt = HeadWt - mean(HeadWt))
cabbages
transform(cabbages, DevWt = HeadWt - mean(HeadWt))
library(plyr)
cb <- ddply(cabbages, "Cult", transform, DevWt = HeadWt - mean(HeadWt))
cb
# The data before normalizing
ggplot(cb, aes(x=Cult, y=HeadWt)) + geom_boxplot()
# After normalizing
ggplot(cb, aes(x=Cult, y=DevWt)) + geom_boxplot()

ddply(cabbages, c("Cult", "Date"), transform,
      DevWt = HeadWt - mean(HeadWt), DevVitC = VitC - mean(VitC))

#Summarizing Data by Groups
library(MASS) # For the data set
library(plyr)
ddply(cabbages, c("Cult", "Date"), summarise, Weight = mean(HeadWt),
      VitC = mean(VitC))
#data set
cabbages

summarise(cabbages, Weight = mean(HeadWt))

ddply(cabbages, "Cult", summarise, Weight = mean(HeadWt))

ddply(cabbages, c("Cult", "Date"), summarise, Weight = mean(HeadWt),
      VitC = mean(VitC))

ddply(cabbages, c("Cult", "Date"), summarise,
      Weight = mean(HeadWt),
      sd = sd(HeadWt),
      n = length(HeadWt))
#sprinkle a few NAs into HeadWt
c1 <- cabbages # Make a copy
c1$HeadWt[c(1,20,45)] <- NA # Set some values to NA
ddply(c1, c("Cult", "Date"), summarise,
      Weight = mean(HeadWt),
      sd = sd(HeadWt),
      n = length(HeadWt))

ddply(c1, c("Cult", "Date"), summarise,
      Weight = mean(HeadWt, na.rm=TRUE),
      sd = sd(HeadWt, na.rm=TRUE),
      n = sum(!is.na(HeadWt)))

# Copy cabbages and remove all rows with both c52 and d21
c2 <- subset(c1, !( Cult=="c52" & Date=="d21" ) )
c2a <- ddply(c2, c("Cult", "Date"), summarise,
             Weight = mean(HeadWt, na.rm=TRUE),
             sd = sd(HeadWt, na.rm=TRUE),
             n = sum(!is.na(HeadWt)))
c2a

#fill in the missing combination (Figure 15-3, right), give ddply() the .drop=FALSE flag:
# Make the graph
ggplot(c2a, aes(x=Date, fill=Cult, y=Weight)) + geom_bar(position="dodge", stat="identity")

c2b <- ddply(c2, c("Cult", "Date"), .drop=FALSE, summarise,
             Weight = mean(HeadWt, na.rm=TRUE),
             sd = sd(HeadWt, na.rm=TRUE),
             n = sum(!is.na(HeadWt)))
c2b
# Make the graph
ggplot(c2b, aes(x=Date, fill=Cult, y=Weight)) + geom_bar(position="dodge", stat="identity")

# Summarizing Data with Standard Errors and Confidence Intervals
library(MASS) # For the data set
library(plyr)
ca <- ddply(cabbages, c("Cult", "Date"), summarise,
            Weight = mean(HeadWt, na.rm=TRUE),
            sd = sd(HeadWt, na.rm=TRUE),
            n = sum(!is.na(HeadWt)),
            se = sd/sqrt(n))
ca

ddply(cabbages, c("Cult", "Date"), summarise,
      Weight = mean(HeadWt, na.rm=TRUE),
      sd = sd(HeadWt, na.rm=TRUE),
      n = sum(!is.na(HeadWt)),
      se = sd / sqrtn)
#use the qt() function to get the quantile,
ciMult <- qt(.975, ca$n-1)
ciMult
ca$ci <- ca$se * ciMult
#all in one line
ca$ci95 <- ca$se * qt(.975, ca$n)

summarySE <- function(data=NULL, measurevar, groupvars=NULL,
                      conf.interval=.95, na.rm=FALSE, .drop=TRUE) {
  require(plyr)
  # New version of length that can handle NAs: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
  # This does the summary
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col, na.rm) {
                   c( n = length2(xx[,col], na.rm=na.rm),
                      mean = mean (xx[,col], na.rm=na.rm),
                      sd = sd (xx[,col], na.rm=na.rm)
                   )
                 },
                 measurevar,
                 na.rm
  )
  # Rename the "mean" column
  datac <- rename(datac, c("mean" = measurevar))
  datac$se <- datac$sd / sqrt(datac$n) # Calculate standard error of the mean
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval:
  # e.g., if conf.interval is .95, use .975 (above/below), and use
  # df=n-1, or if n==0, use df=0
  ciMult <- qt(conf.interval/2 + .5, datac$n-1)
  datac$ci <- datac$se * ciMult
  return(datac)
}


# Remove all rows with both c52 and d21
c2 <- subset(cabbages, !( Cult=="c52" & Date=="d21" ) )
# Set some values to NA
c2$HeadWt[c(1,20,45)] <- NA
summarySE(c2, "HeadWt", c("Cult", "Date"), conf.interval=.99,
          na.rm=TRUE, .drop=FALSE)

#Converting Data from Wide to Long
library(gcookbook) # For the data set
anthoming

library(reshape2)
melt(anthoming, id.vars="Angle", variable.name="condition", value.name="count")
#data set
drunk

melt(drunk, id.vars="sex", measure.vars=c("0-29", "30-39"),
     variable.name="age", value.name="count")

#use more than one column as the ID variables:
plum_wide 

melt(plum_wide, id.vars=c("length","time"), variable.name="survival",
     value.name="count")

#add an ID variable before using melt():
# Make a copy of the data
co <- corneas
co
# Add an ID column
co$id <- 1:nrow(co)
melt(co, id.vars="id", variable.name="eye", value.name="thickness")

#Converting Data from Long to Wide
library(gcookbook) # For the data set
plum

library(reshape2)
dcast(plum, length + time ~ survival, value.var="count")
#When there is more than one variable variable, the values are combined with an underscore
dcast(plum, time ~ length + survival, value.var="count")

#Converting a Time Series Object to Times and Values  
# Look at nhtemp Time Series object
nhtemp
# Get times for each observation
as.numeric(time(nhtemp))
# Get value of each observation
as.numeric(nhtemp)
# Put them in a data frame
nht <- data.frame(year=as.numeric(time(nhtemp)), temp=as.numeric(nhtemp))
nht

#data set
presidents
#convert it to a two-column data frame with one column representing the year with fractional values,
pres_rating <- data.frame(
  year = as.numeric(time(presidents)),
  rating = as.numeric(presidents)
)
pres_rating
#store the year and quarter in separate columns
pres_rating2 <- data.frame(
  year = as.numeric(floor(time(presidents))),
  quarter = as.numeric(cycle(presidents)),
  rating = as.numeric(presidents)
)
pres_rating2
################## The R book ####

# the R development Core
#citation()

# Getting Help in R
?read.table
help.search("data input")
find(lowess)
apropos(lm)

# example of Functions
example(lm)

# Demo of R functions
demo(persp)
demo(graphics)
demo(Hershey)
demo(plotmath)

# Libraries in R
library(spatial)

#Contents of Libraries
library(help=spatial)

# installing packages and libraries
install.packages("akima")
install.packages("chron")
install.packages("Ime4")
install.packages("mcmc")
install.packages("odesolve")
install.packages("spdep")
install.packages("spatstat")
install.packages("tree")

 # edit the bacteria dataframe (part of the MASS library)
library(MASS)
attach(bacteria)
fix(bacteria)

 # print smallest value from x,y,z
pmin(x,y,z)

 #get the column name
names(bacteria)
 #get first 5 rows of dataframe
head(bacteria)
 #get last 5 rows of dataframe
tail(bacteria)

 # random sequence 
sample(table)
sample(table$cloumn)

 # get the matrices
x <- matrix(c(1,2,3,4,5,6,7,8,9), nrow = 3)
dim(x)
class(x)
attributes(x)

 # convert a vector into a matrix
vector <- c(1,2,3,4,5,6,6,5,4,3,2,1)
vmatrix <- matrix(vector,byrow=T,nrow=3) #vertical put values
vmatrix <- matrix(vector,byrow=F,nrow=3) #horizantal put values

 #another way to convert a vector into a matrix
dim(vector) <- c(3,4)
 
 # check if it's a matrix
is.matrix(vector)

 # convert columns and rows
vector <- t(vector)

 # get an array of number
A <- c(1:25)
 
 # get an array of letter
L <- letters[1:20] # anything abovve 26 will be shown as NA

 # Character Strings
a<-"abc"
b<-"123"
pets<-c("cat","dog","gerbil","terrapin")

 # change the data type as numberic
as.numeric(a)
as.numeric(b)

 # length of the vector (count value in vector)
length(pets)

 # count individual character strings
nchar(pets)

 # check the character strings
class(pets)

 # check if the strings is factor
is.factor(pets)

 # when the vector become part of dataframe, it will become factors
df <- data.frame(pets)
class(df)
class(df$pets)
is.factor(df$pets)

 # concat values
paste(a,b,sep="")
paste(a,b)

 # concat with string
paste(a,b,"a longer phrase containing blanks",sep="")

 # paste something with all rows
d <- c(a,b,"new")
e <- paste(d,"this will shows up in every line")



 #extracting part of string
x <- "testing this phrase"
 
 #check how long is x
nchar(x)
q <- character(19)
for (i in 1:29) q[i] <- substr(x,1,i)
q

 #split up a character string into individual characters
strsplit(x,split=character(0))


 # counting the number of occurrences of characters
table(strsplit(x,split=character(0)))

 #counting the number of words in a phrase
words<-1+table(strsplit(x,split=character(0)))[1]
words

 # get the string out of the phrase
strsplit(x,"this")

 # return the 1st value from the element list which is split by 'this'
strsplit(x,"this")[[1]] [2]

 # switch to upper case
toupper("whatEVer")

 # switch to lower case
tolower("WHATevER")

 # match function
a <- c(8,5,2,1,4,5,123,23,213,4,51,85)
b <- c(8,5,456,35,225,523,358,621,4,556,1)
match(a,b)

 # give a easier way to show if it is match or not
m <- c("T","F")
m[1+is.na(match(a,b))]

####### functions
y <- c(3,3,4,5,6,8,7,9)
 # Arithmetic mean of a single sample (avg.)
mean(y)

 # Median of a single sample
sort(y)[ceiling(length(y)/2)]

 # general function to calculate medians
med<-function(x) {
  odd.even <- length(x)%%2
  if (odd.even == 0) (sort(x)[length(x)/2] + sort(x)[1 + length(x)/2])/2
  else sort(x)[ceiling(length(x)/2)]
}

 # take the 1st number out
y[-1]

 # use the function on vector
med(y)
med(y[-1])

 #Geometric mean
100000^0.2

insects<-c(1,10,1000,10,1)
mean(insects)
 # same as above
exp(mean(log(insects)))

 #a function to calculate geometric mean of a vector of numbers x
geometric<-function (x) exp(mean(log(x)))
 #testing it with the insect dat
geometric(insects)

 # Harmonic mean
harmonic<-function (x) 1/mean(1/x)
harmonic(c(1,2,4,1))

 # Variance

 # Degrees of freedom
y<-c(13,7,5,12,9,15,6,11,9,7,12)
variance<-function(x) sum((x - mean(x))^2)/(length(x)-1)
variance(y)
 # same as 
var(y)

 #Variance Ratio Test
variance.ratio<-function(x,y) {
                                v1<-var(x)
                                v2<-var(y)
                                    if (var(x) > var(y)) {
                                                          vr<-var(x)/var(y)
                                                          df1<-length(x)-1
                                                          df2<-length(y)-1
                                                        }
                                    else { 
                                           vr<-var(y)/var(x)
                                           df1<-length(y)-1
                                           df2<-length(x)-1
                                        }
                              2*(1-pf(vr,df1,df2)) }

 # an example 
a<-rnorm(10,15,2)
b<-rnorm(10,15,4)
 # test the function
variance.ratio(a,b)
 # same as above
var.test(a,b)

 # Using Variance
se<-function(x) sqrt(var(x)/length(x))

ci95<-function(x) {
  t.value<- qt(0.975,length(x)-1)
  standard.error<-se(x)
  ci<-t.value*standard.error
  cat("95% Confidence Interval = ", mean(x) -ci, "to ", mean(x) +ci,"\n") }

x<-rnorm(150,25,3)
ci95(x)

xv<-rnorm(30)

sem<-numeric(30)
sem[1]<-NA
for(i in 2:30) sem[i]<-se(xv[1:i])
plot(1:30,sem,ylim=c(0,0.8),
     ylab="standard error of mean",xlab="sample size n",pch=16)
 # add line on the graphic
lines(2:30,1/sqrt(2:30))
 
 # Error Bars
error.bars<-function(yv,z,nn){
  xv<-
    barplot(yv,ylim=c(0,(max(yv)+max(z))),names=nn,ylab=deparse(substitute(yv)
    ))
  g=(max(xv)-min(xv))/50
  for (i in 1:length(xv)) {
    lines(c(xv[i],xv[i]),c(yv[i]+z[i],yv[i]-z[i]))
    lines(c(xv[i]-g,xv[i]+g),c(yv[i]+z[i], yv[i]+z[i]))
    lines(c(xv[i]-g,xv[i]+g),c(yv[i]-z[i], yv[i]-z[i]))
  }}

# se<-rep(28.75,5)
# labels<-as.character(levels(clipping))
# ybar<-as.vector(tapply(biomass,clipping,mean))
# error.bars(ybar,se,labels)

xy.error.bars<-function (x,y,xbar,ybar){
  plot(x, y, pch=16, ylim=c(min(y-ybar),max(y+ybar)),
       xlim=c(min(x-xbar),max(x+xbar)))
  arrows(x, y-ybar, x, y+ybar, code=3, angle=90, length=0.1)
  arrows(x-xbar, y, x+xbar, y, code=3, angle=90, length=0.1) }

x <- rnorm(10,25,5)
y <- rnorm(10,100,20)
xb <- runif(10)*5
yb <- runif(10)*20
xy.error.bars(x,y,xb,yb)


 # Loops and Repeats
for (i in 1:5) print(i^2)

j <- k <- 0
for (i in 1:5) {
  j<-j+1
  k<-k+i*j
  print(i+j+k) }

 # write a function of x!= x*(x-1)*(x-2)*(x-3)...*2*1
fac1<-function(x) {
  f <- 1
  if (x<2) return (1)
  for (i in 2:x) {
    f <- f*i
    f }}
 # try this function from 0 to 5
sapply(0:5,fac1)

 #while function
fac2<-function(x) {
  f <- 1
  t <- x
  while(t>1) {
    f <- f*t
    t <- t-1 }
  return(f) }
 # try while function
sapply(0:5,fac2)

 # repeat function
fac3<-function(x) {
  f <- 1
  t <- x
  repeat {
    if (t<2) break
    f <- f*t
    t <- t-1 }
  return(f) }
 # try repeat function
sapply(0:5,fac3)
 # build in loops or repeats function
cumprod(1:5)
 # fix the 0 issue in Cumprod function
fac4<-function(x) max(cumprod(1:x))
max(cumprod(1:0))
sapply(0:5,fac4)
 # build in function for x!
fac5<-function(x) gamma(x+1)
sapply(0:5,fac5)
 # build in factorial function
sapply(0:5,factorial)

 # uses the while function in converting a specified number to its binary representation
binary<-function(x) {
  i<-0
  string<-numeric(32)
  while(x>0) {
    string[32-i]<-x %% 2
    x<-x%/% 2
    i<-i+1 }
  first<-match(1,string)
  string[first:32] }
 # try this function
sapply(15:17,binary)

 # Uses while to generate the Fibonacci series
fibonacci<-function(n) {
  a<-1
  b<-0
  while(n>0)
  {swap<-a
  a<-a+b
  b<-swap
  n<-n-1 }
  b }
 # try this function
sapply(1:10,fibonacci)

 # Loop avoidance
y<-c(8,3,5,7,6,6,8,9,2,3,9,4,10,4,11)
y [y<0] <- 0
for (i in 1:length(y)) { if(y[i] < 0) y[i] <- 0 }

 # ifelse function
z <- ifelse(y < 0, -1, 1)

 # slowness of loops
x<-runif(10000000)
system.time(max(x))
pc<-proc.time()
cmax<-x[1]
for (i in 2:10000000) {
  if(x[i]>cmax) cmax<-x[i] }
proc.time()-pc

 #Do not ‘grow’ data sets in loops or recursive function calls

 #The switch Function
central<-function(y, measure) {
  switch(measure,
         Mean = mean(y),
         Geometric = exp(mean(log(y))),
         Harmonic = 1/mean(1/y),
         Median = median(y),
         stop("Measure not included")) }

central(rnorm(100,10,2),"Harmonic")
central(rnorm(100,10,2),4)

 #Optional Arguments
charplot<-function(x,y,pc=16,co="red"){
  plot(y~x,pch=pc,col=co)}
 # get the graphic with red circles
charplot(1:10,1:10)
 # get the graphic with red triangles
charplot(1:10,1:10,17)
 # changing color
charplot(1:10,1:10,co="navy")
 # changing color and symbol
charplot(1:10,1:10,15,"green")
 # setting must be in the right order
charplot(1:10,1:10,"green",15)
 # unless specify variable names
charplot(1:10,1:10,co="green",pc=15)

 #Variable Numbers of Arguments
many.means <- function (...) {
  data <- list(...)
  n<- length(data)
  means <- numeric(n)
  vars <- numeric(n)
  for (i in 1:n) {
    means[i]<-mean(data[[i]])
    vars[i]<-var(data[[i]])
  }
  print(means)
  print(vars)
  invisible(NULL)
}

x<-rnorm(100)
y<-rnorm(200)
z<-rnorm(300)

 #invoke the function:
many.means(x,y,z)
 
 #Returning Values from a Function
parmax<-function (a,b) {
  c<-pmax(a,b)
  median(c) }

x<-c(1,9,2,8,3,7)
y<-c(9,2,8,3,7,2)
parmax(x,y)

parboth<-function (a,b) {
  c<-pmax(a,b)
  d<-pmin(a,b)
  answer<-list(median(c),median(d))
  names(answer)[[1]]<-"median of the parallel maxima"
  names(answer)[[2]]<-"median of the parallel minima"
  return(answer) }

parboth(x,y)

 #Anonymous Functions
(function(x,y){ z <- 2*x^2 + y^2; x+y+z })(0:7, 1)

 #Flexible Handling of Arguments to Functions
plotx2 <- function (x, y=z^2) {
  z<-1:x
  plot(z,y,type="l") }

par(mfrow=c(1,2))
plotx2(12)
plotx2(12,1:12)

 #Evaluating Functions with apply, sapply and lapply
(X<-matrix(1:24,nrow=4))
X<-matrix(1:24,nrow=4)
X
 #used for applying functions to the rows or columns of matrices or dataframes
apply(X,1,sum)
apply(X,2,sum)
apply(X,1,sqrt)
apply(X,2,sqrt)
apply(X,1,sample)
apply(X,1,function(x) x^2+x)

 #apply a function to a vector
sapply(3:7, seq)

sapdecay<-read.table("c:\\temp\\sapdecay.txt",header=T)
attach(sapdecay)
names(sapdecay)

sumsq <- function(a,xv=x,yv=y)
{ yf <- exp(-a*xv)
sum((yv-yf)^2) }

lm(log(y)~x)
a<-seq(0.01,0.2,.005)
plot(a,sapply(a,sumsq),type="l")

a[min(sapply(a,sumsq))==sapply(a,sumsq)]
plot(x,y)
xv<-seq(0,50,0.1)
lines(xv,exp(-0.055*xv))
fa<-function(a) sum((y-exp(-a*x))^2)

optimize(fa,c(0.01,0.1))
fb<-function(a) sum(abs(y-exp(-a*x)))
optimize(fb,c(0.01,0.1))

 #Lists and lapply
a<-c("a","b","c","d")
b<-c(1,2,3,4,4,3,2,1)
c<-c(T,T,F)

list.object<-list(a,b,c)
class(list.object)

lapply(list.object,length)
lapply(list.object,class)

lapply(list.object,mean)

 #Looking for runs of numbers within vectors
(poisson<-rpois(150,0.7))
rle(poisson)
max(rle(poisson)[[1]])

which(rle(poisson)[[1]]==6)
rle(poisson)[[2]][24]

run.and.value<-function (x) {
  a<- max(rle(poisson)[[1]])
  b<-rle(poisson)[[2]][which(rle(poisson)[[1]] == a)]
  cat("length = ",a," value = ",b, "\n")}

run.and.value(poisson)
length(rle(poisson)[[2]])

n1<-25
n2<-30
y<-c(rep(1,n1),rep(0,n2))
len<-numeric(10000)
for (i in 1:10000) len[i]<-length(rle(sample(y))[[2]])
quantile(len,c(0.025,0.975))

 #Saving Data Produced within R to Disc
nbnumbers<-rnbinom(1000, size=1, mu=1.2)
write(nbnumbers,"c:\\temp\\nbnumbers.txt",1)
xmat<-matrix(rpois(100000,0.75),nrow=1000)
write.table(xmat,"c:\\temp\\table.txt",col.names=F,row.names=F)

nbtable<-table(nbnumbers)
nbtable

 #Pasting into an Excel Spreadsheet
writeClipboard(as.character(factor.name))
writeClipboard(as.character(numeric.variable))

 #Writing an Excel Readable File from R
write.table(data,"clipboard",sep="\t",col.names=NA)

 #Testing for Equality
x <- sqrt(2)
x * x == 2
x * x - 2

 #Sets: union, intersect and setdiff
setA<-c("a", "b", "c", "d", "e")
setB<-c("d", "e", "f", "g")

union(setA,setB)
intersect(setA,setB)
setdiff(setA,setB)
setdiff(setB,setA)
all(c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA))==
      union(setA,setB))
setequal(c(setdiff(setA,setB),intersect(setA,setB),setdiff(setB,setA)),
         union(setA,setB))

setA %in% setB
setB %in% setA

setA[setA %in% setB]
intersect(setA,setB)

 #Pattern Matching
wf<-read.table("c:\\temp\\worldfloras.txt",header=T)
attach(wf)
names(wf)

as.vector(Country[grep("R",as.character(Country))])
 #restrict the search to countries whose first name begins with R use the ∧
as.vector(Country[grep("^R",as.character(Country))])

 #select those countries with multiple names with upper case R as the first letter of their second or subsequent names, we specify the character string as ‘blank R'
as.vector(Country[grep(" R",as.character(Country))])
                  
 #find all the countries with two or more names, just search for a blank " "                 
as.vector(Country[grep(" ",as.character(Country))])

 #find countries with names ending in ‘y’ use the $ (dollar) symbol
as.vector(Country[grep("y$",as.character(Country))])

 #select countries with names containing upper-case letters from C to E inclusive
as.vector(Country[grep("[C-E]",as.character(Country))])

 #restrict the choice to first letters use the ^ operator before the list of capital letters
as.vector(Country[grep("^[C-E]",as.character(Country))])

 #select countries that do not end with a letter between ‘a’ and ‘t’
as.vector(Country[-grep("[a-t]$",as.character(Country))])

 #put ranges for both upper- and lower-case letters inside the square brackets, separated by a space
as.vector(Country[-grep("[A-T a-t]$",as.character(Country))])

 #Dot . as the ‘anything’ character
as.vector(Country[grep("^.y",as.character(Country))])

 #search for countries with ‘y’ as third letter
as.vector(Country[grep("^..y",as.character(Country))])

 #countries with ‘y’ as their sixth letter
as.vector(Country[grep("^. {5}y",as.character(Country))])

 #countries with 4 or fewer letters in their names
as.vector(Country[grep("^. {,4}$",as.character(Country))])

 #find all the countries with 15 or more characters in their name
as.vector(Country[grep("^. {15, }$",as.character(Country))])

 #Substituting text within character strings
text <- c("arm","leg","head", "foot","hand", "hindleg", "elbow")

 #replace all lower-case ‘h’ with upper-case ‘H'
gsub("h","H",text)

 #convert the first occurrence of a lower-case ‘o’ into an upper-case ‘O’
sub("o","O",text)

 #convert all occurrence of a lower-case ‘o’ into an upper-case ‘O’
gsub("o","O",text)

 #replace the first character of every string with upper-case ‘O’
gsub("^.","O",text)

 #capitalize the first character in each string
gsub("(\\w)(\\w*)", "\\U\\1\\L\\2",text, perl=TRUE)

 #convert all the characters to upper case
gsub("(\\w*)", "\\U\\1",text, perl=TRUE)

 #Locations of the pattern within a vector of character strings using regexpr
text <- c("arm","leg","head", "foot","hand", "hindleg", "elbow")

regexpr("o",text)

grep("o",text)

text[grep("o",text)]

freq<-as.vector(unlist (lapply(gregexpr("o",text),length)))
present<-ifelse(regexpr("o",text)<0,0,1)
freq*present

 #match value
charmatch("m", c("mean", "median", "mode"))

charmatch("med", c("mean", "median", "mode"))

 # Using %in% and which
stock<-c('car','van')
requests<-c('truck','suv','van','sports','car','waggon','car')

 # find the locations in the first-named vector of any and all of the entries in the second-named vector
which(requests %in% stock)

 # what the matches are as well as where they are,
requests [which(requests %in% stock)]

 #use the match function to obtain the same result (p. 47):
stock[match(requests,stock)][!is.na(match(requests,stock))]

 #more complicated way of doing it involves sapply
which(sapply(requests, "%in%", stock))

 #More on pattern matching on p.84 (91)
text <- c("arm","leg","head", "foot","hand", "hindleg", "elbow")
grep("o{1}",text,value=T)
grep("o{2}",text,value=T)
grep("o{3}",text,value=T)
grep("[[:alnum:]]{4, }",text,value=T)
grep("[[:alnum:]]{5, }",text,value=T)
grep("[[:alnum:]]{6, }",text,value=T)
grep("[[:alnum:]]{7, }",text,value=T)

 #Perl regular expressions
 #Perl is good for altering the cases of letters. Here, we capitalize the first character in each string
gsub("(\\w)(\\w*)", "\\U\\1\\L\\2",text, perl=TRUE)

 #convert all the character to upper case:
gsub("(\\w*)", "\\U\\1",text, perl=TRUE)

 #Stripping patterned text out of complex strings
 #Suppose that we want to tease apart the information in these complicated strings:
(entries <-c ("Trial 1 58 cervicornis (52 match)", "Trial 2 60 terrestris (51 matched)",
              "Trial 8 109 flavicollis (101 matches)"))
 #The first task is to remove the material on numbers of matches including the brackets:
gsub(" *$", "", gsub("\\(.*\\)$", "", entries))

# The first argument " *$", "", removes the "trailing blanks" while the second deletes everything
# .∗ between the left \\(and right \\) hand brackets "\\(.*\\)$" substituting this with nothing
# "". The next job is to strip out the material in brackets and to extract that material, ignoring
# the brackets themselves:
pos<- regexpr("\\(.*\\)$", entries)
substring(entries, first=pos+1, last=pos+attr(pos,"match.length")-2)

pos

 #Testing and Coercing in R
as.numeric(factor(c("a","b","c")))
as.numeric(c("a","b","c"))
as.numeric(c("a","4","c"))

geometric<-function(x){
  if(!is.numeric(x)) stop ("Input must be numeric")
  exp(mean(log(x))) }
geometric(c("a","b","c"))

########## Dates and Times in R
Sys.time()
substr(as.character(Sys.time()),1,10)
substr(as.character(Sys.time()),12,19)
unclass(Sys.time())
date()
date<- as.POSIXlt(Sys.time())
date$wday
date$yday
unlist(unclass(date))

 #Calculations with dates and times 
 # different of days
y2<-as.POSIXlt("2003-10-22")
y1<-as.POSIXlt("2005-10-22")
y1-y2

 #different of hours
y3<-as.POSIXlt("2005-10-22 09:30:59")
y4<-as.POSIXlt("2005-10-22 12:45:06")

 #difftime function
 #diff day
difftime("2005-10-21","2003-8-15")
y4-y3

 # calculate different day and same the number as numeric
as.numeric(difftime("2005-10-21","2003-8-15"))
 #diff hour
difftime("2005-10-21 5:12:32","2005-10-21 6:14:21")

ISOdate(2005,10,21)-ISOdate(2003,8,15)
as.difftime(c("0:3:20", "11:23:15"))
as.difftime(c("3:20", "23:15", "2:"), format= "%H:%M")

 #The strptime function  
# %a Abbreviated weekday name
# %A Full weekday name
# %b Abbreviated month name
# %B Full month name
# %c Date and time, locale-specific
# %d Day of the month as decimal number (01–31)
# %H Hours as decimal number (00–23) on the 24-hour clock
# %I Hours as decimal number (01–12) on the 12-hour clock
# %j Day of year as decimal number (001–366)
# %m Month as decimal number (01–12)
# %M Minute as decimal number (00–59)
# %p AM/PM indicator in the locale
# %S Second as decimal number (00–61, allowing for two ‘leap seconds’)
# %U Week of the year (00–53) using the first Sunday as day 1 of week 1
# %w Weekday as decimal number (0–6, Sunday is 0)
# %W Week of the year (00–53) using the first Monday as day 1 of week 1
# %x Date, locale-specific
# %X Time, locale-specific
# %Y Year with century
# %Z Time zone as a character string (output only)

 #Dates in Excel spreadsheets
excel.dates <- c("27/02/2004", "27/02/2005", "14/01/2003",
                 "28/06/2005", "01/01/1999")
strptime(excel.dates,format="%d/%m/%Y")
other.dates<- c("1jan99", "2jan05", "31mar04", "30jul05")
strptime(other.dates, "%d%b%y")

 #Calculating time differences between the rows of a dataframe
class(duration)
duration[1:15]-duration[2:16]
length(duration[1:15]-duration[2:16])
length(duration)
diffs<-c(duration[1:15]-duration[2:16],NA)
diffs
times$diffs<-diffs
times


########## Data input
y <- c (6,7,3,4,8,5,6,2)
 #The scan Function
x<-scan()
 
 #Data Input from Files
 
 #Saving the File from Excel
 
 # different way to load data
map<-read.table("c:\\temp\\bowens.csv",header=T,sep=",")
 #but it is quicker and easier to use read.csv in this case(and there is no need for header=T)
map<-read.csv("c:\\temp\\bowens.csv")
 #If you are tired of writing header=T in all your read.table functions, then switch to
read.delim("c:\\temp\\file.txt")
 #or write your own function
rt<-function(x) read.delim(x)
 #then use the function rt to read data-table files like this:
rt("c:\\temp\\regression.txt")
 #Better yet, remove the need to enter the drive and directory or the file suffix:
rt<-function(x) read.delim(paste("c:\\temp\\",x,".txt",sep=" "))
rt("regression")

 #Browsing to Find Files
data<-read.table(file.choose(),header=T)

 #Separators and Decimal Points
murder<-read.table("c:\\temp\\murders.txt",header=T,as.is="region"); attach(murder)

table(region)
table(murder$region)

is.factor(region)

 #Input and Output Formats
# Formatting is controlled using escape sequences, typically within double quotes:
# \n newline
# \r carriage return
# \t tab character
# \b backspace
# \a bell
# \f form feed
# \v vertical tab

 #Setting the Working Directory
setwd("c:\\temp")
read.table("daphnia.txt",header=T)
getwd()

 #Checking Files from the Command Line
file.exists("c:\\temp\\Decay.txt")

 #Reading Dates and Times from Files

 #Built-in Data Files
data()
 #You can read the documentation for a particular data set with the usual query:
?lynx

try(data(package="spatstat"));Sys.sleep(3)
try(data(package="spdep"));Sys.sleep(3)
try(data(package="MASS"))

attach(OrchardSprays)
decrease

 #Reading Data from Files with Non-standard Formats Using scan
murders<-scan("c:\\temp\\murders.txt", skip=1, what=list("","","",""))

class(murders)

 #convert the list to a dataframe using the as.data.frame function
murder.frame<-as.data.frame(murders)

 #use the variables names from the file as variable names in the dataframe
murder.names<-scan("c:\\temp\\murders.txt",nlines=1,what="character",quiet=T)
murder.names

names(murder.frame)<-murder.names
murder.frame[,2]<-as.numeric(murder.frame[,2])
murder.frame[,3]<-as.numeric(murder.frame[,3])
summary(murder.frame)

murders<-read.table("c:\\temp\\murders.txt",header=T)
summary(murders)
#scan function is quicker than read.table for input of large (numeric only) matrices

 #Reading Files with Different Numbers of Values per Line
line.number<-length(scan("c:\\temp\\rt.txt",sep="\n"))
(my.list<-sapply(0:(line.number-1),
                 function(x) scan("c:\\temp\\rt.txt",skip=x,nlines=1,quiet=T)))

unlist(lapply(my.list,length))
unlist(lapply(1:length(my.list), function(i) my.list[[i]][length(my.list[[i]])]))

 #The readLines Function
readLines("c:\\temp\\murders.txt",n=-1) #n=-1 means read to the end of the file

mo<-readLines("c:\\temp\\murders.txt",n=-1)
strsplit(mo[51],"\t")
 #You would probably want 376 and 6.9 as numeric rather than character objects:
as.numeric(unlist(strsplit(mo[51],"\t")))
 #where the two names Wyoming and West have been coerced to NA, or
as.vector(na.omit(as.numeric(unlist(strsplit(mo[51],"\t")))))

 #get the numbers on their own.
mv<-sapply(2:51,function(i)
  as.vector(na.omit(as.numeric(unlist(strsplit(mo[i],"\t"))))))
pop<-mv[1,]
mur<-mv[2,]

 #get character vectors of the state names and regions
ms<-sapply(2:51,function(i) strsplit(mo[i],"\t"))
texts<-unlist(lapply(1:50,function(i) ms[[i]][c(1,4)]))
sta<-texts[seq(1,99,2)]
reg<- texts[seq(2,100,2)]

 #convert all the information from readLines into a data.frame
data.frame(sta,pop,mur,reg)

 #achieved all above in a single line with read.table
rlines<-readLines("c:\\temp\\rt.txt")
split.lines<-strsplit(rlines,"\t")
new<-sapply(1:5,function(i) as.vector(na.omit(as.numeric(split.lines[[i]]))))
new

############## Dataframes 

 #Selecting Rows from the Dataframe at Random
worms[sample(1:20,8),]

 #Sorting Dataframes
worms[order(Slope),]
worms[rev(order(Slope)),]
worms[order(Vegetation,Worm.density),]
worms[order(Vegetation,Worm.density,Soil.pH),]
worms[order(Vegetation,Worm.density),c(4,7,5,3)]
worms[order(Vegetation,Worm.density), c("Vegetation", "Worm.density", "Soil.pH", "Slope")]

 #Using Logical Conditions to Select Rows from the Dataframe
worms[Damp == T,]
worms[Worm.density > median(Worm.density) & Soil.pH < 5.2,]
 #extract the columns that were numeric
worms[,sapply(worms,is.numeric)]
 #extract the columns that were factors
worms[,sapply(worms,is.factor)]
 #with out row 6 to 15
worms[-(6:15),]
 #without vegetation is Grassland
worms[!(Vegetation=="Grassland"),]
 #drop Damp is False
worms[-which(Damp==F),]
 #same end as the more elegant
worms[!Damp==F,]
 #or even simpler,
worms[Damp==T,]

#Omitting Rows Containing Missing Values, NA
data<-read.table("c:\\temp\\worms.missing.txt",header=T)
data

na.omit(data)
new.frame<-na.exclude(data)
 #test for the presence of missing values across a dataframe
complete.cases(data)
 #sum up the missing values in each variable
apply(apply(data,2,is.na),2,sum)

 #Using order and unique to Eliminate Pseudoreplication
worms[rev(order(Worm.density)),][unique(Vegetation),]

 #Complex Ordering with Mixed Directions
worms[order(Vegetation,-Worm.density),]
worms[order(-rank(Vegetation),-Worm.density),]

 #give the column name 
names(worms)
 
 #grep the variables containing upper-case S
grep("S",names(worms))

 #select column where column name containing upper-case S
worms[,grep("S",names(worms))]

 #A Dataframe with Row Names instead of Row Numbers
detach(worms)
worms<-read.table("c:\\temp\\worms.txt",header=T,row.names=1)
worms

 #Creating a Dataframe from Another Kind of Object
x<-runif(10)
y<-letters[1:10]
z<-sample(c(rep(T,5),rep(F,5)))

 #make them into a dataframe called new
new<-data.frame(y,z,x)
 #convert the table into a dataframe
y<-rpois(1500,1.5)
table(y)
as.data.frame(table(y))
 #lapply with rep for this
short.frame<-as.data.frame(table(y))
long<-as.data.frame(lapply(short.frame, function(x) rep(x, short.frame$Freq)))
long[,1]

 #Eliminating Duplicate Rows from a Dataframe
dups<-read.table("c:\\temp\\dups.txt",header=T)
dups
 #check table without dups
unique(dups)
 #check dups
dups[duplicated(dups),]

 #Dates in Dataframes
nums<-read.table("c:\\temp\\sortdata.txt",header=T)
attach(nums)
names(nums)
 #order by data
nums[order(date),]

dates<-strptime(date,format="%d/%m/%Y")
dates

nums<-cbind(nums,dates)
nums[order(as.character(dates)),1:4]

subset(nums,select=c("name","dates"))

 #Selecting Variables on the Basis of their Attributes
sapply(nums,is.numeric)
nums[,sapply(nums,is.numeric)]

 #Using the match Function in Dataframes
unique(worms$Vegetation)
herbicides<-read.table("c:\\temp\\herbicides.txt",header=T)
herbicides

herbicides$Herbicide[match(worms$Vegetation,herbicides$Type)]
 #add this information as a new column in the worms dataframe:
worms$hb<-herbicides$Herbicide[match(worms$Vegetation,herbicides$Type)]
 #or create a new dataframe called recs containing the herbicide recommendations:
recs<-data.frame(
    worms,hb=herbicides$Herbicide[match(worms$Vegetation,herbicides$Type)])
recs

 #Merging Two Dataframes
(lifeforms<-read.table("c:\\temp\\lifeforms.txt",header=T))
(flowering<-read.table("c:\\temp\\fltimes.txt",header=T))
merge(flowering,lifeforms)

(both<-merge(flowering,lifeforms,all=T))
(seeds<-read.table("c:\\temp\\seedwts.txt",header=T))
merge(both,seeds,by.x=c("Genus","species"),by.y=c("name1","name2"))

 #Adding Margins to a Dataframe
frame<-read.table("c:\\temp\\sales.txt",header=T)
frame

people<-rowMeans(frame[,2:5])
people<-people-mean(people)
people

(new.frame<-cbind(frame,people))

seasons<-colMeans(frame[,2:5])
seasons<-seasons-mean(seasons)
seasons

new.row<-new.frame[1,]

new.row[1]<-"seasonal effects"
new.row[2:5]<-seasons
new.row[6]<-0

(new.frame<-rbind(new.frame,new.row))

gm<-mean(unlist(new.frame[1:5,2:5]))
gm<-rep(gm,4)
new.frame[1:5,2:5]<-sweep(new.frame[1:5,2:5],2,gm)
new.frame

 #Summarizing the Contents of Dataframes
aggregate(worms[,c(2,3,5,7)],by=list(veg=Vegetation),mean)
aggregate(worms[,c(2,3,5,7)],by=list(veg=Vegetation,d=Damp),mean)


############## Graphics (check R graphics book)
############## Tables P.190





new


