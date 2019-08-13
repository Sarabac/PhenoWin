# PhenoWin
Shiny application to draw phenological windows.

## Setup
### Input Geotif Files
The folder **_DOY** must contain the input geotif files. Its name is format
*DOY_[3 digit crop code]-[1 or 2 digits phonology code]_[4 digit year].tif*.

For example:

```
DOY_201-1_1993.tif
DOY_201-1_1997.tif
```
### Data Generation
Launch **Extract_Pheno_Shapefile.R** to generate the data used by the app in
the **_Data** folder.
## Usage
Launch **Graph_Pheno.R** to start the app.

![dashboard](_Images/Dashboard.png)
*screenshot of the app*
### Data selection

The *Mode* radiobutton enable two modes:
+ Select mode:  
  The user click on the entities of the map to select them. The selected red entities
  will be used to create the map.
+ Zone mode:  
  The user click on an entity on the map to delete it.

### Phenologyical windows graph
The graph is drawn according to the data selected by the user. Its limits are
determined by the time slider.  
Each phenological window is drawn on top of the previous one. The alpha value
correspond to the proportion of pixel that reached a phenological phase.
This situation can be visualized in the bottom time line of the following graph.
![dashboard](_Images/graph_example.png)

# R packages
+ tidyverse
+ scales
+ lubridate
+ shiny
+ leaflet
+ leaflet.extras
+ raster
+ sf
