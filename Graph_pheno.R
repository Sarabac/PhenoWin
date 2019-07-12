#https://moderndata.plot.ly/plotly-4-7-0-now-on-cran/
#https://plotly-r.com/
W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(W.DIR)

source("Functions_Pheno.R")

library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(lubridate)



build_DOY_graph = function(dat){
  # create the Phenological graph from the data frame "dat"
  # with 3 columns:
  # "Date": class Date
  # "sum_weight": between 0 and 1
  # "P": phenological stage
  graph =  ggplot(dat, aes(x = Date, y=1, alpha = sum_weight, fill = as.factor(P)))+
    geom_tile() +
    geom_vline(aes(xintercept = as.Date(paste(year(Date), "01", "01", sep = "-")),
               linetype = "Year"), size = 2)+
    geom_vline(aes(xintercept = as.Date(paste(year(Date), month(Date), "01", sep = "-")),
               linetype = "Month"))  +
    labs(fill = "Phenology", alpha = "Probability") +
    color_fill_custom +
    scale_linetype_manual("Breaks", values = c("Month" = "dotted", "Year" = "dashed")) +
    theme(axis.text.x=element_text(angle=30, hjust=1),
          axis.text.x.top = element_text(angle = 30, vjust=0, hjust=0))
  return(graph)
}

period_labelling = function(from, to){
  # define the date break of the graph
  # depending of the lenght of the time period
  dif = ymd(to) - ymd(from)
  label_period = case_when(
    dif > 2100 ~ "1 month",
    dif> 1800 ~ "4 week",
    dif > 1000 ~ "3 week",
    dif > 500 ~ "2 week",
    dif > 200 ~ "1 week",
    dif > 60 ~ "2 day",
    TRUE ~ "1 day"
  )
  return(label_period)
}


s = raster::shapefile(SHP_FILE)
germany = raster::shapefile(GERMANY)
s = sp::spTransform(s, raster::crs(germany))

# the minimal value of the time scale
minDate = as.Date("1993-01-01")
# the maximale value of the time scale
maxDate = as.Date(today() + years(1))

# widgets of the shiny app
ui = fluidPage(
  fluidRow(column(10,
                  leafletOutput("map")),
          column(2,
                 radioButtons("mapChoice", "Mode",
                        c("Point" = 0, "Zone" = 1),
                        selected = 0),
                selectInput("CropSelect", "Select Crop",
                            choices = CROPS_CORRESPONDANCE, selected = 201),
                fileInput("geofile", "Import Geojson",
                          accept = c(".geojson"))
           )),
  fluidRow(
             sliderInput("DatesMerge", "Time Periode",
                         min = minDate,
                         max = maxDate,
                         value=c(as.Date("2015-01-01"),as.Date("2018-01-01") ),
                         timeFormat="%Y-%m-%d", width = "100%")),
  fluidRow(plotOutput("DOY_GRAPH"))
  )


server = function(input, output){
  
  observe({
    req(input$geofile)
    test <<- rgdal::readOGR(input$geofile$datapath)
    print("OK")
  })
  
  select_crop = reactive({
    # load the velox object corresponding to the selected crop
    SCrop = input$CropSelect
    return(readRDS(DATA_FILE(SCrop)))
    })

  filter_Area = reactive({
    # create the data frame for te map
    # depend of the choices of the user
    polyid = input$map_shape_click[["id"]]
    feature = input$map_draw_all_features

    if(!is.null(feature)&input$mapChoice == 0){
      shape = create_feature(feature)
    }else if(!is.null(polyid) & input$mapChoice == 1){
      proxy = leafletProxy("map")
      proxy %>% removeShape("selected")
      if(polyid != "selected"){
      shape = s[s$ID_1 == polyid,]
      proxy %>% addPolygons(data = shape, layerId = "selected",
                            fillColor = "yellow")
      }else{
        return(NULL)
        }
    } else{
      return(NULL)
    }
    
    info = select_crop()
    if(class(shape)[1] == "SpatialPoints"){
      Pd = extract_point(info[[1]], info[[2]], shape)
    }else{
      Pd = extract_polygon(info[[1]], info[[2]], shape)
    }
    return(cumsum_Pheno(Pd, digit = 2))
    
  })

  output$DOY_GRAPH = renderPlot({
    dat = filter_Area()
    if(!is.null(dat)){
      from = input$DatesMerge[1]
      to = input$DatesMerge[2]
      label_period = period_labelling(from,to)
      # name of the selected area
      
       dat %>% filter(Date > as.Date(from) & Date < as.Date(to)) %>%
         build_DOY_graph() +
        scale_x_date(name = "DOY", date_breaks = label_period,  labels = scales::date_format("%j"),
                     sec.axis = dup_axis(labels = scales::date_format("%d %b %Y")))
    }
    })

  output$map = renderLeaflet({
    # Define the map
    if(input$mapChoice == 0){
      # Point mode
      map = leaflet(germany) %>%
        addPolygons(color = "#4444EE", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0,
                    highlightOptions = highlightOptions(color = "red", weight = 3,
                                                        bringToFront = TRUE)) %>% 
        addDrawToolbar(
          polylineOptions = FALSE,
          circleOptions = FALSE,
          rectangleOptions = FALSE,
          circleMarkerOptions = FALSE,
          polygonOptions = TRUE,
          markerOptions = TRUE,
          singleFeature = TRUE,
          editOptions = editToolbarOptions(remove = FALSE)
        )
    }else{
      # Zone mode
      map = leaflet(s) %>%
      addPolygons(color = "#4444EE", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0,
                  layerId = s@data$ID_1,
                  highlightOptions = highlightOptions(color = "red", weight = 3,
                                                      bringToFront = TRUE))
    }
    return(map %>% addTiles())
  })
}

#Launch the Shiny app
shinyApp(ui = ui, server = server)
