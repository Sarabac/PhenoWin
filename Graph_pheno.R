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

# to upload large geojson files
options(shiny.maxRequestSize=30*1024^2)

shape_init = tibble(
  name = c("germany", "Climat"),
  path = c(GERMANY, SHP_FILE)
)
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
                        c("Germany" = 1),
                        selected = 1),
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


server = function(input, output, session){
  
  session$userData$choice = tibble(name = c("Germany"), path = c(GERMANY))
  
  observe({
    req(input$geofile)
    new = tibble(name = input$geofile$name, path = input$geofile$datapath)
    session$userData$choice = rbind(session$userData$choice, new)
    corres = setNames(row_number(session$userData$choice$name), session$userData$choice$name)
    updateRadioButtons(session, "mapChoice", choices = corres)
  })
  
  select_crop = reactive({
    # load the velox object corresponding to the selected crop
    SCrop = input$CropSelect
    return(readRDS(DATA_FILE(SCrop)))
    })
  
  session$userData$flag = 0
  observeEvent(input$map_shape_click, {session$userData$flag = 1})
  observeEvent(input$map_draw_all_features, {session$userData$flag = 2})

  filter_Area = reactive({
    # create the data frame for te map
    # depend of the choices of the user
    polyid = input$map_shape_click[["id"]]
    feature = input$map_draw_all_features
    proxy = leafletProxy("map")
    proxy %>% removeShape("selected")
    print(session$userData$flag)
    if(session$userData$flag == 1){
      polyid = input$map_shape_click[["id"]]
      if(polyid != "selected"){
        shape = session$userData$polyg[session$userData$polyg$ID_APP == polyid,]
        proxy %>% addPolygons(data = shape, layerId = "selected",
                              fillColor = "yellow")
      }else{
        session$userData$flag = 0
        return(NULL)
      }
    }else if (session$userData$flag == 2){
      
      shape = create_feature(feature)
    }else{
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
    # load the geojson
    dat = session$userData$choice[input$mapChoice,]
    session$userData$polyg = sp::spTransform(rgdal::readOGR(dat$path), LEAFLET_CRS)
    session$userData$polyg$ID_APP = 1:dim(session$userData$polyg@data)[1]
      # Define the map
      map = leaflet(session$userData$polyg) %>%
        addPolygons(color = "#4444EE", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0,
                    layerId = session$userData$polyg$ID_APP,
                    highlightOptions = highlightOptions(color = "red", weight = 3,
                                                        bringToFront = TRUE)) %>% 
        addDrawToolbar( targetGroup = "created",
                        polylineOptions = FALSE,
                        circleOptions = FALSE,
                        rectangleOptions = FALSE,
                        circleMarkerOptions = FALSE,
                        polygonOptions = TRUE,
                        markerOptions = TRUE,
                        singleFeature = TRUE,
                        editOptions = editToolbarOptions(remove = FALSE)
        ) %>% 
        addTiles()
      
      session$userData$flag = 0
    return(map)
  })
}

#Launch the Shiny app
shinyApp(ui = ui, server = server)
