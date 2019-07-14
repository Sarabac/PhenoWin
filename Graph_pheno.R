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
  name = c("Germany", "Climat"),
  path = c(GERMANY, SHP_FILE),
  Id_var = ""
)

choice4mapchoice = function(name){
  setNames(1:length(name), name)
}

# the minimal value of the time scale
minDate = as.Date("1993-01-01")
# the maximale value of the time scale
maxDate = as.Date(today() + years(1))

# widgets of the shiny app
ui = fluidPage(
  fluidRow(column(10,
                  leafletOutput("map")),
          column(2,
                 radioButtons("mapChoice", "Polygons",
                        choice4mapchoice(shape_init$name),
                        selected = 1),
                 fileInput("geofile", "Import Geojson",
                           accept = c(".geojson")),
                selectInput("CropSelect", "Select Crop",
                            choices = CROPS_CORRESPONDANCE, selected = 201)
                
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
  
  session$userData$choice = shape_init # initial shapfiles
  
  observe({
    # when the user upload a Geojson
    req(input$geofile)
    # add its path to the previous geojsons path
    new = tibble(name = input$geofile$name, path = input$geofile$datapath, Id_var = "")
    session$userData$choice = rbind(session$userData$choice, new)
    # generate the options for the Radiobuttons
    corres = choice4mapchoice(session$userData$choice$name)
    # Uptdate the radiobutton with the new options
    updateRadioButtons(session, "mapChoice", choices = corres)
    idvar = colnames(rgdal::readOGR(new$path)@data)
    choices = setNames( c(NA, idvar), c("Auto Generated", idvar))
    showModal(modalDialog(
      radioButtons("varchoice", "Choice of the variable",choices = choices),
      footer = tagList(
        actionButton("ok", "OK")))
      )
  })
  observeEvent(input$ok,{
    len = dim(session$userData$choice)[1]
    session$userData$choice[len, "Id_var"] = input$varchoice
    usertest <<- session$userData$choice
    removeModal()
    
  })
  
  select_crop = reactive({
    # load the velox object corresponding to the selected crop
    SCrop = input$CropSelect
    return(readRDS(DATA_FILE(SCrop)))
    })
  # the flag determins if the user is drawing or clicking on a polygon
  session$userData$flag = 0
  observeEvent(input$map_shape_click, {session$userData$flag = 1})
  observeEvent(input$map_marker_click, {session$userData$flag = 1})
  observeEvent(input$map_draw_all_features, {session$userData$flag = 2})

  filter_Area = reactive({
    # create the data frame for the graph
    
    polyclick = input$map_shape_click # do not move it: trigger the event
    markerclick = input$map_marker_click
    cinf <<- input$map_marker_click
    feature = input$map_draw_all_features  # do not move it: trigger the event
    is_marker <<- class(session$userData$polyg)[1] %in% c("SpatialPoints", "SpatialPointsDataFrame")
    proxy = leafletProxy("map")
    # if a yellow selection polygon have been created, it is erased
    proxy %>% clearGroup("Selected")
    
    if(session$userData$flag == 1){
      # when the user is clicking
      if(is_marker){
        shapeclick = markerclick
      }else{
        shapeclick = polyclick
      }
      if(shapeclick[["group"]] != "Selected"){
        # extract the clicked polygon
        shapeid = shapeclick[["id"]]
        shape = session$userData$polyg[session$userData$polyg$ID_APP == shapeid,]
        if(is_marker){
          proxy %>% addAwesomeMarkers(data = shape, layerId = "S",
                     icon = awesomeIcons(markerColor = "red"), group = "Selected")
        }else{
        # add a yellow polygon on top of it to show that it is selected
        proxy %>% addPolygons(data = shape, layerId = shapeid,
                              fillColor = "red", group = "Selected")
        }
      }else{
        # nothing happend if the user click on the already selected feature
        session$userData$flag = 0
        return(NULL)
      }
      
    }else if (session$userData$flag == 2){
      # when the user is drawing
      shape = create_feature(feature)
      
    }else{
      # if session$userData$flag = 0
      return(NULL)
    }
    testmap <<-getMapData(proxy)
    info = select_crop()
    Pd = extract_velox(info[[1]], info[[2]], shape)
    return(cumsum_Pheno(Pd, digit = 2))
    
  })

  output$DOY_GRAPH = renderPlot({
    # Draw the graph
    dat = filter_Area()
    if(!is.null(dat)){
      from = input$DatesMerge[1]
      to = input$DatesMerge[2]
      label_period = period_labelling(from,to)
      # name of the selected area
      
      graph =dat %>% filter(Date > as.Date(from) & Date < as.Date(to)) %>%
         build_DOY_graph() +
        scale_x_date(name="DOY", date_breaks=label_period, labels=scales::date_format("%j"),
                     sec.axis=dup_axis(name="Date", labels = scales::date_format("%d %b %Y")))
      return(graph)
    }
    })

  output$map = renderLeaflet({
    # load the geojson
    dat = session$userData$choice[input$mapChoice,]
    print(dat)
    session$userData$polyg = sp::spTransform(rgdal::readOGR(dat$path, verbose = FALSE),
                                             LEAFLET_CRS)
    # create an ID column to select each polygon
    if(dat$Id_var==""){
      ID_APP = 1:dim(session$userData$polyg@data)[1]
    }else{
      ID_APP = pull(session$userData$polyg@data, dat$Id_var)
    }
    print(ID_APP)
    session$userData$polyg$ID_APP = ID_APP
      # Define the map
      map = leaflet(session$userData$polyg) %>%
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
        addSearchOSM() %>% addResetMapButton() %>%
        addTiles(group = "OpenStreetMap") %>%
        addProviderTiles("Esri.WorldImagery", group = "Orthos") %>%
        addProviderTiles("OpenTopoMap", group = "OpenTopoMap") %>%
        addLayersControl(
          baseGroups = c("OpenStreetMap", "OpenTopoMap","Orthos"),
          overlayGroups = c("Geojson", "Selected")
        )
      
      if (class(session$userData$polyg)[1] %in% c("SpatialPoints", "SpatialPointsDataFrame")){
        map = map %>% addAwesomeMarkers(icon = awesomeIcons(markerColor = "green"),
                                        layerId = session$userData$polyg$ID_APP,
                                        label = as.character(session$userData$polyg$ID_APP),
                                        labelOptions = labelOptions(noHide = T),
                                        group = "Geojson")
      }else{
        map = map %>% addPolygons(color = "green", weight = 1, smoothFactor = 0.5,
                            opacity = 1.0, fillOpacity = 0,
                            layerId = session$userData$polyg$ID_APP,
                            group = "Geojson",
                            label = as.character(session$userData$polyg$ID_APP),
                            labelOptions = labelOptions(noHide = T),
                            highlightOptions = highlightOptions(color = "red", weight = 3,
                                                                bringToFront = TRUE))
      }
      
      session$userData$flag = 0
      
    return(map)
  })
}

#Launch the Shiny app
shinyApp(ui = ui, server = server)
