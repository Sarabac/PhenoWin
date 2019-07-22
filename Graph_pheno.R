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

#shape_init = rbind(
#  load4leaflet(SHP_FILE, "Climat"),
#  load4leaflet(GERMANY, "Germany")
#)

shape_init = load4leaflet(GERMANY, "Germany")

# the minimal value of the time scale
minDate = as.Date("1993-01-01")
# the maximale value of the time scale
maxDate = as.Date(today() + years(1))

# widgets of the shiny app
ui = fluidPage(
  tags$head(# css styles
    tags$style(HTML("
      #compute{background-color:GreenYellow }
      *{font: bold 12px/30px Arial, serif}
      "))
  ),
  fluidRow(
    column(4,
           actionButton("compute", "Compute", icon = icon("play")),
           actionButton("deselectAll", "Deselect All")),
    column(2, selectInput("CropSelect", "Select Crop",
                choices = CROPS_CORRESPONDANCE)),
    column(2, fileInput("geofile", "Import Geojson",
          accept = c(".geojson")))),
  fluidRow(leafletOutput("map")),
  fluidRow(
             sliderInput("DatesMerge", "Time Periode",
                         min = minDate,
                         max = maxDate,
                         value=c(as.Date("2015-01-01"),as.Date("2018-01-01") ),
                         timeFormat="%Y-%m-%d", width = "100%")),
  fluidRow(plotOutput("DOY_GRAPH"))
  )


server = function(input, output, session){
  
  session$userData$shapes = shape_init
  session$userData$currentGeo = list()
  
  observe({
    # when the user upload a Geojson file
    req(input$geofile)
    # remember its path and name
    session$userData$currentGeo["name"] = input$geofile$name
    session$userData$currentGeo["path"] = input$geofile$datapath
    #take the potential IDs for the layer. The user will choose one.
    idvar = input$geofile$datapath %>% sf::read_sf() %>%
      sf::st_drop_geometry() %>% colnames()
    #There is also the possibility to create IDs based on row number
    choices = setNames( c("", idvar), c("Auto Generated", idvar))
    showModal(modalDialog(
      radioButtons("varchoice", "Choice of the variable", choices=choices),
      footer = tagList(actionButton("ok", "OK")))
      )
  })
  
  observeEvent(input$ok,{
    #When the choice of the IDs is made
    infos = session$userData$currentGeo #retrive the file data
    newShape = load4leaflet(infos$path,
                            str_remove(infos$name, "\\..*$"),
                            input$varchoice)
    session$userData$shapes = rbind(
      session$userData$shapes,
      newShape
    )# add the shapes to the previous ones
    leafletProxy("map") %>% create_layer(newShape) %>%
      create_layerControl(unique(session$userData$shapes$name))
    removeModal()
  })
  
  select_crop = reactive({
    # load the velox object corresponding to the selected crop
    SCrop = input$CropSelect
    return(readRDS(DATA_FILE(SCrop)))
    })
  
  on_click = function(clickID){
    # general function when a feature is clicked
    if(is.null(clickID)){return(NULL)}
    Clid <<-clickID
    shapeChanged = session$userData$shapes %>%
      mutate(different = Lid%in%clickID) %>% 
      mutate(selected = xor(selected, different))
    #reverse selection if id detected
    session$userData$shapes = select(shapeChanged, -different)
    ch <<- shapeChanged
    usr <<- session$userData$shapes
    leafletProxy("map") %>%
      create_layer(filter(shapeChanged, different))
    }
  
  #track click on the polygons or points
  observe({on_click(input$map_shape_click[["id"]])})   #for polygons   
  observe({on_click(input$map_marker_click[["id"]])})  #for points
  observeEvent(input$map_draw_new_feature, {
    print("New Feature")
    drdd <<- input$map_draw_new_feature
    newF <<- create_feature(drdd)
    Ncustom = session$userData$shapes %>% 
      filter(name=="Custom") %>% nrow()
    print(Ncustom)
    NewSF = tibble(
      IDs = as.character(Ncustom+1),
      geometry = newF,
      name="Custom",
      selected=TRUE
    ) %>% mutate(Lid = paste(name, IDs, sep="_")) %>% 
      sf::st_sf()
    session$userData$shapes = NewSF %>% 
      rbind(session$userData$shapes)
    leafletProxy("map") %>% create_layer(NewSF)
    
  })
  
  observeEvent(input$deselectAll,{
    # deselect all features
    session$userData$shapes = session$userData$shapes %>%
      mutate(selected = FALSE)
    leafletProxy("map") %>% # update the map
      create_layer(session$userData$shapes) 
  })
  

  Build_Dataset = eventReactive(input$compute,{
    # create the data frame for the graph

    #take all the selected features
    selected = session$userData$shapes %>% filter(selected)
    print(selected)
    if(!nrow(selected)){return(NULL)}#stop if nothing is selected
    
    infos = select_crop()# the reactive function above
    #info[[1]] about the layer (Crop, Year, Phase)
    #info[[2]] the velox objet related to info[[1]]
    Pd = extract_velox(infos[[1]], infos[[2]], selected)
    sum_Pd = cumsum_Pheno(Pd, digit = 2) %>% 
      inner_join(select(selected, name, IDs, Lid), by=c("Area"="Lid"))
    #set more inforation about the area
    return(sum_Pd)
  })

  output$DOY_GRAPH = renderPlot({
    # Draw the graph
    dat = Build_Dataset()
    if(is.null(dat)){return(NULL)}
    from = input$DatesMerge[1]
    to = input$DatesMerge[2]
    label_period = period_labelling(from,to)
    
    graph = dat %>% #remove the data not within the time scale
      filter(Date > as.Date(from) & Date < as.Date(to)) %>%
      build_DOY_graph(date_breaks=label_period,
                      user_facet = facet_grid(name+IDs ~ Crop))

    return(graph)
    })

  output$map = renderLeaflet({
    #initialize the map
    shapes = session$userData$shapes # the initial polygones
    map = create_map() %>%
      create_layer(shapes) %>% 
      create_layerControl(unique(shapes$name))
    return(map)
  })
}

#Launch the Shiny app
shinyApp(ui = ui, server = server)


