#https://moderndata.plot.ly/plotly-4-7-0-now-on-cran/
#https://plotly-r.com/
#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
W.DIR <- "~/Dropbox/Kuhn/phenology/PhenoWin"
setwd(W.DIR)

source("Functions_Pheno.R")

library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(lubridate)

# to upload large geojson files
options(shiny.maxRequestSize=30*1024^2)
addResourcePath("_Images", "_Images")

# load the border of Germany
shape_init = load4leaflet(GERMANY, "Germany")
# prevent selecton on the entier Germany
shape_init$selected = NA

# the minimal value of the time scale
minDate = as.Date("1993-01-01")
# the maximale value of the time scale
maxDate = as.Date(today() + years(1))

# widgets of the shiny app
title_div = h1(img(src="_Images/EMRA_Logo.svg", width="30px"),
                "PhenoWin: Visualisation of phenological windows in Germany" )
ui = navbarPage(
  selected = "APP",
  windowTitle = "PhenoWin",
  title_div,
  tags$head(
    tags$link(rel="shortcut icon", href="_Images/EMRA_Logo.ico")
  ),
  includeCSS("_Images/interface.css"),
  tabPanel("APP",
    fluidRow(
      div(actionButton("compute", "Draw Graph", icon = icon("play")),
          actionButton("deselectAll", "Deselect All")),
      div(checkboxInput("polar", "Polar Graph"),
          sliderInput("height", "Graph Height", min=400, max=5000, value=500)),
      selectInput("CropSelect", "Select Crop",
                  choices = CROPS_CORRESPONDANCE),
      radioButtons("clickSelect", "Mode:", inline=TRUE,
                          choices=c("Select"="select", "Delete"="delete"),
                          selected="select"),
      fileInput("geofile", "Import Geojson", accept = c(".geojson")),
      div(
        downloadButton("downloadData", "Download Extracted Dataset") ,
        downloadButton("downloadPhase", "Download Phase Code"),
        downloadButton("downloadGeojson", "Export Geojson")),
      class = "buttons"
    ),
  fluidRow(leafletOutput("map")),
  fluidRow(
             sliderInput("DatesMerge", NULL,
                         min = minDate,
                         max = maxDate,
                         value=c(as.Date("2015-01-01"),as.Date("2018-01-01") ),
                         timeFormat="%Y-%m-%d", width = "100%")),
  fluidRow(plotOutput("DOY_GRAPH"))
  ),
  tabPanel("HELP", includeMarkdown("README.md"))
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
    newShape = load4leaflet(infos$path, #remove the file extension
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
  
  on_click = function(clickID){
    # function called when a feature is clicked
    if(is.null(clickID)){return(NULL)}
    
    mode = isolate({input$clickSelect})
    if(mode=="select"){ # select mode
    shapeChanged = session$userData$shapes %>%
      mutate(different = Lid%in%clickID) %>% 
      mutate(selected = xor(selected, different))
    # xor reverse selection if Lid detected
    session$userData$shapes = dplyr::select(shapeChanged, -different)
    leafletProxy("map") %>%
      create_layer(filter(shapeChanged, different))
    }else{ # delete mode
      session$userData$shapes = session$userData$shapes %>% 
        filter(!Lid%in%clickID)
      leafletProxy("map") %>% removeShape(clickID) %>% 
        removeMarker(clickID)
    }
  }
  
  #track click on the polygons or points
  observe({on_click(input$map_shape_click[["id"]])})   #for polygons   
  observe({on_click(input$map_marker_click[["id"]])})  #for points
  observeEvent(input$map_draw_new_feature, {
    drawing_infos = input$map_draw_new_feature
    newF = create_feature(drawing_infos) # geometry object
    #determin how many features already exist
    Ncustom = session$userData$shapes %>% 
      filter(name=="Custom") %>% nrow()
    #create the new feature
    NewSF = tibble(
      IDs = as.character(Ncustom+1), # increase the ID number
      geometry = newF,
      name="Custom",
      selected=TRUE
    ) %>% mutate(Lid = paste(name, IDs, sep="_")) %>% 
      sf::st_sf() # convert into sf
    #add tto the user list
    session$userData$shapes = NewSF %>% 
      rbind(session$userData$shapes)
    #add to the map
    leafletProxy("map") %>%
      create_layer(NewSF) %>%
      # 
      create_layerControl(unique(session$userData$shapes$name))
    
  })
  
  observeEvent(input$deselectAll,{
    # deselect all features
    session$userData$shapes = session$userData$shapes %>%
      mutate(selected = if_else(is.na(selected), NA, FALSE))
    leafletProxy("map") %>% # update the map
      create_layer(filter(session$userData$shapes, !selected)) 
  })
  

  Extract_Dataset = eventReactive(input$compute,{
    # create the data frame for the graph

    #take all the selected features
    showModal(modalDialog(helpText("Loading"), footer =NULL))
    selected = session$userData$shapes %>% filter(selected)
    ss <<- selected
    if(!nrow(selected)){
      removeModal()
      return(NULL)
    }#stop if nothing is selected
    # load the velox object corresponding to the selected crop
    SCrop = input$CropSelect
    infos = readRDS(DATA_FILE(SCrop))
    #info[[1]] about the layer (Crop, Year, Phase)
    #info[[2]] the velox objet related to info[[1]]
    Pd = extract_velox(infos[[1]], infos[[2]], selected)
    removeModal()
    return(Pd)
  })
    
  Build_Dataset = reactive({
    Pd = Extract_Dataset()
    if(is.null(Pd)){return(NULL)}
    sum_Pd = cumsum_Pheno(Pd, digit = 2) %>% 
      inner_join( # get the user name of the area
        dplyr::select(sf::st_drop_geometry(session$userData$shapes),
                        name, IDs, Lid),
        by=c("Area"="Lid")
        )
    #set more inforation about the area
    return(sum_Pd)
  })
  observeEvent(input$compute, {
  output$DOY_GRAPH = renderPlot({
    # Draw the graph
    dataset = Build_Dataset()
    if(is.null(dataset)){ # error message
      return(
      ggplot() + 
        geom_text(aes(label = lab, x=x, y=y),
                  tibble(lab = c("No Area selected"), x=0, y=0))
    )}
      
    dat = dataset %>% 
    #join with the name of the crop
      left_join(CROPS_CORRESPONDANCE_FRAME, by = "Crop") %>% 
      mutate(Crop = coalesce(Crop_name , as.character(Crop)))
    if(is.null(dat)){return(NULL)}
    from = input$DatesMerge[1]
    to = input$DatesMerge[2]
    label_period = period_labelling(from,to)
    
    graph = dat %>% #remove the data not within the time scale
      filter(Date > as.Date(from) & Date < as.Date(to)) %>%
      build_DOY_graph(date_breaks=label_period,
                      user_facet = facet_grid(name+IDs ~ Crop),
                      polar = input$polar)

    return(graph)
  }, height = input$height)
  })
  
  output$downloadData <- downloadHandler(
    filename = "PhenoWin.csv",
    content = function(file) {
      Pd = Extract_Dataset() %>% 
        left_join(CROPS_CORRESPONDANCE_FRAME, by = "Crop") %>% 
        mutate(Crop = coalesce(Crop_name , as.character(Crop))) %>% 
        dplyr::select(-Crop_name)
      if(is.null(nrow(Pd))){
        write.csv(tibble(), file, row.names = FALSE)
      }else{
        write.csv(Pd, file, row.names = FALSE)
      }
    })
    
  output$downloadPhase <- downloadHandler(
    filename = "Phase.csv",
    content = function(file) {
      write.csv(phasesCode, file, row.names = FALSE)
    })
  output$downloadGeojson <- downloadHandler(
    filename = "features.Geojson",
    content = function(file) {
      shape_export = drop_na(session$userData$shapes, selected)
      sf::st_write(shape_export, file)
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


