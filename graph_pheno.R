#https://moderndata.plot.ly/plotly-4-7-0-now-on-cran/
#https://plotly-r.com/
W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(W.DIR)

library(tidyverse)
library(shiny)
library(leaflet)
library(lubridate)

source("variables_pheno.R")
source("functions_Pheno.R")

color_scale = scale_fill_manual(values = setNames(col.p, as.character(CT.P)))




build_DOY_graph = function(dat){
  graph =  ggplot(dat, aes(x = Date, y=1, alpha = sum_weight, fill = as.factor(P)))+
    geom_tile() + 
    geom_vline(aes(xintercept = as.Date(paste(year(Date), "01", "01", sep = "-"))),
               linetype = "dashed", size = 2)+ 
    geom_vline(aes(xintercept = as.Date(paste(year(Date), month(Date), "01", sep = "-"))),
               linetype = "dotted")  +
    labs(fill = "Phenology", alpha = "Probability") +
    color_fill_custom
  return(graph)
}

period_labelling = function(from, to){
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


tif_info = extract_tif_info(RU.DIR)
s = raster::shapefile(SHP_FILE)
germany = raster::shapefile(GERMANY)
#germany = raster::shapefile(WEATHERGRID)
raster::plot(germany)

minDate = as.Date(paste(min(tif_info$Year), "01","01", sep = "-"))
maxDate = as.Date(paste(max(tif_info$Year) + 1, "01","01", sep = "-"))


ui = fluidPage(
  fluidRow(column(10,
                  leafletOutput("map")),
          column(2,
                 radioButtons("mapChoice", "Map level",
                        c("Contry" = 0, "Landern" = 1),
                        selected = 0),
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


server = function(input, output){
  
  creat_point = reactive({
    click = input$map_shape_click
    lng = click[["lng"]]
    lat = click[["lat"]]
    point = sp::SpatialPoints(cbind(c(lng), c(lat)), sp::CRS("+proj=longlat +datum=WGS84"))
    return(point)
  })
  
  filter_Area = reactive({
    if(!is.null(input$map_shape_click)){
      SCrop = input$CropSelect
      if (input$mapChoice == 0){
        from = input$DatesMerge[1]
        to = input$DatesMerge[2]
        minYear = year(ymd(from)) -1
        maxYear = year(ymd(to)) +1
        selected_tif = tif_info %>% 
          filter(Crop == SCrop & Year >= minYear & Year <= maxYear) %>% 
          pull(dir)
        ph.ct = raster::stack(selected_tif )
        point = creat_point()
        ppp <<- point
        Pd = extract_DOY(ph.ct, sp::spTransform(point, raster::crs(ph.ct)))
        dat = cumsum_Pheno(Pd, digit = 1)
        proxy = leafletProxy("map")
        proxy %>% clearMarkers() %>%
          addMarkers(data = creat_point())
      }else{
      Pdoy = readRDS(DATA_FILE(SCrop))
      area_id = as.integer(input$map_shape_click[["id"]])
      if(!(area_id %in% Pdoy$Area)){area_id = unique(Pdoy$Area)[1]}
      dat = Pdoy %>% filter(Area == area_id )
      }
      
      return(dat)
    }else{
      return(NULL)
    }
      
  })

  output$DOY_GRAPH = renderPlot({
    
    dat = filter_Area()
    if(!is.null(dat)){
    from = input$DatesMerge[1]
    to = input$DatesMerge[2]
    label_period = period_labelling(from,to)
    lab_title = s@data %>% filter(ID_1 %in% dat$Area) %>% pull(NAME_1) %>% unique()

     dat %>% filter(Date > as.Date(from) & Date < as.Date(to)) %>% build_DOY_graph() +
      scale_x_date(date_breaks = label_period,  labels = scales::date_format("%j"),
                   sec.axis = dup_axis(name = lab_title,
                                       labels = scales::date_format("%d %b %Y")))  +
      theme(axis.text.x=element_text(angle=30, hjust=0.5),
            axis.text.x.top = element_text(angle = 30, vjust=0, hjust=0))
    }
    })
  
  output$map = renderLeaflet({
    
    if(input$mapChoice == 0){
      map = leaflet(germany) %>%
        addPolygons(color = "#4444EE", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0,
                    layerId = germany@data$ID_0,
                    highlightOptions = highlightOptions(color = "red", weight = 3,
                                                        bringToFront = TRUE))
    }else{
      map = leaflet(s) %>%
      addPolygons(color = "#4444EE", weight = 1, smoothFactor = 0.5,
                  opacity = 1.0, fillOpacity = 0.5,
                  fillColor = "red",
                  layerId = s@data$ID_1,
                  highlightOptions = highlightOptions(color = "white", weight = 3,
                                                      bringToFront = TRUE))
    }
    return(map %>% addTiles())
    
  })
}

shinyApp(ui = ui, server = server)

#s@data$NAME_1[input$map_shape_click[["id"]]]



test = Pdoy %>% filter(Area == 1 & Crop == 202) 
build_DOY_graph(test) + 
  coord_cartesian(xlim = c(as.Date("2016-01-01"), as.Date("2018-03-31")))

lng = 10.19892
lat = 49.38237
point = sp::SpatialPoints(cbind(c(lng), c(lat)),
                                   proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
raster::plot(germany)
raster::plot(point, add = TRUE)
Pd = extract_DOY(ph.ct, point)
sum_weight = cumsum_Pheno(Pd, digit = 1)
build_DOY_graph(sum_weight)


cell = raster::cellFromPolygon(ph.ct, point)

p = sp::spTransform(point, raster::crs(ph.ct))
Val = raster::extract(ph.ct, p)

z = raster::click(ph.ct, n=1)

k = raster::rasterToPoints(ph.ct)
raster::plot(k)
