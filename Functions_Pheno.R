
list.of.packages = c("tidyverse", "lubridate", "sf",
                     "raster", "velox", "shiny", "leaflet", "scales", "leaflet.extras", "rgdal")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)}

# functions used to extract the data and build the graphs
library(tidyverse)
library(lubridate)
library(leaflet.extras)

### Variables
GERMANY = file.path("_Zones/DEU_adm0.shp") #border of germany

RU.DIR = "_DOY/" # DOY geotif file
DATA_FOLDER = "_Data" # where the result of Extract_Pheno_Shapefile is stored
DATA_FILE = function(n){file.path(DATA_FOLDER, paste("DOY_",n,".rds", sep=""))}
# Access the phenological data of the Crop "n", ready for the graph
LEAFLET_CRS = sf::st_crs(4326)


### Colors corresponding to each phenological stage
phasesCode = read.csv("Phases.csv") %>% arrange(Code)

# Corespondance between the name of the crop and its number
CROPS_CORRESPONDANCE_FRAME = read.csv(
  "crops.csv",
  colClasses = c ("numeric", "character")
  )
CROPS_CORRESPONDANCE = setNames(
  as.list(CROPS_CORRESPONDANCE_FRAME$Crop),
  CROPS_CORRESPONDANCE_FRAME$Crop_name
  )

### Functions 

extract_n = function(dat, n){
  # extract a number of length n from a character vector
  as.integer(str_extract(dat, paste("(?:(?<!\\d)\\d{",n,"}(?!\\d))", sep="")))
}

extract_code = function(dat){
  tibble(
    Crop = extract_n(dat, 3),
    Year = extract_n(dat, 4),
    # Phenology have a lenght 1 or 2
    P = coalesce(extract_n(dat, 2),extract_n(dat, 1))
  )
}

extract_tif_info = function(directory){
  # extract the path of each geotif in a directory with
  # the Crop number, the year and the phenological stage
  tibble(dir = list.files(directory, full.names = T)) %>%
    filter(str_detect(.$dir, ".tif$")) %>% # take all the .tif files
    bind_cols(extract_code(.$dir))
}

extract_date = function(dat){
  mutate(dat, DOY = round(DOY)) %>%
    mutate(Date = as.Date(paste(Year,"01-01",sep="-")) + days(DOY - 1))
}

extract_velox = function(infos, r, sfObj){
  # infos: data frame contaning  the crop, date, phase
  # r: velox object
  # sfObj: sf object
  infos = infos %>% mutate(join=as.character(row_number()))
  repro = sf::st_transform(sfObj, crs = r$crs)
  point = repro %>% filter(is.point(geometry)) %>%
    mutate(RN=row_number())
  polyg = repro %>% filter(!is.point(geometry)) %>%
    mutate(RN=row_number()) %>%
    sf::st_cast("MULTIPOLYGON")# for homogenous geometry classes
  PhenoDOY = tibble(Area = character(), Crop = factor(), P=factor(),
                    DOY = numeric(), weight = numeric())
  if(nrow(point)){
    v = r$extract_points(point)
    colnames(v) = infos$join
    PhenoDOY = as_tibble(v) %>% mutate(RN = row_number()) %>%
      gather("join", "DOY", -RN) %>%
      inner_join(infos, by="join") %>%
      inner_join(sf::st_drop_geometry(point), by="RN") %>% 
      select(DOY, Area=Lid, Crop, P, Year) %>% mutate(weight = 1) %>% 
      bind_rows(PhenoDOY)
  }
  if (nrow(polyg)){
  v = r$extract(polyg, df=TRUE, small = TRUE)
  colnames(v) = c("RN", infos$join) # first column is polygone ID
  PhenoDOY = as_tibble(v) %>%
    gather("join", "DOY", -RN) %>%
    inner_join(infos, by="join") %>%
    inner_join(sf::st_drop_geometry(polyg), by="RN") %>% 
    select(DOY, Area=Lid, Crop, P, Year) %>%
    group_by(Area, Crop, P, Year) %>%
    mutate(DOY = round(DOY), weight = 1/n()) %>% 
    group_by(Area, Crop, P, Year, DOY) %>%
    # calculate the proportion of each DOY
    # in the polygon
    summarise(weight = sum(weight)) %>% 
    bind_rows(PhenoDOY) %>% 
    ungroup()
  }
  return(PhenoDOY %>% drop_na() %>% extract_date())
}


date_cluster = function(date){
  # group together phenological change that
  # happend in simalar date
  # they correspond to the same event
  dist_mat = dist(as.integer(date), method = 'euclidean')
  hsingle <- hclust(dist_mat, method = "single") # cluster by min time periode
  # if less than 10 day separate 2 same phenological stage: it is the same event
  groups = cutree(hsingle, h=10) 
  return(groups)
}

####
cumsum_Pheno = function(weighted_pixels, digit = 2){
  # digit : precision of the weight of the phenological stage
  
  Pheno_order = weighted_pixels %>%
    group_by(Area, Crop, P) %>%
    mutate(group = date_cluster(Date)) %>% 
    group_by(Area, Crop, P, group) %>% 
    summarise(start = min(Date),
              end = max(Date))%>%
    group_by(Area, Crop) %>%
    arrange(Area, Crop, as.numeric(start)) %>%
    mutate(P_order = row_number()) %>%
    # a phenological window stop when 
    # all the pixels reach the next phinological stage
    mutate(stop = c(end[-1], last(end))) %>%
    ungroup()
  # generate all the date in a phenological window
  total_period = Pheno_order %>%
    group_by(Area, Crop, P_order) %>%
    expand(Date = as.Date(seq.Date(start,stop, by="day"))) %>%
    ungroup() %>%
    inner_join(Pheno_order %>%
                 select(Area, Crop, P, P_order),
               by = c("Area", "Crop", "P_order"))

  sum_weight = left_join(total_period, weighted_pixels, by = c("Area", "Crop", "Date", "P")) %>%
    fill(P) %>% mutate(weight = replace_na(weight, 0)) %>% # fill the NA caused by the left join
    mutate(Year = year(Date)) %>%
    group_by(Area, Crop, P_order) %>%
    mutate(sum_weight = round(cumsum(weight), digit = digit)) %>%
    filter(sum_weight > 0) %>% # remove useless dates
    ungroup() %>% 
    arrange(P_order)

  return(sum_weight)
}

create_feature = function(feature){
  # extract the coordinate of points returned by leaflet
  co = feature[["geometry"]][["coordinates"]]
  if(feature[["geometry"]][["type"]] == "Polygon"){
    coor = co[[1]]
    xy = matrix(nrow = length(coor), ncol = 2)
    for(a in 1:length(coor)){
      for(b in 1:length(coor[[a]])){
        xy[a,b] = coor[[a]][[b]]
      }
    }
    sps = sf::st_polygon(list(xy))
  }else{
    sps = sf::st_point(c(co[[1]],co[[2]]))
  }
  geom_set = sf::st_sfc(sps, crs=LEAFLET_CRS)
  # assign the WG84 projection
  return(geom_set)
}

build_DOY_graph = function(dat, date_breaks=waiver(),
                           user_facet=facet_grid(Area~Crop)){
  # create the Phenological graph from the data frame "dat" with attributs:
  #     "Date": class Date
  #     "sum_weight": between 0 and 1, proportion of pixels in phase P
  #     "P": phenological stage
  #     "Area": spatial entities ID
  #     "Crop": the crop ID
  # date_breaks: character
  fphasesCode = filter(phasesCode, Code%in%dat$P)
  color_fill_rule = as.vector(fphasesCode$Color)
  names(color_fill_rule) <- fphasesCode$Code
  graph =  ggplot(dat, aes(x = Date, y=1, alpha = sum_weight,
                           fill = as.factor(P)))+
    geom_tile() + 
    user_facet+
    scale_fill_manual(values = color_fill_rule) + # color fill scall of the phenological stages
    geom_vline(aes(
          xintercept = as.Date(paste(year(Date),"01", "01", sep="-")),
          linetype = "Year"), size = 2)+
    geom_vline(aes(
          xintercept = as.Date(paste(year(Date), month(Date), "01", sep="-")),
                   linetype = "Month"))  +
    labs(fill = "Phenology", alpha = "Weight") +
    
    scale_x_date(name="DOY", date_breaks=date_breaks,
                 labels=scales::date_format("%j"),
                 sec.axis=dup_axis(
                   name="Date",labels = scales::date_format("%d %b %Y"))) +
    scale_linetype_manual("Breaks", 
                          values = c("Month"="dotted", "Year"="dashed")) +
    theme(axis.text.x=element_text(angle=30, hjust=1),
          axis.text.x.top = element_text(angle = 30, vjust=0, hjust=0),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank()
          )
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
is.point = function(geometry){
  sf::st_geometry_type(geometry) %in% c("POINT","MULTIPOINT")
}
create_map = function(){
  # origin is a shapefile which extent is the default
  # map extent
  map = leaflet() %>%
    addDrawToolbar( targetGroup = "created",
                    polylineOptions = FALSE,
                    circleOptions = FALSE,
                    rectangleOptions = FALSE,
                    circleMarkerOptions = FALSE,
                    singleFeature = TRUE
    ) %>% 
    addSearchOSM() %>% addResetMapButton() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Orthos") %>%
    addProviderTiles("OpenTopoMap", group = "OpenTopoMap")
  return(map)
}

create_layer = function(map, shape){
  if(!nrow(shape)){return(map)} # no change if nothing to add
  for(Li in shape$Lid){
    sh = shape %>% filter(Lid==Li)
    
    if(is.na(sh$selected)){
      color = "black"
      fillOpacity = 0
      highlightOpt = NULL
      label = NULL
      layerId = NULL
    }else{
      color = ifelse(sh$selected, "red", "blue")
      fillOpacity = 0.3
      highlightOpt = highlightOptions(
        color = "orange", weight = 3, bringToFront = TRUE)
      label = as.character(sh$IDs)
      layerId = sh$Lid
    }
    if (is.point(sh)){
      map = map %>% removeMarker(sh$Lid) %>% 
        addAwesomeMarkers(icon = awesomeIcons(markerColor=color),
                          layerId = layerId,
                          label = label,
                          labelOptions = labelOptions(noHide = T),
                          group = sh$name,
                          data = sh)
    }else{
      map = map %>% removeShape(sh$Lid) %>% 
        addPolygons(color = color, weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = fillOpacity,
                    layerId = layerId,
                    group = sh$name,
                    data = sh,
                    label = label,
                    labelOptions = labelOptions(noHide = T),
                    highlightOptions = highlightOpt)
    }
  }
  return(map)
}

create_layerControl = function(map, groupNames = c()){
  return(addLayersControl(map,
    baseGroups = c("OpenStreetMap", "OpenTopoMap","Orthos"),
    overlayGroups=groupNames,
    options = layersControlOptions(collapsed = FALSE)
  ))
}

load4leaflet = function(path, name, ID_var=""){
  polyg=sf::st_transform(sf::read_sf(path),LEAFLET_CRS)
  if(ID_var==""){
    result = transmute(polyg, IDs = row_number())
  }else{
    result = dplyr::select(polyg, IDs = !!ID_var)
  }
  return(result %>%
           mutate(name = name, selected = FALSE) %>%
           mutate(Lid = paste(name, IDs, sep="_")))
  }
