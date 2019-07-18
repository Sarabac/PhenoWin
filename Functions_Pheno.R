
list.of.packages = c("tidyverse", "lubridate", "sp",
                     "raster", "velox", "shiny", "leaflet", "scales", "leaflet.extras", "rgdal")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)}

# functions used to extract the data and build the graphs
library(tidyverse)
library(lubridate)
library(leaflet.extras)

# variables
# variables
SHP_FILE = file.path("_Zones/gem2005_BKR_simple.shp") # Landern
GERMANY = file.path("_Zones/DEU_adm0.shp") #border of germany

RU.DIR = "_DOY/" # DOY geotif file
DATA_FOLDER = "_Data" # where the result of Extract_Pheno_Shapefile is stored
DATA_FILE = function(n){file.path(DATA_FOLDER, paste("DOY_",n,".rds", sep=""))}
# Access the phenological data of the Crop "n", ready for the graph
LEAFLET_CRS = sp::CRS("+proj=longlat +datum=WGS84")

### Colors corresponding to each phenological stage
CT.P <- c(5,10,12,14,15,17,18,19,21,22,24,67)
col.p <- c("#FFFE89", #5
           "#3288BD", #10
           "#e5f4e3", #12
           "#c4e7bf", #14
           "#ABDDA4", #15
           "#93d4c0", #17
           "#66C2A5", #18
           "#feecb9", #19
           "#FDAE61", #21
           "#FDAE61", #22
           "#F46D43", #24
           "#99c693") #67
color_fill_rule = setNames(col.p, CT.P)
color_fill_custom = scale_fill_manual(values = color_fill_rule)

# Corespondance between the name of the crop and its number
CROPS_CORRESPONDANCE = list(
  "201" = 201,
  "202" = 202,
  "204" = 204,
  "205" = 205,
  "208" = 208,
  "215" = 215
)



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
  tinfos <<- infos
  tr <<- r
  tsfObj <<- sfObj
  infos = infos %>% mutate(join=as.character(row_number()))
  repro = sf::st_transform(sfObj, crs = r$crs)
  point = repro %>% filter(is.point(geometry)) %>% mutate(RN=row_number())
  polyg = repro %>% filter(!is.point(geometry))%>% mutate(RN=row_number())
  PhenoDOY = tibble(DOY = numeric(), Area = character(), weight = numeric())
  print("avant")
  if(nrow(point)){
    print("apres")
    v = r$extract_points(point)
    colnames(v) = infos$join
    PhenoDOY = as_tibble(v) %>% mutate(RN = row_number()) %>%
      gather("join", "DOY", -RN) %>%
      inner_join(infos, by="join") %>% inner_join(point, by="RN") %>% 
      select(DOY, Area=Lid, Crop, P, Year) %>% mutate(weight = 1)
      
    return(PhenoDOY %>% extract_date())
    print("finis")
    verif <<- v
    # join the extracted values and the data frame
    PhenoDOY =  tibble(DOY = c(v), Area = point$Lid, weight = 1) %>%
      cbind(infos)
  }else{ # if the object is a polygon
  v = r$extract(repro)[[1]]
  join = 1:dim(v)[[2]]
  # create an index column to join the pixels
  # and their infos
  colnames(v) = join
  infos$join = as.character(join)
  Pd = as_tibble(v) %>% gather("join", "DOY") %>%
    inner_join(infos, by="join") %>%
    group_by(P, Year, Crop) %>%
    mutate(DOY = round(DOY), weight = 1/n()) %>% 
    group_by(P, Year, Crop, DOY) %>%
    # calculate the proportion of each DOY
    # in the polygon
    summarise(weight = sum(weight)) %>% 
    mutate(Area = 1)
  }
  return(Pd %>% drop_na() %>% extract_date())
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

create_feature = function(feature){ #### maybe leaflet::derivePoints, leaflet::derivePolygons
  # extract the coordinate of points returned by leaflet
  co = feature[["features"]][[1]][["geometry"]][["coordinates"]]
  if(feature[["features"]][[1]][["geometry"]][["type"]] == "Polygon"){
    coor = co[[1]]
    xy = tibble(x_coord = NA, y_coord = NA, .rows = length(coor))
    for(a in 1:length(coor)){
      for(b in 1:length(coor[[a]])){
        xy[a,b] = coor[[a]][[b]]
      }
    }
    p = sp::Polygon(xy)
    ps = sp::Polygons(list(p),1)
    sps = sp::SpatialPolygons(list(ps), proj4string=LEAFLET_CRS)
  }else{
    p = cbind(c(co[[1]]), c(co[[2]]))
    sps = sp::SpatialPoints(p, proj4string=LEAFLET_CRS)
  }
  # assign the WG84 projection
  return(sps)
}

build_DOY_graph = function(dat){
  # create the Phenological graph from the data frame "dat"
  # with 3 columns:
  # "Date": class Date
  # "sum_weight": between 0 and 1
  # "P": phenological stage
  graph =  ggplot(dat, aes(x = Date, y=1, alpha = sum_weight, fill = as.factor(P)))+
    geom_tile() +
    facet_grid(Area~Crop)+
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
                    polygonOptions = TRUE,
                    markerOptions = TRUE,
                    singleFeature = TRUE,
                    editOptions = editToolbarOptions(remove = FALSE)
    ) %>% 
    addSearchOSM() %>% addResetMapButton() %>%
    addTiles(group = "OpenStreetMap") %>%
    addProviderTiles("Esri.WorldImagery", group = "Orthos") %>%
    addProviderTiles("OpenTopoMap", group = "OpenTopoMap")
  return(map)
}

create_layer = function(map, shape, color="green"){
  for(nam in unique(shape$name)){
    point <<- shape %>% filter(nam==name&is.point(geometry))
    polyg = shape %>% filter(nam==name&!is.point(geometry))
    if (nrow(point)){
      map = map %>%
        addAwesomeMarkers(icon = awesomeIcons(markerColor=color),
                          layerId = point$Lid,
                          label = as.character(point$IDs),
                          labelOptions = labelOptions(noHide = T),
                          group = nam,
                          data = point)
    }
    if (nrow(polyg)){
      map = map %>% 
        addPolygons(color = color, weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0,
                    layerId = polyg$Lid,
                    group = nam,
                    data = polyg,
                    label = as.character(polyg$IDs),
                    labelOptions = labelOptions(noHide = T),
                    highlightOptions = highlightOptions(
                      color = "red", weight = 3, bringToFront = TRUE))
    }
  }
  return(map)
}

create_layerControl = function(map, groupNames = c()){
  return(addLayersControl(map,
    baseGroups = c("OpenStreetMap", "OpenTopoMap","Orthos"),
    overlayGroups =c(groupNames, c("Selected")),
    options = layersControlOptions(collapsed = FALSE)
  ))
}

load4leaflet = function(path, name, ID_var=""){
  polyg <<- sf::st_transform(sf::read_sf(path),LEAFLET_CRS)
  ID_var <<-ID_var
  if(ID_var==""){
    result = transmute(polyg, IDs = row_number())
  }else{
    result = select(polyg, IDs = !!ID_var)
  }
  return(result %>%
           mutate(name = name) %>% mutate(Lid = paste(name, IDs, sep="_")))
  }
