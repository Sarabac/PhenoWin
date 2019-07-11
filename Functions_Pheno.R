
list.of.packages = c("tidyverse", "lubridate", "sp",
                     "raster", "velox", "shiny", "leaflet", "scales")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)){install.packages(new.packages)}

# functions used to extract the data and build the graphs
library(tidyverse)
library(lubridate)

# variables
# variables
SHP_FILE = file.path("_Zones/gem2005_BKR_simple.shp") # Landern
GERMANY = file.path("_Zones/DEU_adm0.shp") #border of germany

RU.DIR = "_DOY/" # DOY geotif file
DATA_FOLDER = "_Data" # where the result of Extract_Pheno_Shapefile is stored
DATA_FILE = function(n){file.path(DATA_FOLDER, paste("DOY_",n,".rds", sep=""))}
# Access the phenological data of the Crop "n", ready for the graph

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
    mutate(Date = as.Date(paste(Year,DOY,sep="-"), "%Y-%j"))
}


extract_point = function(infos, r, p){
  # infos: data frame contaning  the crp, date, stage
  # r: velox object
  # p: point
  repro = sp::spTransform(p, CRSobj = sp::CRS(r$crs, TRUE))
  v = r$extract_points(repro)
  # join the extracted values and the data frame
  da = tibble(DOY = c(v), Area = 1, weight = 1) %>%
    cbind(infos) %>% drop_na() %>%  extract_date()
  return(da)
}

extract_polygon = function(infos, r, p){
  # infos: data frame contaning  the crp, date, stage
  # r: velox object
  # p: point
  repro = sp::spTransform(p, CRSobj = sp::CRS(r$crs, TRUE))
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
    mutate(Area = 1) %>%
    drop_na() %>% 
    extract_date()
  return(Pd)
}


####
cumsum_Pheno = function(weighted_pixels, digit = 2){
  # digit : precision of the weight of the phenological stage
  Pheno_order = weighted_pixels %>%
    group_by(Area, Crop, Year, P) %>%
    summarise(start = as.Date(min(Date)),
              end = as.Date(max(Date)))%>%
    group_by(Area, Crop) %>%
    arrange(Area, Crop, as.numeric(start)) %>%
    mutate(P_order = row_number()) %>%
    mutate(stop = c(end[-1], last(end))) %>%
    ungroup()

  total_period = Pheno_order %>%
    group_by(Area, Crop, P_order) %>%
    expand(as.Date(seq.Date(start,stop, by="day"))) %>%
    ungroup()%>% rename(Date = 4) %>%
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
