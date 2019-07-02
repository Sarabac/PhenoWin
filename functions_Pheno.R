

library(tidyverse)
library(lubridate)

source("variables_pheno.R")

extract_n = function(dat, n){
  as.integer(str_extract(dat, paste("(?:(?<!\\d)\\d{",n,"}(?!\\d))", sep="")))
}

extract_code = function(dat){
  tibble(
    Crop = extract_n(dat, 3),
    Year = extract_n(dat, 4),
    P = coalesce(extract_n(dat, 2),extract_n(dat, 1))
  )
}

extract_tif_info = function(directory){
  tibble(dir = list.files(directory, full.names = T)) %>% 
    filter(str_detect(.$dir, ".tif$")) %>% # take all the .tif files
    bind_cols(extract_code(.$dir))
}


extract_DOY = function(x,y, ID_var_poly =""){
  # x : raster
  # y : spatialpolygon or spatialpoint
  # ID_var: character, the name of the id variable
  # in the polygon dataset ( other than "ID")
  pixels = raster::extract(x, y, weights = TRUE, normalizeWeights=TRUE, progress='text')
  if("SpatialPoints" %in% class(y)){
    weighted_pixels = as_tibble(pixels)%>%
      gather("name", "DOY", factor_key = TRUE) %>%
      drop_na() %>%
      mutate(DOY = round(DOY), Area = 1, weight = 1)
  }else{
  for(i in 1:length(pixels)){
      dat = as_tibble(pixels[[i]]) %>%
        gather("name", "DOY", -weight, factor_key = TRUE)%>%
        drop_na() %>%
        mutate(DOY = round(DOY)) %>%
        group_by(name, DOY) %>%
        summarise(weight = sum(weight)) %>%
        ungroup() %>%
        ## assign the corresponding ID 
        mutate(Area = pull(s@data, ID_var_poly)[[i]])
    if(i==1){
      weighted_pixels = dat
    }else{
      weighted_pixels = rbind(weighted_pixels, dat)
    }
  }}
  
  return(weighted_pixels %>%
           bind_cols(extract_code(.$name)) %>% 
           mutate(Date = as.Date(paste(Year,01,01,sep="-")) + days(DOY-1)))
}

####
cumsum_Pheno = function(weighted_pixels, digit = 2){
  
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
  
  
  sum_weight = left_join(total_period, weighted_pixels, by = c("Area", "Crop", "Date", "P")) %>% # if 2 phen have the same DOY ?
    fill(P) %>% mutate(weight = replace_na(weight, 0)) %>%
    mutate(Year = year(Date)) %>%
    group_by(Area, Crop, P_order) %>%
    mutate(sum_weight = round(cumsum(weight), digit = digit)) %>%
    filter(sum_weight > 0) %>%
    ungroup()
  
  return(sum_weight)
}
# save
#sum_weight %>% select(Area, Crop, P, P_order, Date, sum_weight) %>% saveRDS("DOY.rds")
