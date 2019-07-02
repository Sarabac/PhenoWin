
W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
#W.DIR = "/home/luxis/Dropbox/Kuhn/phenology/PhenoWin"
setwd(W.DIR)


library(tidyverse)
library(lubridate)

source("variables_pheno.R")
source("functions_Pheno.R")

tif_info = extract_tif_info(RU.DIR)

for(crop in unique(tif_info$Crop)){
  ph.ct = raster::stack(tif_info %>% filter(Crop==crop) %>% pull(dir))
  ##import testsite
  #s <- raster::shapefile(file.path(RU.DIR,RU))
  s = raster::shapefile(SHP_FILE)
  #reprojection of shapefile
  s = sp::spTransform(s, raster::crs(ph.ct))
  raster::plot(s)
  # extract each pixel and it proportion in the spatial polygon
  
  weighted_pixels = extract_DOY(ph.ct, s, ID_var_poly = "ID_1")
  
  saveRDS(weighted_pixels, "weights.rds")
  weighted_pixels = readRDS("weights.rds")
  ####
  
  
  sum_weight = cumsum_Pheno(weighted_pixels, digit = 1)
  
  
  # save
  sum_weight %>% select(Area, Crop, P, P_order, Date, sum_weight) %>%
    saveRDS(paste("_Data/DOY_",crop,".rds"))
}
