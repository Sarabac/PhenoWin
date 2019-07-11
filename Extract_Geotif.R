#W.DIR <- dirname(rstudioapi::getActiveDocumentContext()$path)
#W.DIR = "/home/luxis/Dropbox/Kuhn/phenology/PhenoWin"
W.DIR = "L:\\Lucas\\phenology\\PhenoWin"
setwd(W.DIR)


library(tidyverse)
library(lubridate)

source("Functions_Pheno.R")

tif_info = extract_tif_info(RU.DIR)
crop_list = unique(tif_info$Crop)
i = 0
for (crop in crop_list){
  print(paste(i,length(crop_list), sep="/"))
  sel = tif_info %>% filter(Crop==crop)
  a = velox::velox(raster::stack(sel$dir, quick = TRUE))
  print("Save ...")
  saveRDS(list(sel, a), DATA_FILE(crop))
  i = i + 1
}
