# variables
SHP_FILE = file.path("Deu_admin/DEU_adm1.shp")
GERMANY = file.path("Deu_admin/DEU_adm0.shp")
WEATHERGRID = file.path("Weathergrid/WEATHER_GRID_EPSG31467.shp")

RU.DIR = "_DOY/"
PHASE.DIR = "_output/"
OUT.DIR <- "_output/"
RU = "VG250_KRS_UCKERMARK_epsg25832"
DATA_FOLDER = "_DATA"
DATA_FILE = function(n){file.path(DATA_FOLDER, paste("DOY_",n,".rds", sep=""))}


CT.P <- c(10,12,15,18,19,21,24)
col.p <- c("#3288BD", #10
           "#e5f4e3", #12
           "#ABDDA4", #15
           "#66C2A5", #18
           "#feecb9", #19
           "#FDAE61", #21
           "#F46D43", #24
           "#D53E4F") #AH
color_fill_rule = setNames(col.p, CT.P)
color_fill_custom = scale_fill_manual(values = color_fill_rule)

CROPS_CORRESPONDANCE = list(
  "201" = 201,
  "202" = 202,
  "204" = 204,
  "205" = 205,
  "208" = 208,
  "215" = 215
)
