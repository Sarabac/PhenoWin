setwd("/home/luxis/Dropbox/Kuhn/phenology/PhenoWin")

library(sf)
library(raster)
library(velox)
source("Functions_Pheno.R")
phenoProj = st_crs(31467)
phenoCall = st_crs(4326)

xminP = 3279500
yminP = 5237500
cellLenth = 1000
nrows=866
ncols= 654

germany = st_read(GERMANY) %>% dplyr::select(field = 1)

Rcoord = ((1:nrows)*cellLenth - cellLenth/2) + yminP
Ccoord = ((1:ncols)*cellLenth - cellLenth/2) + xminP
points = expand.grid(Rcoord, Ccoord) %>% 
  st_as_sf(coords = c(2,1))
st_crs(points) <- phenoProj

Tpoint = st_transform(points, phenoCall)

Pcoord = st_join(Tpoint,germany) %>%  drop_na() %>%  dplyr::select() 




##### TRASH ####

plot(Tpoint)
plot(germany, add=TRUE, col="red")
f = st_intersection(Tpoint, germany)

j = st_join(Tpoint,germany)

crossprod(Rcoord, Ccoord)

Mext = new("Extent"
    , xmin = 3279500
    , xmax = 3922500
    , ymin = 5237500
    , ymax = 6103500
)
Mncells = 556838

Mraster = raster(Mext, nrows=866, ncols= 654,
       crs=CRS(phenoCall[["proj4string"]])) %>% setValues(1)
germany = st_read(GERMANY) %>% dplyr::select(field = 1)
Vraster = velox(Mraster)
Vraster$rasterize(germany, field = "field")

Mpoint = rasterToPoints(Mraster, spatial=TRUE) %>% 
  st_as_sf()%>% dplyr::select()

MpCrop = st_join(Mpoint, germany, join=st_contains)
j = drop_na(MpCrop)
 plot(MpCrop)

PHASE.FILES = list.files("/home/luxis/Dropbox/Kuhn/phenology/PhenoWin1/_DOY",
                         "\\.tif$", full.names = TRUE)
ra = raster(PHASE.FILES[1])
ex = extent(ra)

extent