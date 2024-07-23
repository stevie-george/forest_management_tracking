library(raster)
library(unixtools)
## INFYS 2ndo Ciclo
#setwd('/home/stevie/CONAFOR/Reporte_2_ciclo_INFyS/Resultados_2_ciclo/MAPAS/')


## INFYS 3er Ciclo
#setwd('E:/CONAFOR_linux_backup/CONAFOR/Reporte_3er_ciclo_INFyS/Resultados_3_ciclo/SerieVII/seleccion_mapas_quantiles/')
years = 2000:2009

## mosaic files inside a directory

#setwd('/media/stevie/Stevie/Data/AGB_maps/Harris_tiles/')
setwd('/Volumes/STVHD/LUCC_Mex_v6_30thresh/all_years/')
set.tempdir('/Volumes/STVHD/temp/')
#setwd('/Users/stephaniegeorge/Documents/ctrees/Projects/Mexico/Cool_Effect/Landsat_AD/Mexico/CTrees_LUCC_EXP_Mex_v6_bench_2021_/2022')

## for 1 

# ListRasters <- function(list_names) {
#   raster_list <- list() # initialise the list of rasters
#   for (i in 1:(length(list_names))){ 
#     grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
#     raster_file <- stack(grd_name, bands=1)
#   }
#   raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
# }
# 
# wgs84.tif.list <- list.files(path=getwd(), pattern=glob2rx("*.tif"), full.names=T,recursive=F)
# 
# list_names <- NULL
# for (i in 1:length(wgs84.tif.list)) {
#   list_names <- c(list_names, wgs84.tif.list[i])
# }
# 
# raster.list <-sapply(list_names, FUN = ListRasters)
# 
# names(raster.list) <- NULL
# 
# raster.list$fun <- mean
# mos <- do.call(mosaic, raster.list)
# 
# 
# writeRaster(mos, file='forest_cover_Mex_2013.tif')
# #cellStats(mos, mean)


## mosaic for each year in one folder with all years

for (j in 1:(length(years))){ 
  ListRasters <- function(list_names) {
    raster_list <- list() # initialise the list of rasters
    for (i in 1:(length(list_names))){ 
      grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
      raster_file <- stack(grd_name, bands=1)
    }
    raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
  }
  
  wgs84.tif.list <- list.files(path=getwd(), pattern=glob2rx(paste0("*",years[j],"*")), full.names=T,recursive=F)
  
  list_names <- NULL
  for (i in 1:length(wgs84.tif.list)) {
    list_names <- c(list_names, wgs84.tif.list[i])
  }
  
  raster.list <-sapply(list_names, FUN = ListRasters)
  
  names(raster.list) <- NULL
  
  raster.list$fun <- mean
  mos <- do.call(mosaic, raster.list)
  
  
  writeRaster(mos, file=paste0('LUCC_Mex_',years[j],'.tif'))
  rm(mos)
  gc()
  

}
