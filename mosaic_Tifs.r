library(raster)

## INFYS 2ndo Ciclo
#setwd('/home/stevie/CONAFOR/Reporte_2_ciclo_INFyS/Resultados_2_ciclo/MAPAS/')


## INFYS 3er Ciclo
#setwd('E:/CONAFOR_linux_backup/CONAFOR/Reporte_3er_ciclo_INFyS/Resultados_3_ciclo/SerieVII/seleccion_mapas_quantiles/')


## mosaicos de mapas para comparacion

#setwd('/media/stevie/Stevie/Data/AGB_maps/Harris_tiles/')
# setwd('/Users/stephaniegeorge/Documents/ctrees/Data/CCI/AGB_2020/')

setwd('/Volumes/STVHD/ALOS_HH_2019_2021/')

ListRasters <- function(list_names) {
  raster_list <- list() # initialise the list of rasters
  for (i in 1:(length(list_names))){ 
    grd_name <- list_names[i] # list_names contains all the names of the images in .grd format
    raster_file <- stack(grd_name, bands=1)
  }
  raster_list <- append(raster_list, raster_file) # update raster_list at each iteration
}

wgs84.tif.list <- list.files(path=getwd(), pattern=glob2rx("*.tif"), full.names=T,recursive=F)

list_names <- NULL
for (i in 1:length(wgs84.tif.list)) {
  list_names <- c(list_names, wgs84.tif.list[i])
}

raster.list <-sapply(list_names, FUN = ListRasters)



names(raster.list) <- NULL

raster.list$fun <- mean
mos <- do.call(mosaic, raster.list)

#plot(mos)

## 
# mos1 <- mos
#
# mos1[mos1 > 200] <- 200
# mos1[mos1 < 0] <- 0
# 
# plot(mos1)

writeRaster(mos, file='ALOS_HH_2019_2021_25m.tif')
#cellStats(mos, mean)



