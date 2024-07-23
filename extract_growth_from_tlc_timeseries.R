library(raster)
library(terra)
library(sf)
library(exactextractr)


# site shape
#country_municipalities = st_as_sf(getData('GADM', country='MEX', level=2))
#site = st_as_sf('') 

#AP1 <- st_read('/Users/stephaniegeorge/Documents/ctrees/Projects/Mexico/Cool_Effect/Project_Areas/activity_area/AA_METALTEPEC_WGS84.shp')
#crs(AP1)

## Area Polygon (Activity area)
AP <- st_read('/Users/stephaniegeorge/Downloads/SHAPE AREA DE ACTIVIDAD FINAL Febrero_2023/AREA_DE_ACTIVIDAD_FINAL_febrero_2023.shp') %>% st_transform(CRS("+proj=longlat +datum=WGS84 +units=m")) %>% st_zm()


site = AP %>% st_as_sf()

#plot(site)

#raster_path = '/Users/stephaniegeorge/Downloads/total_live_carbon/'
#raster_path = '/Users/stephaniegeorge/Downloads/test/'
raster_path = '/Users/stephaniegeorge/Downloads/removals/'
# list all tiff files in your directory 
list_files <- list.files(raster_path,pattern = "*.tif", full.names = T)  

# then apply the same operations for all of the target files
# first create a list for whatever you want to loop over
mean_dat <- list()
sum_dat <- list()
sum_all <- list()
cell_sums <-list()
# now do the same things for all items in the list
for (i in 1:length(list_files)){
  tmp_diff = rast(list_files[i])
  #site = st_transform(site, CRS(projection(tmp_diff)))
  tif_crop = tmp_diff %>% crop(site)  %>% mask(site)
  plot(tif_crop)
  #plot(site, add=T)
  #cellStats(raster(tif_crop), 'mean')
  
  print(cellStats(raster(tif_crop), 'sum'))
  mean_dat[[i]] = exact_extract(tif_crop, site, 'mean')
  #sum_dat[[i]] = exact_extract(tif_crop, site, 'sum')
  #sum_all[[i]] = sum(sum_dat[[i]])
  cell_sums[[i]] = cellStats(raster(tif_crop), 'sum')
  mean_all[[i]] = sum(mean_dat[[i]])
  print(sum(sum_dat[[i]]))
  
  #t <- rast(tmp_diff)
  #s <- vect(seg)
  
}

years = 2015:2020

#plot(years,cell_sums, type='l', ylab = 'TLC MgCO2', main ='Tarahumar')
#plot(years,sum_all, type='l', ylab = 'TLC MgCO2', main ='Tarahumar')
plot(years,sum_all, type='l', ylab = 'Removals MgCO2', main ='Tarahumar')



# Removals
# units are tCO2e per pixel!
# all the data are stored in the list, you can access them using the index 
# year 1 removals (mean) t1 - t2
delta_c = sum(mean_dat[[2]]/10) - sum(mean_dat[[1]]/10)
delta_c2 = sum(mean_dat[[3]]/10) - sum(mean_dat[[2]]/10)
# growth per area
area_sum = sum(st_area(AP))/10000

#results in CO2e
delta_c/area_sum
delta_c2/area_sum
#results in C
(delta_c/area_sum)/(44/12)
(delta_c2/area_sum)/(44/12)

# or save out your target results like:
mean_tlcd <- mean_dat[[1]]

table_test = cbind(site,mean_r)

class(table_test)

st_write(table_test, 'test.csv')
