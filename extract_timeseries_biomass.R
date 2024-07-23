library(terra)
library(sf)
library(exactextractr)

# site shape
#country_municipalities = st_as_sf(getData('GADM', country='MEX', level=2))
#site = st_as_sf('') 

AP <- st_read('/Users/stephaniegeorge/Documents/ctrees/Projects/Mexico/Cool_Effect/Project_Areas/activity_area/AA_METALTEPEC_WGS84.shp') %>% st_transform(CRS("+proj=longlat +datum=WGS84 +units=m")) %>% st_zm() 


site = AP %>% st_as_sf()

plot(site)

raster_path = '/Users/stephaniegeorge/Downloads/removals'

# list all tiff files in your directory 
list_files <- list.files(raster_path ,pattern = "*.tiff", full.names = T)  

# then apply the same operations for all of the target files
# first create a list for whatever you want to loop over
MeanAGB <- list()

# now do the same things for all items in the list
for (i in 1:length(list_files)){
  tmp_diff = rast(list_files[i])
  site = st_transform(site, CRS(projection(tmp_diff)))
  tif_crop = tmp_diff %>% crop(site) # %>% resample()
  plot(tif_crop)
  MeanAGB[[i]] = exact_extract(tif_crop, site, 'mean')
  #t <- rast(tmp_diff)
  #s <- vect(seg)
  
}

# then all the data are stored in the list, you can access them using the index like:
MeanAGB[[1]]

# or save out your target results like:
zuk_1 <- zuk[[1]]
