library(raster)
library(sf)
library(rgdal)
library(dplyr)
library(terra)
library(rgeos)
library(RColorBrewer)
library(spdep)
library(exactextractr)

# 
#setwd('/Users/stephaniegeorge/Documents/ctrees/Projects/Mexico/Project_Areas/activity_area/')

# define path to directory with project shapefiles

dir_shapefiles = '/home/rstudio/project_kml/'

# load shapefiles
# list files
shp.list <- list.files(dir_shapefiles, pattern=glob2rx("*.kml"), full.names=T,recursive=F)

list_names <- NULL
for (i in 1:length(shp.list)) {
  list_names <- c(list_names, shp.list[i])
}

# get names
renamed = list.files(dir_shapefiles, pattern=glob2rx("*.kml"),recursive=F)
renamed2 = tools::file_path_sans_ext(basename(renamed))


# define path to directory with raster files for extraction
# raster_path = '/Users/stephaniegeorge/Downloads/total_live_carbon/'
raster_path = '/home/rstudio/home/rstudio/ctrees_output_yan/'
#raster_path = '/Users/stephaniegeorge/Downloads/removals/'
# list all tiff files in your directory 
list_files <- list.files(raster_path,pattern = "*.tif", full.names = T)  


## create lists with target datasets to loop over
list_def_emissions = list_files[grep("_deforestation_emissions", list_files)]
list_deg_emissions = list_files[grep("_degradation_emissions", list_files)]
list_fire_emissions = list_files[grep("_forest_fire_emissions", list_files)]
list_removals = list_files[grep("_forest_removals", list_files)]


#### Main function
## extracts emissions from deforestation, degradation and fire for all projects
for (i in 1:length(renamed)) {
  AP <- st_read(paste0(dir_shapefiles,renamed[i])) %>% st_transform(CRS("+proj=longlat +datum=WGS84 +units=m")) %>% st_zm() 
  
  site = AP %>% st_as_sf()
  
  #plot(site)
  
  
  
  # create empty lists to store files
  #mean_dat <- list()
  sum_dat_def <- list()
  sum_all_def <- list()
  #cell_sums <-list()
  
  ### loop over data and save into list
  ## process deforestation emissions
  for (q in 1:length(list_def_emissions)){
    tmp_def_diff = rast(list_def_emissions[q])
    tif_crop_def = tmp_def_diff %>% crop(site)  %>% mask(site)
    sum_dat_def[[q]] = exact_extract(tif_crop_def, site, 'sum')
    sum_all_def[[q]] = sum(sum_dat_def[[q]])
    print(sum(sum_dat_def[[q]]))
  }
  
  ## process degradation
  sum_dat_deg <- list()
  sum_all_deg <- list()
  for (j in 1:length(list_deg_emissions)){
    tmp_deg_diff = rast(list_deg_emissions[j])
    tif_crop_deg = tmp_deg_diff %>% crop(site)  %>% mask(site)
    sum_dat_deg[[j]] = exact_extract(tif_crop_deg, site, 'sum')
    sum_all_deg[[j]] = sum(sum_dat_deg[[j]])
    print(sum(sum_dat_deg[[j]]))
  }
  
  
  
  
  ## process fire
  sum_all_fire <- list()
  sum_dat_fire <- list()
  
  for (k in 1:length(list_fire_emissions)){
    tmp_fire_diff = rast(list_fire_emissions[k])
    #site = st_transform(site, CRS(projection(tmp_diff)))
    tif_crop_fire = tmp_fire_diff %>% crop(site)  %>% mask(site)
    
    #plot(tif_crop_fire)
    #plot(site, add=T)
    #cellStats(raster(tif_crop), 'mean')
    
    #print(cellStats(raster(tif_crop), 'sum'))
    #mean_dat[[i]] = exact_extract(tif_crop, site, 'mean')
    
    sum_dat_fire[[k]] = exact_extract(tif_crop_fire, site, 'sum')
    sum_all_fire[[k]] = sum(sum_dat_fire[[k]])
    
    #cell_sums[[i]] = cellStats(raster(tif_crop), 'sum')
    print(sum(sum_dat_fire[[k]]))
    
    #t <- rast(tmp_diff)
    #s <- vect(seg)
  }
  
  # ## process Total Live Carbon
  # sum_dat_tlc <- list()
  # sum_all_tlc <- list()
  # for (v in 1:length(list_tlc_emissions)){
  #   tmp_tlc_diff = rast(list_tlc_emissions[v])
  #   tif_crop_tlc = tmp_tlc_diff %>% crop(site)  %>% mask(site)
  #   sum_dat_tlc[[v]] = exact_extract(tif_crop_tlc, site, 'sum')
  #   sum_all_tlc[[v]] = sum(sum_dat_tlc[[v]])
  #   print(sum(sum_dat_tlc[[v]]))
  # }
  
  ## process Removals
  sum_dat_rem <- list()
  sum_all_rem <- list()
  for (w in 1:length(list_removals)){
    tmp_rem_diff = rast(list_removals[w])
    tif_crop_rem = tmp_rem_diff %>% crop(site)  %>% mask(site)
    sum_dat_rem[[w]] = exact_extract(tif_crop_rem, site, 'sum')
    sum_all_rem[[w]] = sum(sum_dat_rem[[w]])
    print(sum(sum_dat_rem[[w]]))
  }
  
  
  # ## compare to means
  # 
  # # create empty lists to store files
  # mean_dat_def <- list()
  # sum_means_def <- list()
  # #sum_dat_def <- list()
  # #sum_all_def <- list()
  # #cell_sums <-list()
  # 
  # ### loop over data and save into list
  # ## process deforestation emissions
  # for (i in 1:length(list_def_emissions)){
  #   tmp_def_diff = rast(list_def_emissions[i])
  #   tif_crop_def = tmp_def_diff %>% crop(site)  %>% mask(site)
  #   plot(tif_crop_def)
  #   mean_dat_def[[i]] = exact_extract(tif_crop_def, site, 'mean')
  #   mean_dat_def[[i]] = mean_dat_def[[i]][!is.na(mean_dat_def[[i]])]
  #   sum_means_def[[i]] = sum(mean_dat_def[[i]])
  #   print(sum(sum_means_def[[i]]))
  # }
  
  
  
  years = 2001:2022
  
  #length(sum_all_deg)
  #sum_all_def
  
  
  sum_all_deg_scaled = list()
  
  for (l in 1:length(sum_all_deg)){
    sum_all_deg_scaled[[l]] = sum_all_deg[[l]]/10
  }
  
  sum_all_def_scaled = list()
  for (m in 1:length(sum_all_def)){
    sum_all_def_scaled[[m]] = sum_all_def[[m]]/10
  }
  
  sum_all_fire_scaled = list()
  for (n in 1:length(sum_all_fire)){
    sum_all_fire_scaled[[n]] = sum_all_fire[[n]]/10
  }
  
  
  sum_all_rem_scaled = list()
  for (p in 1:length(sum_all_rem)){
    sum_all_rem_scaled[[p]] = sum_all_rem[[p]]/10
  }
  
  
  #plot(years,cell_sums, type='l', ylab = 'TLC MgCO2', main ='Tarahumar')
  #plot(years,sum_all, type='l', ylab = 'TLC MgCO2', main ='Tarahumar')
  
  plot(years,sum_all_def_scaled, type='l', ylab = expression('Emissions CO'[2]*e*' ha'^-1), main =renamed2[i], ylim = c(0,max(unlist(sum_all_def_scaled))))
  lines(years,sum_all_deg_scaled, col ='orange')
  lines(years,sum_all_fire_scaled, col ='red')
  #lines(years,sum_all_tlc, col ='darkgreen')
  #lines(years,sum_all_rem_scaled, col ='lightgreen')
  legend(bg="transparent",'topleft', c('deforestation emissions','degradation emissions','fire emissions'), lty=1,col = c('black', 'orange','red'))
  
  
  plot(years,sum_all_rem_scaled, col ='lightgreen', type='l', ylab = expression('Removals CO'[2]*e*' ha'^-1), main =renamed2[i], ylim = c(min(unlist(sum_all_rem_scaled)),max(unlist(sum_all_rem_scaled))))
  abline(0,0, lty=2)
  # lines(years,sum_all_deg_scaled, col ='orange')
  # lines(years,sum_all_fire_scaled, col ='red')
  # #lines(years,sum_all_tlc, col ='darkgreen')
  # lines(years,sum_all_rem_scaled, col ='lightgreen')
  legend(bg="transparent",'topright', c('Project removals'), lty=1,col = c('lightgreen'))
  
  
  
  
  
  
  # plot(years[18:22],sum_all_def_scaled[18:22], type='l', ylab = 'Emissions', main =renamed[i], ylim = c(0,1300))
  # lines(years,sum_all_deg_scaled, col ='orange')
  # lines(years,sum_all_fire_scaled, col ='red')
  # legend('topleft', c('deforestation emissions','degradation emissions','fire emissions'), lty=1,col = c('black', 'orange','red'))
  
  df_all = cbind(years, sum_all_def_scaled, sum_all_deg_scaled, sum_all_fire_scaled,sum_all_rem)
  colnames(df_all) = c('years', 'deforestation_emissions', 'degradation_emissions','fire_emissions','Removals' )
  
  head(df_all)
  
  write.csv(df_all, paste0(renamed[i],'_emissions_removals_1km.csv'))
  
  
  print(i)
}





# plots.dir.path  <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
# 
# 
# 
# plots.png.detials <- file.info(plots.png.paths)
# plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
# sorted.png.names <- gsub(plots.dir.path, getwd(), row.names(plots.png.detials), fixed=TRUE)
# numbered.png.names <- paste0(getwd(), 1:length(sorted.png.names), ".png")
# 
# # Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
# file.rename(from=sorted.png.names, to=numbered.png.names)
# 
# file.copy(from=plots.png.paths, to=getwd())





