library(raster)
library(sf)
library(rgdal)


setwd('/Users/stephaniegeorge/Documents/ctrees/Projects/Mexico/Project_Areas/activity_area/')

# Load tif files

# Carbon
AGB_2015 = raster('/Users/stephaniegeorge/Documents/ctrees/Data/Maps/Mexico_biomass_100m_2015.tif')
proj4string(AGB_2015) <- CRS("+proj=longlat +datum=WGS84 +units=m")

AGB_2020 = raster('/Users/stephaniegeorge/Documents/ctrees/Data/Maps/Mexico_biomass_100m_2020.tif')
proj4string(AGB_2020) <- CRS("+proj=longlat +datum=WGS84 +units=m")

# Cover loss

#AGB_2015_extract <- extract(AGB_2015,sp_dat, buffer=56.418, weights=TRUE, fun=mean,cellnumbers=TRUE)

cover_loss = raster('../../Cover_Loss_MX_100m_2000-2022/cover_loss.tif')
proj4string(cover_loss) <- CRS("+proj=longlat +datum=WGS84 +units=m")


# load shapefile

# pol <- st_read('./Mexico IFM/Capulalpam ANEXO 1. KLM Y MAPAS/SHAPE FILE PMF/PMF_CAPU.shp')
# pol_c <- st_transform(pol, CRS(projection(AGB_2015)))
# tif_crop <- AGB_2015%>% crop(pol_c)
# plot(tif_crop)


shp.list <- list.files(getwd(), pattern=glob2rx("*.shp"), full.names=T,recursive=F)

list_names <- NULL
for (i in 1:length(shp.list)) {
  list_names <- c(list_names, shp.list[i])
}

renamed = list.files(getwd(), pattern=glob2rx("*.shp"),recursive=F)
renamed2 = tools::file_path_sans_ext(basename(renamed))

#### Main function


for (i in 1:length(renamed)) {
  pol <- st_read(renamed[i]) %>% st_zm()
  #plot_data_2  <- st_filter(plot_data,st_make_valid(pol))
  pol_c <- st_transform(pol, CRS("+proj=longlat +datum=WGS84 +units=m"))
  tif_crop <- AGB_2015%>% crop(pol_c)
  pol_c$AGB_2015_mean <- extract(tif_crop,pol_c, fun=mean,cellnumbers=TRUE)
  tif_crop2 <- AGB_2020%>% crop(pol_c)
  pol_c$AGB_2020_mean <- extract(tif_crop2,pol_c, fun=mean,cellnumbers=TRUE)
  tif_crop3 <- cover_loss%>% crop(pol_c)
  pol_c$CoverLoss <- extract(tif_crop3,pol_c, fun=mean,cellnumbers=TRUE)
  pol_c$Losscount <- sum(pol_c$CoverLoss != 0)   
  st_write(pol_c,paste(renamed2[i],"_Ctrees.shp"))
  df <- st_drop_geometry(pol_c)
  write.csv(df,paste(renamed2[i],"_Ctrees.csv"))
  print(i)
}
