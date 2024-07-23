# Obtaining statistics by stratum

#rm(list=ls())

#load libraries
library(rgdal)
library(raster)
library(sf)
library(dplyr)
library(raster)
library(rgdal)

#setwd('/home/stevie/Documents/CTrees/Mexico/')
#setwd('/Users/stephaniegeorge/Documents/ctrees/NFI_map_comparison/')

setwd('/Users/stephaniegeorge/Documents/ctrees/Gedi_extracts/')

# read database
#plot_data <- st_read('/home/stevie/Plot_AGB_INFyS_2015_2020_pixel.shp', crs = CRS("+proj=longlat +datum=WGS84 +no_defs +units=m"))

#plot_data <- st_read('NFI_plots_maps_extract_states.shp', crs = CRS("+proj=longlat +datum=WGS84 +no_defs +units=m"))
plot_data <- st_read('NFI_C3_ha_Gedi_metrics_25m.shp', crs = CRS("+proj=longlat +datum=WGS84 +no_defs +units=m"))

names(plot_data)

strata <- strata[8:32]
plot_data <- st_as_sf(plot_data)
#plot(plot_data[c(1,10,11,12)], pch = 20)
# 
# Read shapefile with state boundaries
#sh <-shapefile('./usv250s6_LCC_Att.shp')
sh <-st_read('../dest20gw/dest20gw.shp')
#plot(sh)
sh2 <- st_as_sf(sh)
# regrid to match CRS
sh <- st_transform(sh, crs(plot_data))
# 
levels(as.factor(sh$NOM_ENT))

## Save names in a list
strata <- c(levels(as.factor(sh$NOM_ENT))) 

#strata1 <- strata[8:32]
#strata2 <- strata[28:32]

# ## Join points by state info
# points_join <- st_join(plot_data, sh2)
# st_write(points_join, "points_join.shp")

#plot_data$AGB_2020_pixel <- plot_data$X2020_1/10
#plot_data$AGB_2015_pixel <- plot_data$X2015_1/10

plot_data <- na.omit(plot_data)

MeanAGB <- list()
sdAGB <- list()
SampleSize <- list()
VarianceAGB <- list()

MeanAGB_2015 <- list()
sdAGB_2015 <- list()
SampleSize_2015 <- list()
VarianceAGB_2015 <- list()

MeanAGB_2020 <- list()
sdAGB_2020 <- list()
SampleSize_2020 <- list()
VarianceAGB_2020 <- list()


MeanAGB_GEDI <- list()
sdAGB_GEDI <- list()
SampleSize_GEDI <- list()
VarianceAGB_GEDI <- list()


par(mar = c(4, 4, 1, 1), mfrow=c(6,6))

for (i in strata2) {
  pol <- filter(sh, NOM_ENT == i)
  plot_data_2  <- st_filter(plot_data,st_make_valid(pol))
  MeanAGB[[i]] <- mean(plot_data_2$AGB_Mgha)
  SampleSize[[i]] <- nrow(plot_data_2)
  sdAGB[[i]] <- sd(plot_data_2$AGB_Mgha)
  VarianceAGB[[i]] <- var(plot_data_2$AGB_Mgha)
  #MeanAGB_2015[[i]] <- mean(plot_data_2$AGB_2015_pixel)
  #SampleSize_2015[[i]] <- nrow(plot_data_2)
  #sdAGB_2015[[i]] <- sd(plot_data_2$AGB_2015_pixel)
  #VarianceAGB_2015[[i]] <- var(plot_data_2$AGB_2015_pixel)
  #MeanAGB_2020[[i]] <- mean(plot_data_2$AGB_2020_pixel)
  #SampleSize_2020[[i]] <- nrow(plot_data_2)
  #sdAGB_2020[[i]] <- sd(plot_data_2$AGB_2020_pixel)
  #VarianceAGB_2020[[i]] <- var(plot_data_2$AGB_2020_pixel)
  MeanAGB_GEDI[[i]] <- mean(plot_data_2$GEDI_AGB_3)
  SampleSize_GEDI[[i]] <- nrow(plot_data_2$GEDI_AGB_3)
  sdAGB_GEDI[[i]] <- sd(plot_data_2$GEDI_AGB_3)
  VarianceAGB_GEDI[[i]] <- var(plot_data_2$GEDI_AGB_3)
  lm1 <- lm(plot_data_2$AGB_Mgha ~ plot_data_2$GEDI_AGB_3)
  plot(plot_data_2$GEDI_AGB_3,plot_data_2$AGB_Mgha, main =i, pch=20, col= 'gray', xlab = 'GEDI AGB', ylab = 'Field AGB')
  abline(lm1, lwd = 2, lty=2 ,col = 'purple')
  abline(0,1, lwd = 2)
  print(i)
}

# Left join produces same result
AGB_by_state_tbl <- plot_data %>% group_by(NOM_ENT) %>%
  summarise(mean_agb=mean(AGB_Mgha),
            sd_agb=sd(AGB_Mgha),
            var_agb=var(AGB_Mgha),
            #Mean_2015 = mean(AGB_2015_pixel),
            #sd_2015 = sd(AGB_2015_pixel),
            #var_2015 = var(AGB_2015_pixel),
            #Mean_2020 = mean(AGB_2020_pixel),
            #sd_2020 = sd(AGB_2020_pixel),
            #var_2020 = var(AGB_2020_pixel),
            Mean_GEDI = mean(AGB_2020_pixel),
            sd_GEDI = sd(AGB_2020_pixel),
            var_GEDI = var(AGB_2020_pixel)) %>% st_drop_geometry()

head(plot_data)
write.csv(AGB_by_state_tbl, "AGB_State_tbl_point_to_pixel_GEDI.csv")

C3 <- tibble(strata, MeanAGB, sdAGB, SampleSize, VarianceAGB, MeanAGB_2015, sdAGB_2015, SampleSize_2015, VarianceAGB_2015, MeanAGB_2020, sdAGB_2020, SampleSize_2020, VarianceAGB_2020)
C3
df <- apply(C3,2,as.character)
write.table(df, "AGB_statistics_by_state_point_to_pixel_03-27-23.csv")
#shapefile(points_join, "plot_data_jurisdiction.shp")

## Plot
library(ggplot2)

ggplot(plot_data,aes(x=AGB_Mgha,y=NOM_ENT, na.rm=T))+geom_boxplot() #+
ggplot(plot_data,aes(x=AGB_2015_pixel/10,y=NOM_ENT, na.rm=T))+geom_boxplot() #+
ggplot(plot_data,aes(x=AGB_2020_pixel/10,y=NOM_ENT, na.rm=T))+geom_boxplot() #+

AGB_by_state_tbl <- AGB_by_state_tbl[2:33,]

par(mar=c(6,3,2,1),mfrow=c(1,1))
plot(AGB_by_state_tbl$mean_agb, pch = 20, ylim = c(0,170), xaxt='n', ylab = "Mean AGB Mg ha-1", xlab= '', main = "NFI Points to map pixels")
lines(AGB_by_state_tbl$mean_agb)
points(AGB_by_state_tbl$Mean_2015, pch = 20, col ='blue')
lines(AGB_by_state_tbl$Mean_2015,col ='blue')
points(AGB_by_state_tbl$Mean_2020,col ='red', pch= 20)
lines(AGB_by_state_tbl$Mean_2020,col ='red')
axis(1, at=1:32, labels= strata, las  = 3)
legend('topleft', c('Field Plots','2015 AGB map', '2020 AGB map'), col = c('black','blue', 'red'), pch =20, bg ='transparent')



#theme(axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1))
#boxplot(AGB_Mgha~NOM_ENT,data=points_join)





