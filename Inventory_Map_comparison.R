library(sf)
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
library(viridis)
library(ggpointdensity)
library(exactextractr)

library(ggpmisc)
library(ggside)

library(reshape)

setwd('/Users/stephaniegeorge/Documents/ctrees/Data/')

## Preprocess field plot data

plot_data <-  read.csv("./Field/INFyS_3er_ciclo/Daso_A_Cgl_C3_V7_24052022.csv",  
                       fileEncoding = "Latin1", check.names = F)

plot_data$Coord_X <- plot_data$X_C3
plot_data$Coord_Y <- plot_data$Y_C3


dat <- plot_data[is.na(plot_data$X_C3)==FALSE,]

sp_dat <- dat |> st_as_sf(coords=c("X_C3","Y_C3"), crs=4326)
head(sp_dat)
plot(sp_dat)
sp_dat <- st_transform(sp_dat, CRS("+proj=longlat +datum=WGS84 +units=m"))

# Make a 1 ha buffer around points for extraction
#sp_dat_1ha <- st_buffer(sp_dat, 56.418)

## import maps

AGB_2015 <- raster('./Maps/Mexico_biomass_100m_2015.tif')
proj4string(AGB_2015) <- CRS("+proj=longlat +datum=WGS84 +units=m")
AGB_2015[AGB_2015<0] = 0
plot(AGB_2015)

AGB_2020 <- raster('./Maps/Mexico_biomass_100m_2020.tif')
proj4string(AGB_2020) <- CRS("+proj=longlat +datum=WGS84 +units=m")

GEDI <- raster('/Users/stephaniegeorge/Documents/ctrees/lidar/Mexico/GEDI/gedi_25m/mean_biomass/GEDI_25m_AGB.tif')

radius <- sqrt(10000/pi)

AGB_2015_extract <- extract(AGB_2015,sp_dat, buffer=radius, fun=mean)
sp_dat$AGB_2015e <- AGB_2015_extract

AGB_2020_extract <- extract(AGB_2020,sp_dat, buffer=radius, fun=mean)
sp_dat$AGB_2020e <- AGB_2020_extract

AGB_GEDI <- extract(GEDI,sp_dat, buffer=radius, fun=mean)
sp_dat$GEDI_25m <- AGB_GEDI

sp_dat$AGB <- sp_dat$'Daso_A_Biomasa_aerea_Ton/HA'
sp_dat$AGB_2015 <- sp_dat$AGB_2015e/10
sp_dat$AGB_2020 <- sp_dat$AGB_2020e/10
head(sp_dat)

states_t <- st_read('./Mexico/State_limits/dest20gw.shp')
states <- st_as_sf(states_t,CRS("+proj=longlat +datum=WGS84 +units=m"))


cover_t <- st_read('./Mexico/Land_use/SerieVII_wgs84/Usv_SVII_MGM2021_wgs84.shp')
cover <- st_as_sf(cover_t,CRS("+proj=longlat +datum=WGS84 +units=m"))


#AGB_15_ex <- extract(AGB_2015,states, fun=mean)

#writeRaster(AGB_2015, 'Mexico_AGB_2015_100m_00.tif')

sp_dat_states <- st_join(sp_dat, states, left=T)
head(sp_dat_states)


#write.csv(sp_dat_states, 'Mex_NFI_Ctrees_AGB_GEDI.csv')

# Filter only positive 
newdata <- na.omit(sp_dat_states)
# newdata <- subset(sp_dat_states, AGB_2015 >= 0)
# newdata <- subset(newdata, AGB_2020 >= 0)
# newdata <- na.omit(newdata)
# 
# 
# plot(newdata$AGB_2015)


## make some plots

lm(AGB~AGB_2015, data = newdata)

ggplot(data = newdata, mapping = aes(x = AGB_2015, y = AGB)) +
  geom_pointdensity() +
  scale_color_viridis()+
  stat_poly_line() +
  geom_abline(intercept = 0, slope = 1, lty=2, lwd=2)+
  stat_poly_eq(use_label(c("R2")))



lm(AGB~AGB_2020, data = newdata)

ggplot(data = newdata, mapping = aes(x = AGB_2020, y = AGB)) +
  geom_pointdensity() +
  scale_color_viridis()+
  stat_poly_line() +
  geom_abline(intercept = 0, slope = 1, lty=2, lwd=2)+
  stat_poly_eq(use_label(c("R2")))


#2015

ggp <- ggplot(newdata, aes(x = AGB_2015, y = AGB)) +           
  geom_pointdensity() +
  scale_color_viridis()+
  facet_wrap( ~ Formacion_S6_C4)
#ggp
ggp +                                     
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  stat_poly_line() +
  geom_abline(intercept = 0, slope = 1, lty=2)+
  stat_poly_eq(use_label(c("R2"))) 

#2020
ggp <- ggplot(newdata, aes(x = AGB_2020, y = AGB)) +           
  geom_pointdensity() +
  scale_color_viridis()+
  facet_wrap( ~ Formacion_S6_C4)
#ggp
ggp +                                     
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  
  geom_abline(intercept = 0, slope = 1, lty=2)+
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")))

# By state
# 2015
ggp <- ggplot(newdata, aes(x = AGB_2015, y = AGB)) +           
  geom_pointdensity() +
  scale_color_viridis()+
  facet_wrap( ~ NOM_ENT)
ggp +                                     
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  geom_abline(intercept = 0, slope = 1, lty=2)+
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")))

# 2020
ggp <- ggplot(newdata, aes(x = AGB_2020, y = AGB)) +           
  geom_pointdensity() +
  scale_color_viridis()+
  facet_wrap( ~ NOM_ENT)
ggp +                                     
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  geom_abline(intercept = 0, slope = 1, lty=2)+
  stat_poly_line() +
  stat_poly_eq(use_label(c("R2")))






#2015
ggplot(data = newdata, aes(x = AGB_2015, y = AGB, colour = Formacion_S6_C4)) +
  stat_poly_line() + geom_abline(intercept = 0, slope = 1)
#+ stat_poly_eq(use_label(c("eq", "R2")))
#2020
ggplot(data = newdata, aes(x = AGB_2020, y = AGB, colour = Formacion_S6_C4)) +
  stat_poly_line() + geom_abline(intercept = 0, slope = 1)# + stat_poly_eq(use_label(c("eq", "R2")))


## Plot
library(ggplot2)

ggplot(newdata,aes(x=AGB,y=NOM_ENT, na.rm=T))+geom_boxplot() #+
ggplot(newdata,aes(x=AGB_2015,y=NOM_ENT, na.rm=T))+geom_boxplot() #+
ggplot(newdata,aes(x=AGB_2020,y=NOM_ENT, na.rm=T))+geom_boxplot() #+


ggplot(newdata,aes(x=AGB,y=Formacion_S6_C4, na.rm=T))+geom_boxplot() #+
ggplot(newdata,aes(x=AGB_2015,y=Formacion_S6_C4, na.rm=T))+geom_boxplot() #+
ggplot(newdata,aes(x=AGB_2020,y=Formacion_S6_C4, na.rm=T))+geom_boxplot() #+


# Left join 
AGB_by_state_tbl <- newdata %>% group_by(NOM_ENT) %>%
  summarise(mean_agb.y=mean(AGB),
            sd_agb=sd(AGB),
            var_agb=var(AGB),
            Mean_2015 = mean(AGB_2015),
            sd_2015 = sd(AGB_2015),
            var_2015 = var(AGB_2015),
            Mean_2020 = mean(AGB_2020),
            sd_2020 = sd(AGB_2020),
            var_2020 = var(AGB_2020),
            Mean_GEDI = mean(GEDI_25m),
            sd_GEDI = sd(GEDI_25m),
            var_GEDI = var(GEDI_25m)
            
            )%>% st_drop_geometry()

AGB_by_state_tbl

#write.csv(AGB_by_state_tbl, "AGB_NFI_C3_vs_2015_2020_GEDI_States_plot_to_pixel.csv")


# Left join 
AGB_by_st <- sp_dat_states %>% group_by(NOM_ENT) %>%
  summarise(mean_agb.y=mean(`Daso_A_Biomasa_aerea_Ton/HA`),
            sd_agb=sd(`Daso_A_Biomasa_aerea_Ton/HA`),
            var_agb=var(`Daso_A_Biomasa_aerea_Ton/HA`)) %>% st_drop_geometry()

AGB_by_st
#write.csv(AGB_by_st,'AGB_by_st_C3.csv')



# Left join 
AGB_by_cover_tbl <- newdata %>% group_by(Formacion_S6_C4) %>%
  summarise(mean_agb.y=mean(AGB),
            sd_agb=sd(AGB),
            var_agb=var(AGB),
            Mean_2015 = mean(AGB_2015),
            sd_2015 = sd(AGB_2015),
            var_2015 = var(AGB_2015),
            Mean_2020 = mean(AGB_2020),
            sd_2020 = sd(AGB_2020),
            var_2020 = var(AGB_2020),
            Mean_GEDI = mean(GEDI_25m),
            sd_GEDI = sd(GEDI_25m),
            var_GEDI = var(GEDI_25m)
            ) %>% st_drop_geometry()

AGB_by_cover_tbl
#write.csv(AGB_by_cover_tbl, "AGB_NFI_C3_vs_2015_2020_GEDI_cover.csv")

boxplot(newdata$AGB, newdata$AGB_2015, newdata$AGB_2020, newdata$GEDI_25m )


# df <- cbind(newdata$IdConglomerado,newdata$AGB,  newdata$AGB_2015, newdata$AGB_2020, newdata$NOM_ENT, newdata$Formacion_S6_C4)
# colnames(df) <- c('IdConglomerado', 'AGB', 'AGB_2015', 'AGB_2020', 'NOM_ENT', 'FORMACION')
# df <- data.frame(df)
# 
# agb1 <- cbind(df$AGB, 'AGB', df$NOM_ENT, df$FORMACION)
# agb2 <- cbind(df$AGB_2015, 'AGB_2015', df$NOM_ENT, df$FORMACION)
# agb3 <- cbind(df$AGB_2020, 'AGB_2020', df$NOM_ENT, df$FORMACION)
# 
# 
# df_box <- data.frame(rbind(agb1, agb2, agb3))
# 
# colnames(df_box) <- c("AGB", "Survey", "State", "Cover")
# head(df_box)
# # Plot boxplot using ggplot function
# 
# # 
# # plot <- ggplot(data.frame(df_box), aes(x=factor(Survey), y=AGB))+
# #   geom_boxplot()+
# #   theme( legend.position = "none" )
# # # print boxplot
# # plot


AGB_by_state_all_pixels = read.csv("AGB_comparisons/AGB_Map_2015_2020_GEDI_all_pixels.csv")
head(AGB_by_state_all_pixels)

AGB_by_state_tbl <- merge(AGB_by_state_tbl, AGB_by_st, by = 'NOM_ENT')

#AGB_plots_to_pixels = read.csv('AGB_comparisons/AGB_NFI_C3_vs_2015_2020_GEDI_States.csv')
#head(AGB_plots_to_pixels)


# ggplot(df_box, aes(x=Cover, y=AGB, fill=Survey)) + 
#   geom_boxplot()
strata <- c(levels(as.factor(states$NOM_ENT)))
par(mar=c(8,5,1,1))
plot(AGB_by_state_tbl$mean_agb.y.y, pch = 20, ylim = c(0,250), xaxt='n', ylab = "Mean AGB Mg ha-1", xlab= '', main = "NFI plots to map pixels")
lines(AGB_by_state_tbl$mean_agb.y.y)
# mid<- barplot(AGB_by_state_tbl$mean_agb.y)
# arrows(x0=mid, y0=AGB_by_state_tbl$mean_agb.y-AGB_by_state_tbl$sd_agb, x1=mid, y1=AGB_by_state_tbl$mean_agb.y+AGB_by_state_tbl$sd_agb, code=3, angle=90, length=0.1)
points(AGB_by_state_tbl$Mean_2015, pch = 20, col ='blue')
lines(AGB_by_state_tbl$Mean_2015,col ='blue')
points(AGB_by_state_tbl$Mean_2020,col ='red', pch= 20)
lines(AGB_by_state_tbl$Mean_2020,col ='red')
points(AGB_by_state_tbl$Mean_GEDI, pch = 20, col ='green')
lines(AGB_by_state_tbl$Mean_GEDI,col ='green')
axis(1, at=1:29, labels= strata[-c(7, 27, 29)], las  = 3, cex.axis = 0.7)
legend('topleft', c('Field Plots','2015 AGB map', '2020 AGB map', 'GEDI AGB'), col = c('black','blue', 'red', 'green'), pch =20, bg ='transparent')

head(AGB_by_state_tbl)
##
par(mar=c(4,4,2,2))
plot(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_2015,col='blue' ,pch = 20,ylim=c(0,100),xlim=c(0,100), xlab = "Mean Field AGB [Mg ha-1]", ylab= 'Mean Estimated AGB [Mg ha-1]', main = "NFI plots to map pixels")
#abline(lm(AGB_by_state_tbl$mean_agb.y ~AGB_by_state_tbl$Mean_2015))
abline(0,1,lty=2)
abline(lm(AGB_by_state_tbl$Mean_2015~AGB_by_state_tbl$mean_agb.y.y), col ='blue')
points(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_2020, pch = 20, col ='red')
abline(lm(AGB_by_state_tbl$Mean_2020~AGB_by_state_tbl$mean_agb.y.y), col ='red')
points(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_GEDI, pch = 20, col ='green')
abline(lm(AGB_by_state_tbl$Mean_GEDI~AGB_by_state_tbl$mean_agb.y.y), col ='green')
text(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_2015, labels=AGB_by_state_tbl$NOM_ENT, cex=0.5, col='blue')
text(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_2020, labels=AGB_by_state_tbl$NOM_ENT, cex=0.5, col='red')
text(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_GEDI, labels=AGB_by_state_tbl$NOM_ENT, cex=0.5, col='green')

legend('topleft', c('2015 AGB map', '2020 AGB map', 'GEDI AGB'), col = c('blue', 'red', 'green'), pch =20, bg ='transparent')



##### All pixels in state

strata <- c(levels(as.factor(states$NOM_ENT)))
par(mar=c(8,5,1,1))
plot(AGB_by_state_all_pixels$AGB_C3, pch = 20, ylim = c(0,150), xaxt='n', ylab = "Mean AGB Mg ha-1", xlab= '', main = "All pixels in state")
lines(AGB_by_state_all_pixels$AGB_C3)

points(AGB_by_state_all_pixels$X2015_mean/10, pch = 20, col ='purple')
lines(AGB_by_state_all_pixels$X2015_mean/10,col ='purple')

points(AGB_by_state_all_pixels$AGB_2020,col ='gray', pch= 20)
lines(AGB_by_state_all_pixels$AGB_2020,col ='gray')

points(AGB_by_state_all_pixels$X_GEDImean, pch = 20, col ='orange')
lines(AGB_by_state_all_pixels$X_GEDImean,col ='orange')
axis(1, at=1:31, labels= strata[-c(7)], las  = 3, cex.axis = 0.7)
legend('topleft', c('Field Plots','2015 AGB map', '2020 AGB map', 'GEDI AGB'), col = c('black','purple', 'gray', 'orange'), pch =20, bg ='transparent')

#####

par(mar=c(4,4,2,2))
plot(AGB_by_state_all_pixels$AGB_C3,AGB_by_state_all_pixels$X2015_mean/10,col='purple' ,pch = 20,ylim=c(0,100),xlim=c(0,100), xlab = "Mean Field AGB [Mg ha-1]", ylab= 'Mean Estimated AGB [Mg ha-1]', main = "All pixels in state")
#abline(lm(AGB_by_state_all_pixels$AGB_C3 ~AGB_by_state_all_pixels$Mean_2015))
abline(0,1,lty=2)
abline(lm(AGB_by_state_all_pixels$X2015_mean/10~AGB_by_state_all_pixels$AGB_C3), col ='purple')
points(AGB_by_state_all_pixels$AGB_C3,AGB_by_state_all_pixels$AGB_2020, pch = 20, col ='dark gray')
abline(lm(AGB_by_state_all_pixels$AGB_2020~AGB_by_state_all_pixels$AGB_C3), col ='dark gray')
points(AGB_by_state_all_pixels$AGB_C3,AGB_by_state_all_pixels$X_GEDImean, pch = 20, col ='orange')
abline(lm(AGB_by_state_all_pixels$X_GEDImean~AGB_by_state_all_pixels$AGB_C3), col ='orange')
text(AGB_by_state_all_pixels$AGB_C3,AGB_by_state_all_pixels$X2015_mean/10, labels=AGB_by_state_all_pixels$NOM_ENT, cex=0.5, col='purple')
text(AGB_by_state_all_pixels$AGB_C3,AGB_by_state_all_pixels$AGB_2020, labels=AGB_by_state_all_pixels$NOM_ENT, cex=0.5, col='dark gray')
text(AGB_by_state_all_pixels$AGB_C3,AGB_by_state_all_pixels$X_GEDImean, labels=AGB_by_state_all_pixels$NOM_ENT, cex=0.5, col='orange')

legend('topleft', c('2015 AGB map', '2020 AGB map', 'GEDI AGB'), col = c('purple', 'dark gray', 'orange'), pch =20, bg ='transparent')

par(mar=c(4,4,2,2))
plot(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_2015,col='blue' ,pch = 20,ylim=c(0,100),xlim=c(0,100), xlab = "Mean Field AGB [Mg ha-1]", ylab= 'Mean Estimated AGB [Mg ha-1]', main = "NFI Points to map pixels")
#abline(lm(AGB_by_state_tbl$mean_agb.y ~AGB_by_state_tbl$Mean_2015))
abline(0,1,lty=2)
abline(lm(AGB_by_state_tbl$Mean_2015~AGB_by_state_tbl$mean_agb.y.y), col ='blue')
points(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_2020, pch = 20, col ='red')
abline(lm(AGB_by_state_tbl$Mean_2020~AGB_by_state_tbl$mean_agb.y.y), col ='red')
points(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_GEDI, pch = 20, col ='green')
abline(lm(AGB_by_state_tbl$Mean_GEDI~AGB_by_state_tbl$mean_agb.y.y), col ='green')
text(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_2015, labels=AGB_by_state_tbl$NOM_ENT, cex=0.5, col='blue')
text(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_2020, labels=AGB_by_state_tbl$NOM_ENT, cex=0.5, col='red')
text(AGB_by_state_tbl$mean_agb.y.y,AGB_by_state_tbl$Mean_GEDI, labels=AGB_by_state_tbl$NOM_ENT, cex=0.5, col='green')

legend('topleft', c('2015 AGB map', '2020 AGB map', 'GEDI AGB'), col = c('blue', 'red', 'green'), pch =20, bg ='transparent')



#####
# df_agb_tbl <-read.csv('AGB_NFI_vs_Map_States_C3.csv')
# df_agb_tbl <- data.frame(df_agb_tbl)
# 
# agb1 <- cbind(round(df_agb_tbl$mean_agb.y),round(df_agb_tbl$sd_agb), 'NFI', df_agb_tbl$NOM_ENT)
# agb2 <- cbind(round(df_agb_tbl$Mean_2015),round(df_agb_tbl$sd_2015), '2015', df_agb_tbl$NOM_ENT)
# agb3 <- cbind(round(df_agb_tbl$Mean_2020),round(df_agb_tbl$sd_2020), '2020', df_agb_tbl$NOM_ENT)
# 
# 
# df_box <- data.frame(rbind(agb1, agb2, agb3))
# # 
# colnames(df_box) <- c("AGB","SD", "Survey", "State")
# head(df_box)
# 
# df_box$AGB <- as.numeric(df_box$AGB) 
# df_box$SD <- as.numeric(df_box$SD) 
# 
# p <- ggplot(df_box, aes(x=factor(Survey), y=AGB, fill = Survey))+ geom_bar(stat = 'identity', position = position_dodge())+
#   facet_wrap(~State)
# 
# p <- p + geom_errorbar(aes(ymin = AGB - SD, ymax = AGB + SD), width = 0.2)
# 
# p                            
