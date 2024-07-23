library(sf)
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
library(viridis)

setwd('/Users/stephaniegeorge/Downloads/INFYS_C2_C3/')

dat2 = read.csv('Comp_Daso_Cgl_V2_mapas2.csv')
dat3 = read.csv('Daso_A_Cgl_C3_V7_24052022.csv')


names(dat2)
names(dat3)


merged_df = merge(dat2, dat3, by.x = 'Id_Conglomerado', by.y = 'IdConglomerado')

#remover NA de la coordenada en X
merged_df = merged_df[is.na(merged_df$X_C3)==FALSE,]
merged_df$interval = merged_df$Anio_C3 - merged_df$Id_Anio
merged

#remover NA de la variable de interes e.g., Carbono = Daso_A_CO2.ha_aerea
df = merged_df[is.na(merged_df$Daso_A_Biomasa_aerea_Ton_HA)==FALSE,]
df = df[is.na(df$Daso_A_Biom.ha_aerea)==FALSE,]
df = df[df$Daso_A_Biomasa_aerea_Ton_HA>0,]


sf_df = df |> st_as_sf(coords=c("X_C3","Y_C3"), crs=4326)
sf_df = st_transform(sf_df, CRS("+proj=longlat +datum=WGS84 +units=m"))


states_t = st_read('/Users/stephaniegeorge/Documents/ctrees/Data/Mexico/State_limits/dest20gw.shp')
states = st_as_sf(states_t,CRS("+proj=longlat +datum=WGS84 +units=m"))


cover_t = st_read('/Users/stephaniegeorge/Documents/ctrees/Data/Mexico/Land_use/SerieVII_wgs84/Usv_SVII_MGM2021_wgs84.shp')
cover <- st_as_sf(cover_t,CRS("+proj=longlat +datum=WGS84 +units=m"))
cover <- st_make_valid(cover)


sf_df_states = st_join(sf_df, states, left=T)
sf_df_states

sf_df_st_cvr = st_join(sf_df_states, cover, left=T)
head(sf_df_st_cvr)

#write.csv(sf_df_st_cvr, './dur/merged_c2_c3_st_cv.csv')
#st_write(sf_df, 'c2_c3_all.shp')

head(sf_df_states)
#df_agb = na.omit(sf_df_st_cvr)

df_agb = sf_df_st_cvr
#write.csv(df_agb, 'df_agb.csv')





# summarize by state 
AGB_by_state_tbl <- df_agb %>% group_by(NOM_ENT) %>%
  summarise(mean_agb_c2=mean(Daso_A_Biom.ha_aerea),
            #sd_agb=sd(AGB),
            #var_agb=var(AGB),
            mean_agb_c3 = mean(Daso_A_Biomasa_aerea_Ton_HA),
            #sd_2015 = sd(AGB_2015),
            #var_2015 = var(AGB_2015),
            #Mean_2020 = mean(AGB_2020),
            #sd_2020 = sd(AGB_2020),
            #var_2020 = var(AGB_2020)
  )%>% st_drop_geometry()


df_by_st = as.data.frame(AGB_by_state_tbl)

df_by_st$agb_co2_c3 = (df_by_st$mean_agb_c3*.5)*44/12
df_by_st$agb_co2_c2 = (df_by_st$mean_agb_c2*.5)*44/12


df_by_st$inc = df_by_st$mean_agb_c3 - df_by_st$mean_agb_c2
df_by_st$inc_co2 = (df_by_st$agb_co2_c3 - df_by_st$agb_co2_c2)/df_by_st$interval

#write.csv(df_by_st, 'c3-c2_by_state.csv')
# summarize by cover

# Left join 
AGB_by_cvr_tbl <- df_agb %>% group_by(StratoForm) %>%
  summarise(mean_agb_c2=mean(Daso_A_Biom.ha_aerea),
            #sd_agb=sd(AGB),
            #var_agb=var(AGB),
            mean_agb_c3 = mean(Daso_A_Biomasa_aerea_Ton_HA),
            #sd_2015 = sd(AGB_2015),
            #var_2015 = var(AGB_2015),
            #Mean_2020 = mean(AGB_2020),
            #sd_2020 = sd(AGB_2020),
            #var_2020 = var(AGB_2020)
  )%>% st_drop_geometry() 

df_by_cv = as.data.frame(AGB_by_cvr_tbl)


AGB_by_state_cvr_tbl <- df_agb %>% group_by(NOM_ENT,StratoForm) %>% 
  summarise(mean_agb_c2=mean(Daso_A_Biom.ha_aerea),
            #var_agb=var(AGB),
            mean_agb_c3 = mean(Daso_A_Biomasa_aerea_Ton_HA),
            #sd_2015 = sd(AGB_2015),
            #var_2015 = var(AGB_2015),
            #Mean_2020 = mean(AGB_2020),
            #sd_2020 = sd(AGB_2020),
            #var_2020 = var(AGB_2020)
  ) %>% add_count() %>% st_drop_geometry()


AGB_by_state_cvr_tbl <- df_agb %>% group_by(NOM_ENT,StratoForm) %>% 
  summarise(mean_agb_c2=mean(Daso_A_Biom.ha_aerea),
            #var_agb=var(AGB),
            mean_agb_c3 = mean(Daso_A_Biomasa_aerea_Ton_HA),
            #sd_2015 = sd(AGB_2015),
            #var_2015 = var(AGB_2015),
            #Mean_2020 = mean(AGB_2020),
            #sd_2020 = sd(AGB_2020),
            #var_2020 = var(AGB_2020)
  )%>% st_drop_geometry()


 
df_by_st_cv = as.data.frame(AGB_by_state_cvr_tbl)

df_by_st_cv$agb_co2_c3 = (df_by_st_cv$mean_agb_c3*.5)*44/12
df_by_st_cv$agb_co2_c2 = (df_by_st_cv$mean_agb_c2*.5)*44/12
df_by_st_cv$inc = (df_by_st_cv$mean_agb_c3 - df_by_st_cv$mean_agb_c2)/df_by_st_cv$interval
df_by_st_cv$inc_co2 = (df_by_st_cv$agb_co2_c3 - df_by_st_cv$agb_co2_c2)/df_by_st_cv$interval

#write.csv(df_by_st_cv, 'c3-c2_by_state_cover.csv')
write.csv(df_by_st_cv, 'c3-c2_by_state_cover_interv.csv')


dur = df_by_st_cv[df_by_st_cv$NOM_ENT == 'Durango',]
par(mar=c(13,4,4,4))
barplot(dur$inc_co2,main = 'Durango', names.arg=c(dur$StratoForm),col="#69b3a2", las = 2, ylab= 'C3 - C2 [ton CO2 ha-1]', ylim = c(0,16))


library(ggplot2)
library(reshape2)
library(viridis)
head(dur)
bp_df = dur[c(2,5,6)]

df2 <- melt(bp_df, id.vars='StratoForm')
head(df2)

ggplot(na.omit(df2), aes(x=StratoForm, y=value, fill=variable)) +
  geom_bar(stat='identity', position='dodge') +
  scale_fill_manual(values=c("dark green", "99CC99")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(x = "", y = "Mean AGB ton CO2 ha-1")
  



head(merged_df)





