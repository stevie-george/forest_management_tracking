library(sf)
library(rgdal)
library(raster)
library(ggplot2)
library(dplyr)
library(viridis)

setwd('/Users/stephaniegeorge/Downloads/INFYS_C2_C3/')
list.files(getwd())

# ### Part 1 . Increment from NFI data
# 
# dat2 = read.csv('Comp_Daso_Cgl_V2_mapas2.csv')
# dat3 = read.csv('Daso_A_Cgl_C3_V7_24052022.csv')
# 
# 
# names(dat2)
# names(dat3)
# 
# 
# merged_df = merge(dat2, dat3, by.x = 'Id_Conglomerado', by.y = 'IdConglomerado')
# 
# # remove non measured plots from the third cycle 
# merged_df = merged_df[is.na(merged_df$X_C3)==FALSE,]
# merged_df$interval = merged_df$Anio_C3 - merged_df$Id_Anio
# merged
# 
# # remove NA from AGB column
# df = merged_df[is.na(merged_df$Daso_A_Biomasa_aerea_Ton_HA)==FALSE,]
# df = df[is.na(df$Daso_A_Biom.ha_aerea)==FALSE,]
# df = df[df$Daso_A_Biomasa_aerea_Ton_HA>0,]
# 
# 
# sf_df = df |> st_as_sf(coords=c("X_C3","Y_C3"), crs=4326)
# sf_df = st_transform(sf_df, CRS("+proj=longlat +datum=WGS84 +units=m"))
# 
# 
# states_t = st_read('/Users/stephaniegeorge/Documents/ctrees/Data/Mexico/State_limits/dest20gw.shp')
# states = st_as_sf(states_t,CRS("+proj=longlat +datum=WGS84 +units=m"))
# 
# 
# cover_t = st_read('/Users/stephaniegeorge/Documents/ctrees/Data/Mexico/Land_use/SerieVII_wgs84/Usv_SVII_MGM2021_wgs84.shp')
# cover <- st_as_sf(cover_t,CRS("+proj=longlat +datum=WGS84 +units=m"))
# cover <- st_make_valid(cover)
# 
# sf_df_states = st_join(sf_df, states, left=T)
# sf_df_states
# 
# sf_df_st_cvr = st_join(sf_df_states, cover, left=T)
# head(sf_df_st_cvr)
# 
# #write.csv(sf_df_st_cvr, './dur/merged_c2_c3_st_cv.csv')
# #st_write(sf_df, 'c2_c3_all.shp')
# 
# 
# head(sf_df_states)
# #df_agb = na.omit(sf_df_st_cvr)
# 
# df_agb = sf_df_st_cvr
# #write.csv(df_agb, 'df_agb.csv')

df_agb = read.csv('/Users/stephaniegeorge/Documents/ctrees/Projects/Mexico/dur/merged_c2_c3_st_cv.csv')
head(df_agb)

# root to shoot
rs = .46

df_agb$bio_c3 = df_agb$Daso_A_Biomasa_aerea_Ton_HA + (df_agb$Daso_A_Biomasa_aerea_Ton_HA * rs)
df_agb$bio_c2 = df_agb$Daso_A_Biom.ha_aerea + (df_agb$Daso_A_Biom.ha_aerea * rs)

df_agb$agb_c3 = df_agb$Daso_A_Biomasa_aerea_Ton_HA
df_agb$agb_c2 = df_agb$Daso_A_Biom.ha_aerea

df_agb$agb_dif = df_agb$agb_c3 - df_agb$agb_c2

df_agb$interval = df_agb$Anio_C3 - df_agb$Id_Anio


# Remove deforested plots (plots where biomass is >1 in C3) 
#df_agb_nodef = df_agb[df_agb$Daso_A_Biomasa_aerea_Ton_HA>1,]
# only positive increments
#df_agb_pi = df_agb[df_agb$agb_dif>0,]

# summarize by state and cover type
bio_state_cvr_tbl <- df_agb %>% group_by(NOM_ENT,StratoForm) %>% 
  summarise(mean_bio_c2=mean(bio_c2),
            mean_bio_c3 = mean(bio_c3),
            mean_int = mean(interval),
            mean_agb_c2 = mean(agb_c2),
            mean_agb_c3 = mean(agb_c3),
            mean_interval = mean(interval),
  )%>% st_drop_geometry()



# summarize by state, cover type and category (including secondary vegetation)
bio_state_cvr_tbl <- df_agb %>% group_by(NOM_ENT,StratoForm, DESCRIPCIO) %>% 
  summarise(mean_bio_c2=mean(bio_c2),
            mean_bio_c3 = mean(bio_c3),
            mean_int = mean(interval),
            mean_agb_c2 = mean(agb_c2),
            mean_agb_c3 = mean(agb_c3),
            mean_interval = mean(interval),
  )%>% st_drop_geometry()



df_by_st_cvr = data.frame(bio_state_cvr_tbl)

df_by_st_cvr$agb_dif = df_by_st_cvr$mean_agb_c3 - df_by_st_cvr$mean_agb_c2

df_by_st_cvr$agb_inc = df_by_st_cvr$agb_dif/df_by_st_cvr$mean_interval

df_by_st_cvr$bio_inc = df_by_st_cvr$agb_inc + (df_by_st_cvr$agb_inc * 0.46)


df_by_st_cvr$co2_inc = (df_by_st_cvr$bio_inc * 0.5)*44/12
#write.csv(df_by_st, 'c3-c2_by_state.csv')


# summarize by state, cover type and category (including secondary vegetation)
bio_cvr_sec_tbl <- df_agb %>% group_by(StratoForm, DESCRIPCIO) %>% 
  summarise(mean_bio_c2=mean(bio_c2),
            mean_bio_c3 = mean(bio_c3),
            mean_int = mean(interval),
            mean_agb_c2 = mean(agb_c2),
            mean_agb_c3 = mean(agb_c3),
            mean_interval = mean(interval),
  )%>% st_drop_geometry()


df_by_cvr_sec = data.frame(bio_cvr_sec_tbl)

df_by_cvr_sec$agb_dif = df_by_cvr_sec$mean_agb_c3 - df_by_cvr_sec$mean_agb_c2

df_by_cvr_sec$agb_inc = df_by_cvr_sec$agb_dif/df_by_cvr_sec$mean_interval

df_by_cvr_sec$bio_inc = df_by_cvr_sec$agb_inc + (df_by_cvr_sec$agb_inc * 0.46)


df_by_cvr_sec$co2_inc = (df_by_cvr_sec$bio_inc * 0.5)*44/12
write.csv(df_by_cvr_sec, 'c3-c2_by_cover_second.csv')




# Filter states 
conifbroad = bio_cvr_sec_tbl[bio_cvr_sec_tbl$StratoForm == 'CONIFERAS Y LATIFOLIADAS',]
conifbroad = na.omit(conifbroad)
#write.csv(dur, 'durango_increments.csv')
oax = df_by_st_cvr[df_by_st_cvr$NOM_ENT == 'Oaxaca',]
oax = na.omit(oax)



# Filter states 
dur = df_by_st_cvr[df_by_st_cvr$NOM_ENT == 'Durango',]
dur = na.omit(dur)
#write.csv(dur, 'durango_increments.csv')
oax = df_by_st_cvr[df_by_st_cvr$NOM_ENT == 'Oaxaca',]
oax = na.omit(oax)
#write.csv(oax, 'oaxaca_increments.csv')
unique(df_by_st_cvr$NOM_ENT)
ver = df_by_st_cvr[df_by_st_cvr$NOM_ENT == 'Veracruz de Ignacio de la Llave',]
ver = na.omit(ver)
write.csv(ver, 'veracruz_increments.csv')

chih = df_by_st_cvr[df_by_st_cvr$NOM_ENT == 'Chihuahua',]
chih = na.omit(chih)
write.csv(chih, 'chihuahua_increments.csv')


getwd()


mean(dur$co2_inc[c(1,3,4,5,7),])

par(mar=c(13,4,4,4))
barplot(df_by_st_cvr$co2_inc,main = 'Increments', names.arg=c(df_agb$StratoForm),col="#69b3a2", las = 2, ylab= 'C3 - C2 [ton CO2 ha-1]')


library(ggplot2)
library(reshape2)
library(viridis)

# bp_df = dur[c(2,5,6)]
# df2 <- melt(bp_df, id.vars='StratoForm')
# head(df2)
# ggplot(na.omit(df2), aes(x=StratoForm, y=value, fill=variable)) +
#   geom_bar(stat='identity', position='dodge') +
#   scale_fill_manual(values=c("dark green", "99CC99")) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + labs(x = "", y = "Mean AGB ton CO2 ha-1")
  
head(df_by_st_cvr)
ggplot(df_by_st_cvr, aes(x=StratoForm, y=mean_inc,fill=StratoForm))+
  geom_bar(stat='identity')+
  ylab("mean inc")


ggplot(df_by_st_cvr, aes(x=StratoForm, y=mean_inc,fill=StratoForm))+
  geom_bar(stat='identity')+
  facet_wrap(~NOM_ENT)

durango = df_by_st_cvr[df_by_st_cvr$NOM_ENT == 'Durango',]
oaxaca = df_by_st_cvr[df_by_st_cvr$NOM_ENT == 'Oaxaca',]
puebla = df_by_st_cvr[df_by_st_cvr$NOM_ENT == 'Puebla',]
veracruz = df_by_st_cvr[df_by_st_cvr$NOM_ENT == 'Veracruz de Ignacio de la Llave',]


durango = durango[-c(3:8)]
oaxaca = oaxaca[-c(3:8)]
puebla = puebla[-c(3:8)]
veracruz = veracruz[-c(3:8)]

dov = rbind(durango, oaxaca, veracruz)
dov = na.omit(dov)

durango = na.omit(durango)
oaxaca = na.omit(oaxaca)
veracruz = na.omit(veracruz)

mean(durango$mean_inc_co2)
mean(oaxaca$mean_inc_co2)
mean(veracruz$mean_inc_co2)


# write.csv(df_agb_nodef,'incrementos_co2_estado_cobertura.csv' )

# summarize by state 
AGB_by_state_tbl_nodef <- df_agb_nodef %>% group_by(NOM_ENT) %>%
  summarise(mean_agb_c2=mean(Daso_A_Biom.ha_aerea),
            sd_agb_c2=sd(Daso_A_Biom.ha_aerea),
            var_agb_c2=var(Daso_A_Biom.ha_aerea),
            mean_agb_c3 = mean(Daso_A_Biomasa_aerea_Ton_HA),
            sd_agb_c3 = sd(Daso_A_Biomasa_aerea_Ton_HA),
            var_agb_c3 = var(Daso_A_Biomasa_aerea_Ton_HA),
            mean_inc = mean(inc_co2),
  )%>% st_drop_geometry()



# summarize by state and cover type
AGB_by_state_cvr_tbl <- df_agb_nodef %>% group_by(NOM_ENT,StratoForm) %>%
  summarise(mean_agb_c2=mean(Daso_A_Biom.ha_aerea),
            sd_agb_c2=sd(Daso_A_Biom.ha_aerea),
            var_agb_c2=var(Daso_A_Biom.ha_aerea),
            mean_agb_c3 = mean(Daso_A_Biomasa_aerea_Ton_HA),
            sd_agb_c3 = sd(Daso_A_Biomasa_aerea_Ton_HA),
            var_agb_c3 = var(Daso_A_Biomasa_aerea_Ton_HA),
            #mean_inc = mean(inc_co2),
  )%>% st_drop_geometry()


AGB_by_state_cvr_tbl_sf <- df_agb_nodef %>% group_by(NOM_ENT,StratoForm, DESCRIPCIO) %>%
  summarise(mean_agb_c2=mean(Daso_A_Biom.ha_aerea),
            sd_agb_c2=sd(Daso_A_Biom.ha_aerea),
            var_agb_c2=var(Daso_A_Biom.ha_aerea),
            mean_agb_c3 = mean(Daso_A_Biomasa_aerea_Ton_HA),
            sd_agb_c3 = sd(Daso_A_Biomasa_aerea_Ton_HA),
            var_agb_c3 = var(Daso_A_Biomasa_aerea_Ton_HA),
  )%>% st_drop_geometry()



# summarize by state 
AGB_by_state_tbl_sf <- df_agb %>% group_by(NOM_ENT) %>%
  summarise(mean_agb_c2=mean(Daso_A_Biom.ha_aerea),
            sd_agb_c2=sd(Daso_A_Biom.ha_aerea),
            var_agb_c2=var(Daso_A_Biom.ha_aerea),
            mean_agb_c3 = mean(Daso_A_Biomasa_aerea_Ton_HA),
            sd_agb_c3 = sd(Daso_A_Biomasa_aerea_Ton_HA),
            var_agb_c3 = var(Daso_A_Biomasa_aerea_Ton_HA),
  )%>% st_drop_geometry()



# summarize by state and cover type
AGB_by_state_cvr_tbl_sf <- df_agb %>% group_by(NOM_ENT,StratoForm) %>%
  summarise(mean_agb_c2=mean(Daso_A_Biom.ha_aerea),
            sd_agb_c2=sd(Daso_A_Biom.ha_aerea),
            var_agb_c2=var(Daso_A_Biom.ha_aerea),
            mean_agb_c3 = mean(Daso_A_Biomasa_aerea_Ton_HA),
            sd_agb_c3 = sd(Daso_A_Biomasa_aerea_Ton_HA),
            var_agb_c3 = var(Daso_A_Biomasa_aerea_Ton_HA),
  )%>% st_drop_geometry()





