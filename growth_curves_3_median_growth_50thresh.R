library(pacman) # install.packages("pacman")
p_load(terra,sf,raster,fst,ggplot2,ggpubr,dplyr,viridis,ggpointdensity,BIOMASS,collapse,xgboost)

library(tidyverse)
library(investr)
library(minpack.lm)
##############################################################################################################################
# system("aws s3 sync --profile ctrees s3://ctrees-input-data/jrc_deforestation_degradation/ ~/lb_sagang/jrc_defor_degrad")
#list_jrc <- list.files(path = "/Users/stephaniegeorge/Downloads/jrc_mexico", pattern = "\\.tif$", full.names = TRUE)
#list_jrc <- list.files(path = "/Users/stephaniegeorge/Downloads/CTrees_deforestation_50_thresh_Mexico_TV7", pattern = "\\.tif$", full.names = TRUE)

list_jrc <- list.files(path = "/Users/stephaniegeorge/Downloads/thresh_50/Deforestation/", pattern = "\\.tif$", full.names = TRUE)


# list_tree_cov = raster('/Users/stephaniegeorge/Documents/ctrees/Projects/Mexico/Mex_Project_RSData/Hansen/lossyear/lossyear_30m.tif')

agb_aoi = rast("/Users/stephaniegeorge/Documents/ctrees/Data/Maps/Mexico_biomass_100m_2020.tif")
#agb_aoi = rast("/Users/stephaniegeorge/Documents/ctrees/Data/CCI/AGB_2020/ESA_CCI_AGB_Mex_2020.tif")


cov = st_read('/Users/stephaniegeorge/Documents/ctrees/Mexico_AD/shapes/stratoform_mex_SVII.shp')


#shp_africa = vect("E:/GABON/Data_Gabon/Maps/Africa_Boundaries/Africa_Boundaries.shp")
#cb_countries = c('Cameroon','Equatorial Guinea','Central African Republic',
#                 'Gabon','Democratic Republic of the Congo','Republic of Congo')
#shp_id = shp_africa[which(shp_africa$NAME_0 %in% c(cb_countries))]
#plot(shp_id)


## oaxaca test area
# shp_id = vect(ext(-97.85819777436336, -95.00175246186336,  15.635267239745588, 17.954236679584614))
# plot(shp_id)


## chiapas
# shp_id = vect(ext(-89.47882469793169, -87.85010155340044, 18.44542843348844, 19.843916891187288))
# plot(shp_id)


shp_id = vect('/Users/stephaniegeorge/Documents/ctrees/Data/Mexico/Contornos/republica/conto4mgw.shp')
plot(shp_id)


#agb_aoi = agb_aoi %>% crop(shp_id) %>% mask(shp_id)
#plot(agb_aoi)


grd_lrg <- st_make_grid(shp_id, cellsize = c(.5,.5)) %>% #
  st_set_crs(4326) %>%
  st_transform("epsg:4326")

grd_lrg = vect(grd_lrg)
crs(grd_lrg) = "epsg:4326"
plot(grd_lrg)
plot(shp_id, add = T)
#####################
##################### deforestation
require(venn)
# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv = uniqv[!is.na(uniqv)]
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

getsd = function (x, na.rm = TRUE) {
  if (is.matrix(x)) 
    apply(x, 2, sd, na.rm = na.rm)
  else if (is.vector(x)) 
    sqrt(var(x, na.rm = na.rm))
  else if (is.data.frame(x)) 
    sapply(x, sd, na.rm = na.rm)
  else sqrt(var(as.vector(x), na.rm = na.rm))
}


get_quantile <- function(raster, polygons, probs) {
  # Check if probabilities are provided, if not, set default to 0.5 (median)
  if (missing(probs)) {
    probs <- 0.5
  }
  
  # Extract pixel values within polygons
  extracted_values <- terra::extract(raster,polygons)
  
  # Compute quantiles for each polygon
  quantiles <- apply(extracted_values, 1, function(x) {
    quantile(x, probs, na.rm = TRUE)
  })
  
  # Return a data frame with quantiles
  quantiles_df <- data.frame(quantiles)
  return(quantiles_df)
}

list_def = list_jrc[grep("def", list_jrc)]
#list_deg = list_jrc[grep("Deg", list_jrc)]


sf_use_s2(FALSE)
# i = 52

for (tile in 1:length(grd_lrg)) {

  poly_tile_i = grd_lrg[tile]
  poly_overlap = terra::intersect(poly_tile_i,shp_id)

  if (dim(poly_overlap)[1] >0) {

    # list deforestation
    filt_deforestation <- list()
    for (i in 1:length(list_def)) {
      ras_i = rast(list_def[i])
      poly_ras_i = as.polygons(ext(ras_i))
      poly_overlap = terra::intersect(poly_ras_i,poly_tile_i)

      if (is.null(dim(poly_overlap)[1])) {next}
      if (dim(poly_overlap)[1] >0) {

        ras_i = crop(ras_i,poly_tile_i,mask=TRUE)

        filt_deforestation[[length(filt_deforestation) + 1]] <- ras_i}

      # print(paste0(" ------------Tile------------- ",i," / ",length(list_def)))
    }
    if(length(filt_deforestation)==1){ras_defor_i = filt_deforestation[[1]]}
    if(length(filt_deforestation)>1){ras_defor_i <- do.call(terra::merge, filt_deforestation)}
    names(ras_defor_i)="deforestation_year"
    ras_defor_i[ras_defor_i %in% c(0,2021:2022)] = NA

    if(length(freq(ras_defor_i)$value) >1){
      # create polygons
      poly_def_j = as.polygons(ras_defor_i, dissolve=TRUE,na.rm=TRUE)
      crs(poly_def_j) = "epsg:4326"

      # add poly area values
      poly_def_j$area_ha = expanse(poly_def_j, unit="ha")

      # add mean AGB values
      agb_mean = terra::extract(agb_aoi,poly_def_j,fun='mean',na.rm=TRUE)
      poly_def_j$mean_agb = agb_mean[,2]

      agb_median = terra::extract(agb_aoi,poly_def_j,fun='median',na.rm=TRUE)
      poly_def_j$median_agb = agb_median[,2]
      # add sd AGB values
      agb_sd = terra::extract(agb_aoi,poly_def_j,fun = 'sd',na.rm=TRUE)
      poly_def_j$sd_agb = agb_sd[,2]

      agb_q25 = get_quantile(agb_aoi,poly_def_j,.25)
      poly_def_j$q25_agb = agb_q25
      
      agb_q50 = get_quantile(agb_aoi,poly_def_j,.50)
      poly_def_j$q50_agb = agb_q50
      
      agb_q75 = get_quantile(agb_aoi,poly_def_j,.75)
      poly_def_j$q75_agb = agb_q75

      agb_q95 = get_quantile(agb_aoi,poly_def_j,.95)
      poly_def_j$q95_agb = agb_q95

      # # add forest management types
      # management_type = terra::extract(ras_logg_protect,poly_def_j,fun=getmode)
      # names(management_type) = c("id","manage_type")
      # poly_def_j$manage_type = management_type$manage_type
      #
      # # add vegetation cover
      # # veg_type = terra::extract(ras_ecoregion,poly_def_j,fun=getmode)
      # # names(veg_type) = c("id","veg_type")
      # # poly_def_j$veg_type = veg_type$veg_type
      #
      # # add vegetation type cci
      # veg_type_cci = terra::extract(ras_ecoregion,poly_def_j,fun=getmode)
      # names(veg_type_cci) = c("id","veg_type_cci")
      # poly_def_j$veg_type_cci = veg_type_cci$veg_type_cci

      # add lat long
      sf_poly_def_j = st_as_sf(poly_def_j)
      centoid_poly = st_centroid(sf_poly_def_j)[,1]
      centoid_poly = as.data.frame(st_coordinates (centoid_poly))

      poly_def_j$lat = centoid_poly$Y
      poly_def_j$lon = centoid_poly$X

      data_def_j = as.data.frame(poly_def_j)
      # data_def_all_list[[i]] = as.data.frame(poly_def_i)
      write.fst(data_def_j,paste0('/Users/stephaniegeorge/temp_ctrees_agb_thresh_50_deforestation/deforestation_tile_',tile,".fst"))

    }

    print(paste0(" ------------Tile------------- ",tile," / ",length(grd_lrg)))
  }

}

#### part 2


######### deforestation
list_df_def = list.files(path = '/Users/stephaniegeorge/temp_ctrees_agb_thresh_50_deforestation', pattern = ".fst*", full.names = TRUE)
# merge dataset
list_data_def_all = list()

for (i in 1:length(list_df_def)) {
  
  data_tmp = read_fst(list_df_def[i])
  
  if(length(unique(data_tmp$mean_agb))<=1){next}
  
  if(length(unique(data_tmp$mean_agb))>0){
    
    list_data_def_all[[length(list_data_def_all) + 1]] <- data_tmp
    
    print(paste0(" ------------Tile------------- ",i," / ",length(list_df_def)))
    
  }
  
}

data_def_all = do.call("rbind", list_data_def_all)
head(data_def_all)

names(data_def_all)[which(names(data_def_all) == "lon")] ="x"
names(data_def_all)[which(names(data_def_all) == "lat")] ="y"

unique(data_def_all$deforestation_year)
head(data_def_all)

data_def_all <- data_def_all[ which(data_def_all$deforestation_year < 2021), ]
unique(data_def_all$deforestation_year)

#data_tmp = data_tmp[which(data_tmp$age >3,)] %>% as.data.table()
data_def_all$age = 2020 - data_def_all$deforestation_year

require(data.table)
data_def_all = as.data.table(data_def_all)

head(data_def_all)


data_def_all = st_as_sf(data_def_all, coords=c("x","y"), crs=4326)

data_def_all_cov = st_join(data_def_all,cov)

data_def_all_cov = as.data.table(data_def_all_cov)

head(data_def_all_cov)

unique(data_def_all_cov$age)
# write_csv(data_def_all_cov, '/Users/stephaniegeorge/Documents/ctrees/space_x_time/tmf_deforestation_cci_biomass_cover.csv')

# data_def_all_filt$age = as.factor(data_def_all_filt$age)
data_def_summary_management = data_def_all_cov[, by=.(StratoForm,age),#ecoregion_1km_mane,
                                           .(mean_agb = mean(mean_agb/10 ,na.rm=TRUE),
                                             median_agb = mean(median_agb/10 ,na.rm=TRUE),
                                             q25_agb = mean(q25_agb/10 ,na.rm=TRUE),
                                             
                                             q75_agb = mean(q75_agb/10 ,na.rm=TRUE),
                                             q95_agb = mean(q95_agb/10 ,na.rm=TRUE),
                                             nrow = length(mean_agb))]



ggplot()+
  #ylim(0,200)+
  geom_line(aes(x=age,y=q95_agb),data = data_def_summary_management[,c("age","q95_agb", 'StratoForm')],color="red")+ #,"ecoregion_1km_mane"
  geom_line(aes(x=age,y=q75_agb),data = data_def_summary_management[,c("age","q75_agb", 'StratoForm')],color="green4")+ #,"ecoregion_1km_mane"
  # geom_point(aes(x=age,y=mean_agb),data = data_def_summary_management[,c("age","mean_agb","management_name")],color="chartreuse")+ #,"ecoregion_1km_mane"
  geom_line(aes(x=age,y=median_agb),data = data_def_summary_management[,c("age","median_agb", 'StratoForm')],color="cyan")+ #,"ecoregion_1km_mane"
  geom_line(aes(x=age,y=q25_agb),data = data_def_summary_management[,c("age","q25_agb", 'StratoForm')],color="orange")+ #,"ecoregion_1km_mane"
  # facet_grid(ecoregion_1km_mane ~ management_name)+
  facet_wrap( ~ StratoForm)+
  theme_dark()


unique(data_def_all_cov$StratoForm)

list_management = c("SELVAS BAJAS","SELVAS ALTAS Y MEDIANAS","LATIFOLIADAS", "CONIFERAS Y LATIFOLIADAS", "CONIFERAS","BOSQUE MESOFILO")


data_tmp = data_def_all_cov[which(data_def_all_cov$StratoForm == list_management[i]),] %>% as.data.table()
head(data_tmp)

#########
# q95
# summary_q95 = data_tmp[, by=.(age),#ecoregion_1km_mane,
#                        .(mean_q95 = mean(q95_agb/10 ,na.rm=TRUE),
#                          nrow = length(q95_agb))]
# 
# unique(data_tmp$age)
# 
# A_q95 = max(data_tmp$q95_agb[which(data_tmp$age < 20)])
# min_A_q95 = min(data_tmp$q25_agb[which(data_tmp$age < 4)])
# #median_q95 = median(data_tmp$median_agb[which(data_tmp$age == 20)])
# 
# age_tmp = matrix (c(300,A_q95,101), nrow=1, ncol = 3, byrow = T)
# 
# summary_q95 = rbind(summary_q95, age_tmp, use.names = F)
# 
# tail(summary_q95)
# 
# curve_q95 = nlsLM(mean_q95 ~ A * (1 - exp(-k * age))^m,
#                   start = list(A = A_q95, k=1, m = 1),
#                   lower = c(min_A_q95,.01,.5),
#                   data = summary_q95)



# ### linearize model
# c.0 <- min_A_q95 * 0.5
# model.0 <- lm(log(mean_q95 - c.0) ~ age, data=summary_q95)
# start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=c.0)
# model <- nls(mean_q95 ~ a * exp(b * age) + c, data = summary_q95, start = start)
# 
# #view results for convergence
# model
# 
# plot(summary_q95$mean_q95)
# p <- coef(model)
# curve(p["a"] * exp(p["b"] * x) + p["c"], lwd=2, col="Red", add=TRUE)




# pred_q95 = as.data.frame(seq(0:100))
# names(pred_q95) = "age"
# interval_q95 <- as_tibble(predFit(curve_q95, newdata = pred_q95, interval = "confidence", level= 0.95)) %>% 
#   mutate(age = pred_q95$age)
# interval_q95$lwr[which(interval_q95$lwr<0)]=0
# interval_q95$sd = interval_q95$upr - interval_q95$fit
# 
# tmp_q95 = summary_q95[,c("age","mean_q95")]
# tmp_q95 = tmp_q95[which(tmp_q95$age %in% c(1:40,300))]
# tmp_q95$age[which(tmp_q95$age == 300)] = 60
# 
# 
# head(data_tmp)
#########
# q75
# summary_q75 = data_tmp[, by=.(age),#ecoregion_1km_mane,
#                        .(mean_q75 = mean(q75_agb/10 ,na.rm=TRUE),
#                          nrow = length(q75_agb))]
# 
# 
# 
# A_q75 = max(data_tmp$q75_agb[which(data_tmp$age > 20)], na.rm = T)
# min_A_q75 = min(data_tmp$q25_agb[which(data_tmp$age < 4)])
# #median_q75 = median(data_tmp$median_agb[which(data_tmp$age == 32)])
# 
# 
# 
# 
# age_tmp = matrix (c(300,A_q75,101), nrow=1, ncol = 3, byrow = T)
# 
# summary_q75 = rbind(summary_q75, age_tmp, use.names = F)
# 
# tail(summary_q75)
# 
# curve_q75 = nlsLM(mean_q75 ~ A * (1 - exp(-k * age))^m,
#                   start = list(A = A_q75, k=1, m = 1),
#                   lower = c(min_A_q75,.01,.5),
#                   data = summary_q75)
# 
# 
# 
# pred_q75 = as.data.frame(seq(0:100))
# names(pred_q75) = "age"
# interval_q75 <- as_tibble(predFit(curve_q75, newdata = pred_q75, interval = "confidence", level= 0.95)) %>% 
#   mutate(age = pred_q75$age)
# interval_q75$lwr[which(interval_q75$lwr<0)]=0
# interval_q75$sd = interval_q75$upr - interval_q75$fit
# 
# tmp_q75 = summary_q75[,c("age","mean_q75")]
# tmp_q75 = tmp_q75[which(tmp_q75$age %in% c(1:40,300))]
# tmp_q75$age[which(tmp_q75$age == 300)] = 60


#########
# q50
summary_q50 = data_tmp[, by=.(age),#ecoregion_1km_mane,
                       .(mean_q50 = mean(q50_agb/10 ,na.rm=TRUE),
                         nrow = length(q50_agb))]


A_q50 = max(summary_q50$mean_q50[which(summary_q50$age > 5)])
min_A_q50 = min(summary_q50$mean_q50[which(summary_q50$age < 4)])

# A_q50 = max(data_tmp$q50_agb[which(data_tmp$age > 5)])
# min_A_q50 = min(data_tmp$q25_agb[which(data_tmp$age < 4)])
#median_q50 = median(data_tmp$median_agb[which(data_tmp$age == 20)])

# A_q50 = 1500
# min_A_q50 = 750


age_tmp = matrix (c(300,A_q50,101), nrow=1, ncol = 3, byrow = T)

summary_q50 = rbind(summary_q50, age_tmp, use.names = F)

tail(summary_q50)

curve_q50 = nlsLM(mean_q50 ~ A * (1 - exp(-k * age))^m,
                  start = list(A = A_q50, k=1, m = 1),
                  lower = c(min_A_q50,.01,.5),
                  data = summary_q50)



pred_q50 = as.data.frame(seq(0:100))
names(pred_q50) = "age"
interval_q50 <- as_tibble(predFit(curve_q50, newdata = pred_q50, interval = "confidence", level= 0.95)) %>% 
  mutate(age = pred_q50$age)
interval_q50$lwr[which(interval_q50$lwr<0)]=0
interval_q50$sd = interval_q50$upr - interval_q50$fit

tmp_q50 = summary_q50[,c("age","mean_q50")]
tmp_q50 = tmp_q50[which(tmp_q50$age %in% c(1:40,300))]
tmp_q50$age[which(tmp_q50$age == 300)] = 60


# #########
# # q25
# summary_q25 = data_tmp[, by=.(age),#ecoregion_1km_mane,
#                        .(mean_q25 = mean(q25_agb ,na.rm=TRUE),
#                          nrow = length(q25_agb))]
# 
# 
# 
# A_q25 = max(data_tmp$q25_agb[which(data_tmp$age > 20)])
# min_A_q25 = min(data_tmp$q25_agb[which(data_tmp$age < 4)])
# #median_q25 = median(data_tmp$median_agb[which(data_tmp$age == 32)])
# 
# # A_q25 = 1250
# # min_A_q25 = 725
# 
# 
# age_tmp = matrix (c(300,A_q25,101), nrow=1, ncol = 3, byrow = T)
# 
# summary_q25 = rbind(summary_q25, age_tmp, use.names = F)
# 
# tail(summary_q25)
# 
# curve_q25 = nlsLM(mean_q25 ~ A * (1 - exp(-k * age))^m,
#                   start = list(A = A_q25, k=1, m = 1),
#                   lower = c(min_A_q25,.01,.5),
#                   data = summary_q25)
# 
# 
# 
# pred_q25 = as.data.frame(seq(0:100))
# names(pred_q25) = "age"
# interval_q25 <- as_tibble(predFit(curve_q25, newdata = pred_q25, interval = "confidence", level= 0.95)) %>% 
#   mutate(age = pred_q25$age)
# interval_q25$lwr[which(interval_q25$lwr<0)]=0
# interval_q25$sd = interval_q25$upr - interval_q25$fit
# 
# tmp_q25 = summary_q25[,c("age","mean_q25")]
# tmp_q25 = tmp_q25[which(tmp_q25$age %in% c(1:40,300))]
# tmp_q25$age[which(tmp_q25$age == 300)] = 60



cex.label = 12
cex.axis = 12
point_size = 2
point_size_2 = 2


ggplot()+
  ggtitle(list_management[i])+ #"id_name"
  # xlim(0,60)+
  ylim(0,50)+
  labs(x="Time since disturbance (Years)", y= expression(paste("AGB (Mg C ",ha^-1,")")))+
  
  
  # #q95
  # geom_ribbon(aes(x=age, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="green",data=interval_q95[which(interval_q95$age <=50),])+
  # geom_point(aes(x = age,y = mean_q95, fill = "q95"),size=point_size,color='black',alpha=.5,shape=21,data=summary_q95[which(summary_q95$age <=60),])+
  # geom_point(aes(x = age,y = mean_q95, fill = "q95"),size=point_size_2,color='black',shape=21,data=tmp_q95[which(tmp_q95$age ==60),])+
  # geom_line(aes(x = age,y = fit),lwd=1,color="green",data=interval_q95[which(interval_q95$age <=50),])+
  # 
  # #q75
  # geom_ribbon(aes(x=age, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="green",data=interval_q75[which(interval_q75$age <=50),])+
  # geom_point(aes(x = age,y = mean_q75, fill = "q75"),size=point_size,color='black',alpha=.5,shape=21,data=summary_q75[which(summary_q75$age <=60),])+
  # geom_point(aes(x = age,y = mean_q75, fill = "q75"),size=point_size_2,color='black',shape=21,data=tmp_q75[which(tmp_q75$age ==60),])+
  # geom_line(aes(x = age,y = fit),lwd=1,color="green",data=interval_q75[which(interval_q75$age <=50),])+
  
  
  #q50
  geom_ribbon(aes(x=age, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="black",data=interval_q50[which(interval_q50$age <=50),])+
  geom_point(aes(x = age,y = mean_q50, fill = "q50"),size=point_size,color='black',alpha=.5,shape=21,data=summary_q50[which(summary_q50$age <=60),])+
  geom_point(aes(x = age,y = mean_q50, fill = "q50"),size=point_size_2,color='black',shape=21,data=tmp_q50[which(tmp_q50$age ==60),])+
  geom_line(aes(x = age,y = fit),lwd=1,lty=2,color="black",data=interval_q50[which(interval_q50$age <=50),])+
  # 
  
  # #q25
  # geom_ribbon(aes(x=age, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="blue",data=interval_q25[which(interval_q25$age <=50),])+
  # geom_point(aes(x = age,y = mean_q25, fill = "q25"),size=point_size,color='black',alpha=.5,shape=21,data=summary_q25[which(summary_q25$age <=60),])+
  # geom_point(aes(x = age,y = mean_q25, fill = "q25"),size=point_size_2,color='black',shape=21,data=tmp_q25[which(tmp_q25$age ==60),])+
  # geom_line(aes(x = age,y = fit),lwd=1,color="blue",data=interval_q25[which(interval_q25$age <=50),])+
  
  
  scale_fill_manual(labels = c("q95"="AGC q95","q50"="AGC q50","q25"="AGC q25"),
                    values = c("q95"="green","q50"="black","q25"="blue"),
                    breaks = c("q95", "q50", "q25"))+
  
  
  
  # scale_x_continuous(breaks=seq(0,120,by=20),
  #                    labels=c("0"="0","20"="20","40"="40","60"="60","80"="80","100"="100","120"="OGF"))+
  scale_x_continuous(breaks=seq(0,60,by=10),
                     labels=c("0"="0","10"="10","20"="20","30"="30","40"="40","50"="50","60"="OGF"))+
  #scale_fill_manual(labels = c("Concession","Protected","Unprotected"), values = c("red", "green4","orange4"))+
  guides(shape="none")+
  theme_classic()+
  theme(axis.title.y = element_text(size = cex.label),
        axis.title.x = element_text(size = cex.label),
        axis.text.y = element_text(size=cex.axis,color = "black",angle = 90,hjust = 0.5),
        axis.text.x = element_text(size=cex.axis,color = "black"),
        axis.ticks = element_line(linewidth = .5, linetype = "solid",colour = "black"),
        legend.direction = "vertical",
        legend.position = c(0, 1),
        legend.justification = c(0, 1),
        legend.box = "Vertical",
        legend.key.size = unit(0.4, "cm"),
        legend.background=element_rect(fill = alpha("white", 0)),
        legend.title = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
        panel.background = element_rect(colour = "black", linewidth=0))


## end 


# ### 
# 
# data_tmp = data_def_all_cov[which(data_def_all_cov$StratoForm == list_management[i]),] %>% as.data.table()
# head(data_tmp)
# 
# #data_tmp = data_tmp[which(data_tmp$age >3,)] %>% as.data.table()
# #########
# # q95
# summary_q95 = data_tmp[, by=.(age),#ecoregion_1km_mane,
#                        .(mean_q95 = mean(q95_agb ,na.rm=TRUE),
#                          nrow = length(q95_agb))]
# 
# 
# 
# A_q95 = max(data_tmp$q95_agb[which(data_tmp$age == 32)])
# min_A_q95 = min(data_tmp$q25_agb[which(data_tmp$age == 32)])
# median_q95 = median(data_tmp$median_agb[which(data_tmp$age == 32)])
# 
# # A_q95 = 1500
# # min_A_q95 = 700
# 
# 
# age_tmp = matrix (c(300,A_q95,101), nrow=1, ncol = 3, byrow = T)
# 
# summary_q95 = rbind(summary_q95, age_tmp, use.names = F)
# 
# tail(summary_q95)
# 
# curve_q95 = nlsLM(mean_q95 ~ A * (1 - exp(-k * age))^m,
#                   start = list(A = A_q95, k=1, m = 1),
#                   lower = c(min_A_q95,.01,.5),
#                   data = summary_q95)
# 
# 
# 
# # ### linearize model
# # c.0 <- min_A_q95 * 0.5
# # model.0 <- lm(log(mean_q95 - c.0) ~ age, data=summary_q95)
# # start <- list(a=exp(coef(model.0)[1]), b=coef(model.0)[2], c=c.0)
# # model <- nls(mean_q95 ~ a * exp(b * age) + c, data = summary_q95, start = start)
# # 
# # #view results for convergence
# # model
# # 
# # plot(summary_q95$mean_q95)
# # p <- coef(model)
# # curve(p["a"] * exp(p["b"] * x) + p["c"], lwd=2, col="Red", add=TRUE)
# 
# 
# 
# 
# pred_q95 = as.data.frame(seq(0:100))
# names(pred_q95) = "age"
# interval_q95 <- as_tibble(predFit(curve_q95, newdata = pred_q95, interval = "confidence", level= 0.95)) %>% 
#   mutate(age = pred_q95$age)
# interval_q95$lwr[which(interval_q95$lwr<0)]=0
# interval_q95$sd = interval_q95$upr - interval_q95$fit
# 
# tmp_q95 = summary_q95[,c("age","mean_q95")]
# tmp_q95 = tmp_q95[which(tmp_q95$age %in% c(1:40,300))]
# tmp_q95$age[which(tmp_q95$age == 300)] = 60
# 
# 
# head(data_tmp)
# #########
# # q75
# summary_q75 = data_tmp[, by=.(age),#ecoregion_1km_mane,
#                        .(mean_q75 = mean(q75_agb ,na.rm=TRUE),
#                          nrow = length(q75_agb))]
# 
# 
# 
# A_q75 = max(data_tmp$q75_agb[which(data_tmp$age == 32)], na.rm = T)
# min_A_q75 = min(data_tmp$q25_agb[which(data_tmp$age == 32)])
# median_q75 = median(data_tmp$median_agb[which(data_tmp$age == 32)])
# 
# # A_q75 = 1750
# # min_A_q75 = 775
# 
# 
# age_tmp = matrix (c(300,A_q75,101), nrow=1, ncol = 3, byrow = T)
# 
# summary_q75 = rbind(summary_q75, age_tmp, use.names = F)
# 
# tail(summary_q75)
# 
# curve_q75 = nlsLM(mean_q75 ~ A * (1 - exp(-k * age))^m,
#                   start = list(A = A_q75, k=1, m = 1),
#                   lower = c(min_A_q75,.01,.5),
#                   data = summary_q75)
# 
# 
# 
# pred_q75 = as.data.frame(seq(0:100))
# names(pred_q75) = "age"
# interval_q75 <- as_tibble(predFit(curve_q75, newdata = pred_q75, interval = "confidence", level= 0.95)) %>% 
#   mutate(age = pred_q75$age)
# interval_q75$lwr[which(interval_q75$lwr<0)]=0
# interval_q75$sd = interval_q75$upr - interval_q75$fit
# 
# tmp_q75 = summary_q75[,c("age","mean_q75")]
# tmp_q75 = tmp_q75[which(tmp_q75$age %in% c(1:40,300))]
# tmp_q75$age[which(tmp_q75$age == 300)] = 60
# 
# 
# #########
# # q50
# summary_q50 = data_tmp[, by=.(age),#ecoregion_1km_mane,
#                        .(mean_q50 = mean(q50_agb ,na.rm=TRUE),
#                          nrow = length(q50_agb))]
# 
# 
# 
# A_q50 = max(data_tmp$q50_agb[which(data_tmp$age == 32)])
# min_A_q50 = min(data_tmp$q25_agb[which(data_tmp$age == 32)])
# median_q50 = median(data_tmp$median_agb[which(data_tmp$age == 32)])
# 
# # A_q50 = 1500
# # min_A_q50 = 750
# 
# 
# age_tmp = matrix (c(300,A_q50,101), nrow=1, ncol = 3, byrow = T)
# 
# summary_q50 = rbind(summary_q50, age_tmp, use.names = F)
# 
# tail(summary_q50)
# 
# curve_q50 = nlsLM(mean_q50 ~ A * (1 - exp(-k * age))^m,
#                   start = list(A = A_q50, k=1, m = 1),
#                   lower = c(min_A_q50,.01,.5),
#                   data = summary_q50)
# 
# 
# 
# pred_q50 = as.data.frame(seq(0:100))
# names(pred_q50) = "age"
# interval_q50 <- as_tibble(predFit(curve_q50, newdata = pred_q50, interval = "confidence", level= 0.95)) %>% 
#   mutate(age = pred_q50$age)
# interval_q50$lwr[which(interval_q50$lwr<0)]=0
# interval_q50$sd = interval_q50$upr - interval_q50$fit
# 
# tmp_q50 = summary_q50[,c("age","mean_q50")]
# tmp_q50 = tmp_q50[which(tmp_q50$age %in% c(1:40,300))]
# tmp_q50$age[which(tmp_q50$age == 300)] = 60
# 
# 
# #########
# # q25
# summary_q25 = data_tmp[, by=.(age),#ecoregion_1km_mane,
#                        .(mean_q25 = mean(q25_agb ,na.rm=TRUE),
#                          nrow = length(q25_agb))]
# 
# 
# 
# A_q25 = max(data_tmp$q25_agb[which(data_tmp$age == 32)])
# min_A_q25 = min(data_tmp$q25_agb[which(data_tmp$age == 32)])
# median_q25 = median(data_tmp$median_agb[which(data_tmp$age == 32)])
# 
# # A_q25 = 1250
# # min_A_q25 = 725
# 
# 
# age_tmp = matrix (c(300,A_q25,101), nrow=1, ncol = 3, byrow = T)
# 
# summary_q25 = rbind(summary_q25, age_tmp, use.names = F)
# 
# tail(summary_q25)
# 
# curve_q25 = nlsLM(mean_q25 ~ A * (1 - exp(-k * age))^m,
#                   start = list(A = A_q25, k=1, m = 1),
#                   lower = c(min_A_q25,.01,.5),
#                   data = summary_q25)
# 
# 
# 
# pred_q25 = as.data.frame(seq(0:100))
# names(pred_q25) = "age"
# interval_q25 <- as_tibble(predFit(curve_q25, newdata = pred_q25, interval = "confidence", level= 0.95)) %>% 
#   mutate(age = pred_q25$age)
# interval_q25$lwr[which(interval_q25$lwr<0)]=0
# interval_q25$sd = interval_q25$upr - interval_q25$fit
# 
# tmp_q25 = summary_q25[,c("age","mean_q25")]
# tmp_q25 = tmp_q25[which(tmp_q25$age %in% c(1:40,300))]
# tmp_q25$age[which(tmp_q25$age == 300)] = 60
# 
# 
# 
# cex.label = 12
# cex.axis = 12
# point_size = 2
# point_size_2 = 2
# 
# 
# ggplot()+
#   ggtitle(list_management[i])+ #"id_name"
#   # xlim(0,60)+
#   ylim(0,1200)+
#   labs(x="Time since disturbance (Years)", y= expression(paste("AGC (Mg C ",ha^-1,")")))+
#   
#   
#   #q95
#   geom_ribbon(aes(x=age, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="green",data=interval_q95[which(interval_q95$age <=50),])+
#   geom_point(aes(x = age,y = mean_q95, fill = "q95"),size=point_size,color='black',alpha=.5,shape=21,data=summary_q95[which(summary_q95$age <=60),])+
#   geom_point(aes(x = age,y = mean_q95, fill = "q95"),size=point_size_2,color='black',shape=21,data=tmp_q95[which(tmp_q95$age ==60),])+
#   geom_line(aes(x = age,y = fit),lwd=1,color="green",data=interval_q95[which(interval_q95$age <=50),])+
#   
#   # #q75
#   # geom_ribbon(aes(x=age, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="green",data=interval_q75[which(interval_q75$age <=50),])+
#   # geom_point(aes(x = age,y = mean_q75, fill = "q75"),size=point_size,color='black',alpha=.5,shape=21,data=summary_q75[which(summary_q75$age <=60),])+
#   # geom_point(aes(x = age,y = mean_q75, fill = "q75"),size=point_size_2,color='black',shape=21,data=tmp_q75[which(tmp_q75$age ==60),])+
#   # geom_line(aes(x = age,y = fit),lwd=1,color="green",data=interval_q75[which(interval_q75$age <=50),])+
#   
#   
#   # #q50
#   # geom_ribbon(aes(x=age, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="black",data=interval_q50[which(interval_q50$age <=50),])+
#   # geom_point(aes(x = age,y = mean_q50, fill = "q50"),size=point_size,color='black',alpha=.5,shape=21,data=summary_q50[which(summary_q50$age <=60),])+
# # geom_point(aes(x = age,y = mean_q50, fill = "q50"),size=point_size_2,color='black',shape=21,data=tmp_q50[which(tmp_q50$age ==60),])+
# # geom_line(aes(x = age,y = fit),lwd=1,lty=2,color="black",data=interval_q50[which(interval_q50$age <=50),])+
# # 
# 
# # #q25
# # geom_ribbon(aes(x=age, ymin=lwr, ymax=upr), alpha=0.2, inherit.aes=F, fill="blue",data=interval_q25[which(interval_q25$age <=50),])+
# # geom_point(aes(x = age,y = mean_q25, fill = "q25"),size=point_size,color='black',alpha=.5,shape=21,data=summary_q25[which(summary_q25$age <=60),])+
# # geom_point(aes(x = age,y = mean_q25, fill = "q25"),size=point_size_2,color='black',shape=21,data=tmp_q25[which(tmp_q25$age ==60),])+
# # geom_line(aes(x = age,y = fit),lwd=1,color="blue",data=interval_q25[which(interval_q25$age <=50),])+
# 
# 
# scale_fill_manual(labels = c("q95"="AGC q95","q50"="AGC q50","q25"="AGC q25"),
#                   values = c("q95"="green","q50"="black","q25"="blue"),
#                   breaks = c("q95", "q50", "q25"))+
#   
#   
#   
#   # scale_x_continuous(breaks=seq(0,120,by=20),
#   #                    labels=c("0"="0","20"="20","40"="40","60"="60","80"="80","100"="100","120"="OGF"))+
#   scale_x_continuous(breaks=seq(0,60,by=10),
#                      labels=c("0"="0","10"="10","20"="20","30"="30","40"="40","50"="50","60"="OGF"))+
#   #scale_fill_manual(labels = c("Concession","Protected","Unprotected"), values = c("red", "green4","orange4"))+
#   guides(shape="none")+
#   theme_classic()+
#   theme(axis.title.y = element_text(size = cex.label),
#         axis.title.x = element_text(size = cex.label),
#         axis.text.y = element_text(size=cex.axis,color = "black",angle = 90,hjust = 0.5),
#         axis.text.x = element_text(size=cex.axis,color = "black"),
#         axis.ticks = element_line(linewidth = .5, linetype = "solid",colour = "black"),
#         legend.direction = "vertical",
#         legend.position = c(0, 1),
#         legend.justification = c(0, 1),
#         legend.box = "Vertical",
#         legend.key.size = unit(0.4, "cm"),
#         legend.background=element_rect(fill = alpha("white", 0)),
#         legend.title = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
#         panel.background = element_rect(colour = "black", linewidth=0))
# 
# 
# ## end 
# 
