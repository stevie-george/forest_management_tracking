library(pacman) # install.packages("pacman")
p_load(terra,sf,raster,fst,ggplot2,ggpubr,dplyr,viridis,ggpointdensity,BIOMASS,collapse,xgboost)
##############################################################################################################################
# system("aws s3 sync --profile ctrees s3://ctrees-input-data/jrc_deforestation_degradation/ ~/lb_sagang/jrc_defor_degrad")
list_jrc <- list.files(path = "/Users/stephaniegeorge/Downloads/jrc_mexico", pattern = "\\.tif$", full.names = TRUE)
list_tree_cov = list.files(path = "E:/GABON/Data_Gabon/DATA/Rasters/optic_radar/30m/hansen_congo_basin/age_treeloss", pattern = "\\.tif$", full.names = TRUE)

agb_aoi = rast("/Users/stephaniegeorge/Documents/ctrees/Data/Maps/Mexico_biomass_100m_2020.tif")

shp_africa = vect("E:/GABON/Data_Gabon/Maps/Africa_Boundaries/Africa_Boundaries.shp")
cb_countries = c('Cameroon','Equatorial Guinea','Central African Republic',
                 'Gabon','Democratic Republic of the Congo','Republic of Congo')
shp_id = shp_africa[which(shp_africa$NAME_0 %in% c(cb_countries))]
plot(shp_id)

shp_id = vect(ext(-97.85819777436336, -95.00175246186336,  15.635267239745588, 17.954236679584614))
plot(shp_id)

grd_lrg <- st_make_grid(shp_id, cellsize = c(.5,.5)) %>% #
  st_set_crs(4326) %>%
  st_transform("epsg:4326")

grd_lrg = vect(grd_lrg)
crs(grd_lrg) = "epsg:4326"
plot(grd_lrg)
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
  extracted_values <- extract(raster,polygons)
  
  # Compute quantiles for each polygon
  quantiles <- apply(extracted_values, 1, function(x) {
    quantile(x, probs, na.rm = TRUE)
  })
  
  # Return a data frame with quantiles
  quantiles_df <- data.frame(quantiles)
  return(quantiles_df)
}

list_def = list_jrc[grep("Def", list_jrc)]
list_deg = list_jrc[grep("Deg", list_jrc)]


sf_use_s2(FALSE)


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
      write.fst(data_def_j,paste0('/Users/stephaniegeorge/temp/deforestation_tile_',tile,".fst"))
      
    }
    
    # # list Degradation
    # filt_degradation <- list()
    # for (i in 1:length(list_deg)) {
    #   ras_i = rast(list_deg[i])
    #   poly_ras_i = as.polygons(ext(ras_i))
    #   poly_overlap = terra::intersect(poly_ras_i,poly_tile_i)

    #   if (is.null(dim(poly_overlap)[1])) {next}
    #   if (dim(poly_overlap)[1] >0) {

    #     ras_i = crop(ras_i,poly_tile_i,mask=TRUE)

    #     filt_degradation[[length(filt_degradation) + 1]] <- ras_i}

    #   # print(paste0(" ------------Tile------------- ",i," / ",length(list_deg)))
    # }
    # if(length(filt_degradation)==1){ras_degrad_i = filt_degradation[[1]]}
    # if(length(filt_degradation)>1){ras_degrad_i <- do.call(terra::merge, filt_degradation)}
    # names(ras_degrad_i)="degradation_year"
    # ras_degrad_i[ras_degrad_i %in% c(0,2021:2022)] = NA
    
    # if(length(freq(ras_degrad_i)$value) >1){
    # # create polygons
    # poly_deg_j = as.polygons(ras_degrad_i, dissolve=TRUE,na.rm=TRUE)
    # crs(poly_deg_j) = "epsg:4326"

    # # add poly area values
    # poly_deg_j$area_ha = expanse(poly_deg_j, unit="ha")

    # # add mean AGB values
    # agb_mean = terra::extract(agb_aoi,poly_deg_j,fun='mean',na.rm=TRUE)
    # poly_deg_j$mean_agb = agb_mean[,2]
    
    # agb_median = terra::extract(agb_aoi,poly_deg_j,fun='median',na.rm=TRUE)
    # poly_deg_j$median_agb = agb_median[,2]
    # # add sd AGB values
    # agb_sd = terra::extract(agb_aoi,poly_deg_j,fun = 'sd',na.rm=TRUE)
    # poly_deg_j$sd_agb = agb_sd[,2]
    
    # agb_q25 = get_quantile(agb_aoi,poly_deg_j,.25)
    # poly_deg_j$q25_agb = agb_q25
    
    # agb_q75 = get_quantile(agb_aoi,poly_deg_j,.75)
    # poly_deg_j$q75_agb = agb_q75
    
    # agb_q95 = get_quantile(agb_aoi,poly_deg_j,.95)
    # poly_deg_j$q95_agb = agb_q95

    # # add lat long
    # sf_poly_deg_j = st_as_sf(poly_deg_j)
    # centoid_poly = st_centroid(sf_poly_deg_j)[,1]
    # centoid_poly = as.data.frame(st_coordinates (centoid_poly))

    # poly_deg_j$lat = centoid_poly$Y
    # poly_deg_j$lon = centoid_poly$X

    # data_deg_j = as.data.frame(poly_deg_j)
    # # data_deg_all_list[[i]] = as.data.frame(poly_deg_i)
    # write.fst(data_deg_j,paste0(getwd(),tile,".fst"))
    # }
    
    # ###############################
    # # tree cov loss
    # filt_tree_cov <- list()
    # for (i in 1:length(list_tree_cov)) {
    #   ras_i = rast(list_tree_cov[i])
    #   poly_ras_i = as.polygons(ext(ras_i))
    #   poly_overlap = terra::intersect(poly_ras_i,poly_tile_i)
    #   
    #   if (is.null(dim(poly_overlap)[1])) {next}
    #   if (dim(poly_overlap)[1] >0) {
    #     
    #     ras_i = crop(ras_i,poly_tile_i,mask=TRUE)
    #     
    #     filt_tree_cov[[length(filt_tree_cov) + 1]] <- ras_i}
    #   
    #   # print(paste0(" ------------Tile------------- ",i," / ",length(list_tree_cov)))
    # }
    # if(length(filt_tree_cov)==1){ras_tree_cov_i = filt_tree_cov[[1]]}
    # if(length(filt_tree_cov)>1){ras_tree_cov_i <- do.call(terra::merge, filt_tree_cov)}
    # names(ras_tree_cov_i)="tree_loss_year"
    # ras_tree_cov_i[ras_tree_cov_i %in% c(0,21)] = NA
    # 
    # # create polygons
    # poly_tree_cov_j = as.polygons(ras_tree_cov_i, dissolve=TRUE,na.rm=TRUE)
    # crs(poly_tree_cov_j) = "epsg:4326"
    # 
    # # add poly area values
    # poly_tree_cov_j$area_ha = expanse(poly_tree_cov_j, unit="ha")
    # 
    # # add mean AGB values
    # agb_mean = terra::extract(agb_aoi,poly_tree_cov_j,fun='mean',na.rm=TRUE)
    # poly_tree_cov_j$agb = agb_mean[,2]
    # # add sd AGB values
    # agb_sd = terra::extract(agb_aoi,poly_tree_cov_j,fun = 'sd',na.rm=TRUE)
    # poly_tree_cov_j$sd_agb = agb_sd$sd_agb
    # 
    # # add lat long
    # sf_poly_tree_cov_j = st_as_sf(poly_tree_cov_j)
    # centoid_poly = st_centroid(sf_poly_tree_cov_j)[,1]
    # centoid_poly = as.data.frame(st_coordinates (centoid_poly))
    # 
    # poly_tree_cov_j$lat = centoid_poly$Y
    # poly_tree_cov_j$lon = centoid_poly$X
    # 
    # data_tree_cov_j = as.data.frame(poly_tree_cov_j)
    # # data_tree_cov_all_list[[i]] = as.data.frame(poly_tree_cov_i)
    # write.fst(data_tree_cov_j,paste0("F:/africa_analysis/treecov_loss_extract/treecov_loss_tile_",tile,".fst"))
    
    print(paste0(" ------------Tile------------- ",tile," / ",length(grd_lrg)))
  }
  
}

#### part 2


######### deforestation
list_df_def = list.files(path = '/Users/stephaniegeorge/temp/', pattern = ".fst*", full.names = TRUE)
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


data_def_all$age = 2020 - data_def_all$deforestation_year



require(data.table)
data_def_all = as.data.table(data_def_all)
# data_def_all_filt$age = as.factor(data_def_all_filt$age)
data_def_summary_management = data_def_all[, by=.(age),#ecoregion_1km_mane,
                                       .(mean_agb = mean(mean_agb ,na.rm=TRUE),
                                         median_agb = mean(median_agb ,na.rm=TRUE),
                                         q25_agb = mean(q25_agb ,na.rm=TRUE),
                                         q75_agb = mean(q75_agb ,na.rm=TRUE),
                                         q95_agb = mean(q95_agb ,na.rm=TRUE),
                                         nrow = length(mean_agb))]



p = ggplot()+
  #ylim(0,200)+
  geom_point(aes(x=age,y=q95_agb),data = data_def_summary_management[,c("age","q95_agb")],color="red")+ #,"ecoregion_1km_mane"
  geom_point(aes(x=age,y=q75_agb),data = data_def_summary_management[,c("age","q75_agb")],color="green4")+ #,"ecoregion_1km_mane"
  # geom_point(aes(x=age,y=mean_agb),data = data_def_summary_management[,c("age","mean_agb","management_name")],color="chartreuse")+ #,"ecoregion_1km_mane"
  geom_point(aes(x=age,y=median_agb),data = data_def_summary_management[,c("age","median_agb")],color="cyan")+ #,"ecoregion_1km_mane"
  geom_point(aes(x=age,y=q25_agb),data = data_def_summary_management[,c("age","q25_agb")],color="orange")+ #,"ecoregion_1km_mane"
  # facet_grid(ecoregion_1km_mane ~ management_name)+
  #facet_wrap( ~ management_name)+
  theme_classic()


plot (p)
