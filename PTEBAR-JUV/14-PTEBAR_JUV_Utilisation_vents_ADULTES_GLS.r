source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")
require(aniMotum)
require(patchwork)

ad_gls <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_2008-2009_clean_from_Audrey.txt",
                     h = T,
                     sep = "\t")
head(ad_gls)
table(ad_gls$STATUT)

ad_nr <- ad_gls[ad_gls$STATUT == "NR", ]
ad_nr$DATE <- as.POSIXct(ad_nr$DATE,
                         format = "%d/%m/%Y %H:%M")

summary(ad_nr$DATE)
table(month(ad_nr$DATE)) # from march to september

#### ---- spatial object ---- ####
xy <- ad_nr[,c('LON', 'LAT')]
ad_nr_sp <- SpatialPointsDataFrame(coords = xy,
                                 data = ad_nr,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
ad_nr_UTM <- spTransform(ad_nr_sp,
                         CRS('+init=epsg:32743'))
#### ---- Objectif: recuperation des points allant de la colonie vers la zone d'hivernage ---- ####
# Strategie:
# Production du kernel 50 par individu
# Recuperation de la date de premiere entree dans ce kernel
# Recuperation de tous les points antecedents

ls_sp <- split(ad_nr_UTM,
               ad_nr_UTM$ID)

pts_out_ls <- lapply(ls_sp, function(x) {
    # production kernel
    KUD_a <- kernelUD(x,
                      h = 'href')
    KUDvol_a <- getvolumeUD(KUD_a)
    ver50_a <- getverticeshr(KUDvol_a, 50)
    
    # conversion objet sf
    hr50_sf <- st_as_sf(ver50_a)
    x_sf <- st_as_sf(x)
    
    # recuperation des points dans le polygone
    inters <- st_intersects(x_sf,
                            hr50_sf)
    pts_in <- x_sf[lengths(inters) != 0, ]
    
    # recuperation de la date min
    date_min <- min(pts_in$DATE)
    
    # recuperation des points en dehors polygone lors de l'aller
    pts_out <- x_sf[x_sf$DATE < date_min,]
    
    print(mapview(list(hr50_sf, x_sf, pts_out)))
    pts_out
})
# interessant, tous les trajets par le sud, souvent dépasse la zone à l'est pour y revenir

# conversion en DF
pts_out <- do.call("rbind", pts_out_ls)
head(pts_out)

# saveRDS(pts_out,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_Locs_anterieures_zone_hivernage.rds")

#### ---- Extraction des 3 composantes de vent ---- ####

pts_out <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_Locs_anterieures_zone_hivernage.rds")
class(pts_out)
table(month(pts_out$DATE))

# ---- Creation des rasters
env_folder <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2018" 
list_names <- list.files(env_folder,
                         pattern = "2008|2009",
                         full.names = T)

east_ls <- list_names[str_detect(list_names,
                                 pattern = "eastward")]
north_ls <- list_names[str_detect(list_names,
                                 pattern = "northward")]
speed_ls <- list_names[str_detect(list_names,
                                 pattern = "speed")]

wind_speed_stack <- terra::rast(speed_ls)
table(duplicated(time(wind_speed_stack)))
wind_speed_stack <- wind_speed_stack[[!duplicated(time(wind_speed_stack))]]
time(wind_speed_stack)[order(time(wind_speed_stack))] 

wind_north_stack <- terra::rast(north_ls)
table(duplicated(time(wind_north_stack)))
wind_north_stack <- wind_north_stack[[!duplicated(time(wind_north_stack))]]
time(wind_north_stack)[order(time(wind_north_stack))] 

wind_east_stack <- terra::rast(east_ls)
table(duplicated(time(wind_east_stack)))
wind_east_stack <- wind_east_stack[[!duplicated(time(wind_east_stack))]]
time(wind_east_stack)[order(time(wind_east_stack))] 

#######################################
# ---- For WIND speed & orientation ####
########################################

head(pts_out, 50)
pts_out$time_loc_min <- hour(pts_out$DATE)*60 + minute(pts_out$DATE)

# raster aux 6h 
# deb      fin       raster.hour     raster.day
# 21:01 (1261) -> 03:00 (180)  ==> 00:00           J+1 or J depending on bef/aft midnight
# 03:01 (181)  -> 09:00 (540)  ==> 06:00            J
# 09:01 (541)  -> 15:00 (900)  ==> 12:00            J
# 15:01 (901)  -> 21:00 (1260) ==> 18:00            J
pts_out$loc_raster_hour <- NA
pts_out$loc_raster_hour[pts_out$time_loc_min >= 1261 | pts_out$time_loc_min <= 180] <- "00:00:00"
pts_out$loc_raster_hour[pts_out$time_loc_min >= 181 & pts_out$time_loc_min <= 540] <- "06:00:00"
pts_out$loc_raster_hour[pts_out$time_loc_min >= 541 & pts_out$time_loc_min <= 900] <- "12:00:00"
pts_out$loc_raster_hour[pts_out$time_loc_min >= 901 & pts_out$time_loc_min <= 1260] <- "18:00:00"

pts_out$loc_raster_date <- ifelse(pts_out$time_loc_min >= 1261,
                           as.character(date(pts_out$DATE)+1),
                           as.character(date(pts_out$DATE)))

pts_out$loc_raster_layer <- paste(pts_out$loc_raster_date,
                               pts_out$loc_raster_hour,
                               sep = ' ')


pts_out_list <- split(pts_out,
                      pts_out$loc_raster_layer)
length(pts_out_list)

########## !!!!!!!!! DEBEUG ICI AVEC CONVERSION DE PTS_OUT DE UTM VERS LATLON
fl3 <- lapply(pts_out_list, function(x) {
# ----- extraction at location #
    speed_raster <- wind_speed_stack[[str_which(as.character(time(wind_speed_stack)),
                                       unique(x$loc_raster_layer))]]
    
    north_raster <- wind_north_stack[[str_which(as.character(time(wind_north_stack)),
                                       unique(x$loc_raster_layer))]]
    
    east_raster <- wind_east_stack[[str_which(as.character(time(wind_east_stack)),
                                       unique(x$loc_raster_layer))]]
# ----- fusion with df #
    wind_speed_extr <- terra::extract(speed_raster,
                                      terra::vect(x[, c("LON", "LAT")]))
    wind_north_extr <- terra::extract(north_raster,
                            terra::vect(x[, c("LON", "LAT")]))
    wind_east_extr <- terra::extract(east_raster,
                           terra::vect(x[, c("LON", "LAT")]))
    
    # ----- #
    x$wind_speed_loc <- wind_speed_extr[, 2]
    x$wind_north_loc <- wind_north_extr[, 2]
    x$wind_east_loc <- wind_east_extr[, 2]

# ---- #
    print(unique(x$loc_raster_layer))
# ----- #
    x
})
print("ayééé")
