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
pts_out_utm <- do.call("rbind", pts_out_ls)
head(pts_out_utm)
class(pts_out_utm) # class SF - UTM

# conversion de class SF - UTM vers class SF latlon
pts_out <- st_transform(pts_out_utm,
                        crs = "+proj=longlat +datum=WGS84")
mapview(pts_out)

# saveRDS(pts_out,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_Locs_anterieures_zone_hivernage.rds")

#### ---- Extraction des 3 composantes de vent ---- ####

pts_out <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_Locs_anterieures_zone_hivernage.rds")
class(pts_out)
table(month(pts_out$DATE)) # mars avril mai

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
# time(wind_speed_stack)[order(time(wind_speed_stack))] 

wind_north_stack <- terra::rast(north_ls)
table(duplicated(time(wind_north_stack)))
wind_north_stack <- wind_north_stack[[!duplicated(time(wind_north_stack))]]
# time(wind_north_stack)[order(time(wind_north_stack))] 

wind_east_stack <- terra::rast(east_ls)
table(duplicated(time(wind_east_stack)))
wind_east_stack <- wind_east_stack[[!duplicated(time(wind_east_stack))]]
# time(wind_east_stack)[order(time(wind_east_stack))] 

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

# creation du DF
pts_out_wind <- do.call("rbind", fl3)

# formatage des variables vents
summary(pts_out_wind)

pts_out_wind$wind_spd_loc_km_h <- pts_out_wind$wind_speed_loc* 3.6 # conversion wind speed en km/h
summary(pts_out_wind$wind_spd_loc_km_h) # COMPARER AVEC LES BEBES

# calcul orientation vent
# wind_dir <- 180 * atan2(v, u) / pi # with u = east and v = north AND atan2 gives direction in radian, then *180/pi allows the conversion in degree from -180 to 180
# In addition, HERE the atan2 gives the angle with METEOROLOGICAL convention
# N = 0 = 360, E = 90, S = 180, W = 270

# for locs
pts_out_wind$wind_meteo_dir_loc <- 180 * atan2(pts_out_wind$wind_north_loc, pts_out_wind$wind_east_loc) / pi # From -180 to 180°

summary(pts_out_wind$wind_meteo_dir_loc)
hist(pts_out_wind$wind_meteo_dir_loc)

pts_out_wind$wind_meteo_dir0_360_loc <- ifelse(pts_out_wind$wind_meteo_dir_loc >= 0,
                                               pts_out_wind$wind_meteo_dir_loc,
                                               360 + pts_out_wind$wind_meteo_dir_loc) # From 0 to 360
hist(pts_out_wind$wind_meteo_dir0_360_loc,
     breaks = seq(0, 360, 5)) # vent du sud

# visualisation des points avec un vectorplot
nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)

rasterVis::vectorplot(raster::stack(raster(wind_east_stack[[month(time(wind_east_stack)) == 4]]),
                                    raster(wind_north_stack[[month(time(wind_north_stack)) == 4]])),
               isField = 'dXY',
               units = "degrees",
               narrows = 1000,
               lwd.arrows = 2,
               aspX = 0.1,
               region = raster(wind_speed_stack[[month(time(wind_speed_stack)) == 4]]),
               at = my_at,
               col.regions = my_cols,
               colorkey = list(labels = list(cex = 1.5),
                               title = list("wind speed (km/h)",
                                            cex = 2,
                                            vjust = 0)),
            #    main = list(label = paste("week # ", i, " 2018", sep = ""),
                        #    cex = 3),
               xlab = list(label = "Longitude", 
                           cex = 2),
               ylab = list(label = "Latitude",
                           cex = 2),
               scales = list(x = list(cex = 1.5),
                             y = list(cex = 1.5))) +
      layer(c(sp.points(as_Spatial(pts_out[month(pts_out$DATE) == 4,]),
                        col = "white",
                        cex = 3,
                        lwd = 3),
              sp.polygons(ne_countries(),
                    col = "#e3d0d0",
                    fill = "#e3d0d0")))

#### ---- Calcul de l'orientation des oiseaux ---- ####
library(trajr)

l <- split(pts_out_wind, pts_out_wind$ID)

dir <- lapply(l, function(x){
    t <- as.data.frame(x)
    # conversion sp object
    t_sp <- SpatialPoints(t[, c("LON", "LAT")],
                          proj4string = CRS("+proj=longlat"))
    # obtention UTM coord
    t_UTM <- spTransform(t_sp,
                         CRS("+init=epsg:32743"))
    # obtention object trajectory
    coord <- data.frame(x = t_UTM$LON,
                        y = t_UTM$LAT,
                        date = as.numeric(t$DATE))
    trj <- TrajFromCoords(coord,
                          timeCol = "date",
                          spatialUnits = "m")
    # obtention des angles
    rad <- TrajAngles(trj,
                      compass.direction = 0)
    deg <- rad*180/pi
    
    x$dir_bird_deg <- c(deg, NA)
    x$x_trj <- trj$x
    x$y_trj <- trj$y
    x
})

dir_DF <- do.call("rbind", dir)
head(dir_DF)

dir_DF$dir_bird_deg0_360 <- ifelse(dir_DF$dir_bird_deg >= 0,
                                dir_DF$dir_bird_deg,
                                360 + dir_DF$dir_bird_deg) # From 0 to 360
hist(dir_DF$dir_bird_deg0_360,
     breaks = seq(0, 360, 5))

#### ---- Difference orientation nd and bird ---- ####
dir_DF$diff_wind_bird <- (dir_DF$dir_bird_deg0_360 - dir_DF$wind_meteo_dir0_360_loc) %% 360

png("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/Wind_bird_diff_orientation/DIFF_bird_wind_ADULTS_go_wintering.png",
        res = 300,
        width = 15,
        height = 20,
        pointsize = 12,
        units = "cm",
        bg = "white")
hist(dir_DF$diff_wind_bird,
     breaks = seq(0, 360, 5),
     freq = F,
     main = "Diff orient° vent & adultes PTEBAR",
     xlab = "angle (°)")
lines(density(dir_DF$diff_wind_bird,
              from = 0,
              to = 360,
              na.rm = T))
dev.off()

# moyenne circulaire de la difference entre orientation ent & oiseaux
library(circular)
ang_circ <- circular(dir_DF$diff_wind_bird,
                     type = "angles",
                     units = "degrees",
                     modulo = "2pi")
mean.circular(ang_circ, na.rm = T) # 222.0148°



#### ---- Pour comparaison avec juveniles ---- ####
loc <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_diff_wind_bird_dir_110max.rds")

png("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/Wind_bird_diff_orientation/COMP_DIFF_bird_wind_ADULTS_JUV_mars_mai.png",
        res = 300,
        width = 30,
        height = 20,
        pointsize = 12,
        units = "cm",
        bg = "white")
par(mfrow = c(1, 2))

hist(dir_DF$diff_wind_bird,
     breaks = seq(0, 360, 5),
     freq = F,
     main = "Diff orient° vent & adultes PTEBAR",
     xlab = "angle (°)")
lines(density(dir_DF$diff_wind_bird,
              from = 0,
              to = 360,
              na.rm = T))

hist(loc$diff_wind_bird_loc[month(loc$date) %in% 3:5],
     breaks = seq(0, 360, 5),
     freq = F,
     main = "Diff orient° vent & juveniles PTEBAR",
     xlab = "angle (°)")  
lines(density(loc$diff_wind_bird_loc[month(loc$date) %in% 3:5],
              from = 0,
              to = 360,
              na.rm = T))
graphics.off()



x11()
ggplot(dat,
       aes(x = diff_wind_bird_loc)) +
  geom_histogram() +
  coord_polar() +
  scale_x_continuous(limits = c(0,360),
                     breaks = seq(0, 360, by = 45),
                     minor_breaks = seq(0, 360, by = 15))
  
  
  
  
  
  
  
  
  library(openair)
  library(viridis)
  argos_2018 <- loc[loc$group %in% c("2018-Nord", "2018-Sud"), ]
  argos_2018$wind_fake <- 1 
  
  png("C:/Users/ccjuhasz/Desktop/test_WR.png",
        res = 600,
        width = 20,
        height = 20,
        pointsize = 12,
        units = "cm",
        bg = "white")
  
  windRose(mydata = argos_2018,
         wd = "diff_wind_bird_loc",
         ws = "wind_fake",
         angle = 5,
         auto.text = F,
         paddle = F,
         annotate = F,
         col = "olivedrab3")
  dev.off()
  
  

    png("C:/Users/ccjuhasz/Desktop/test_WR.png",
        res = 600,
        width = 20,
        height = 20,
        pointsize = 12,
        units = "cm",
        bg = "white")

  dir_DF2 <- data.frame(dir = dir_DF$diff_wind_bird[!is.na(dir_DF$diff_wind_bird)],
                           speed = 1)
  windRose(mydata = dir_DF2,
         wd = "dir",
         ws = "speed",
         angle = 5,
         auto.text = F,
         paddle = F,
         annotate = F,
         col = "olivedrab3")
  dev.off()
  hist(dir_DF2$dir,
       breaks = seq(0, 360, 5))
