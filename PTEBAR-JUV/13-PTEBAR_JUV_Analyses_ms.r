rm(list = ls())
#### ---- Load packages ------ ####
# ------------------------------- #
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")
require(aniMotum)
require(patchwork)


#### ---- Load data ---- ####
# ------------------------- #
argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
head(argos)
dim(argos)
table(argos$Vessel)

#### ---- Data filter ---- ####
# --------------------------- #

ind <- argos[!(argos$Vessel %in% c("166570",
                                   "166571",
                                   "166573",
                                   "162071")), ] # deletion of too short tracks
inds <- droplevels(ind)
dim(inds)
table(inds$Vessel)
table(inds$Vessel, year(inds$deploy))

inds$Class[inds$Class == "U"] <- 3
table(inds$Class)

#### ---- Formatage pour SSM analyses ---- ####
# ------------------------------------------ #
indss <- inds[, c("Vessel", "Date", "Class", "Longitude", "Latitude")]
names(indss) <- c("id", "date", "lc", "lon", "lat")
head(indss)

###################################################
#### ---- PART 1 - move persistence model ---- ####
##################################################
# tri des donnees GPS base sur l'erreur associee aux localisations et une valeur seuil de vitesse
# obtention de donnees "fittees" (re-estimation de chaque loc au pas de temps initial) pour utilisation dans analyses

# max 110 km/h based on GPS data of ADULT PTEBAR
fit_mp <- fit_ssm(indss,
                  vmax = 30.5, # env. 110 km/h
                  ang = NA,
                  model = "mp",
                  time.step = NA, # to obtain only the fitted values
                  control = ssm_control(verbose = 0))

fit_mp
summary(fit_mp)
data_mp <- as.data.frame(grab(fit_mp,
                              what = "fitted"))
summary(data_mp)
class(data_mp)

#### ---- PART 1 - Exploration des sorties de SSM ---- ####
# ------------------------------------------------------- #

# ---- Vsualisation des tracks corrigées et leurs corrections
check <- 0
for(i in fit_mp$id) {
     # png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/Analyses/Figures/SSM_MP_speed_110kmh_tracks/SSM_MP_FITTED_110max_tracks_",
     #          i,
     #          ".png",
     #          sep = ""),
     #     res = 300,
     #     width = 30,
     #     height = 30,
     #     pointsize = 24,
     #     unit = "cm",
     #     bg = "white")
     
     # x11()
     print(plot(fit_mp[fit_mp$id == i,],
                type = 2))
     dev.off()
     
     check <- check + 1
     if(check == length(fit_mp$id)) {
          print("End of the loop")
     }
}

# ---- inspections visuelles des modèles
check <- 0
# for(i in fit_mp$id) {
     ind <- dplyr::filter(fit_mp, id == i)
     res.rw <- osar(ind)
# x11()
# png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/Analyses/Figures/SSM_MP_speed_110kmh_residuals/SSM_MP_FITTED_110max_",
#               i,
#               ".png",
#               sep = ""),
#     res = 300,
#     width = 30,
#     height = 30,
#     pointsize = 12,
#     unit = "cm",
#     bg = "transparent")

print((plot(res.rw, type = "ts") | plot(res.rw, type = "qq")) / 
  (plot(res.rw, type = "acf") | plot_spacer()))
dev.off()

     check <- check + 1
     print(check)
     if(check == length(fit_mp$id)) {
          print("End of the loop")
     }
# }

# ---- Save data
# saveRDS(data_mp,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_MP_110max.rds")

##### ---- EXPLORATION ZONE - START ---- #####
# max 60 km/h
# fit_mp1 <- fit_ssm(indss,
#                   vmax = 17, # gannet speed = 61 km/h
#                   ang = NA,
#                   model = "mp",
#                   time.step = NA, # to obtain only the fitted values
#                   control = ssm_control(verbose = 0))

# fit_mp1
# summary(fit_mp1)
# data_mp1 <- as.data.frame(grab(fit_mp1,
#                                what = "fitted"))
# data_mp1$values <- "fitted"

# x11(); plot(data_mp1$lon[data_mp1$id == 166568],
#             data_mp1$lat[data_mp1$id == 166568],
#             type = "b",
#             asp = 1)

# saveRDS(data_mp1,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data.rds")

####
# max 60 km/h with reconstruction path (time step = c(1, 3, 6, 12, 24) hours)
# ==> time step = 24
# fit_pred24 <- fit_ssm(indss,
#                       vmax = 17, # gannet speed = 61 km/h
#                       ang = NA,
#                       model = "mp",
#                       time.step = 24, # predicted values at each 24h
#                       control = ssm_control(verbose = 0))
# data_pred24 <- as.data.frame(grab(fit_pred24,
#                                   what = "predicted"))
# data_pred24$values <- "predicted-24h"

# ==> time step = 12
# fit_pred12 <- fit_ssm(indss,
#                       vmax = 17, # gannet speed = 61 km/h
#                       ang = NA,
#                       model = "mp",
#                       time.step = 12, # predicted values at each 12h
#                       control = ssm_control(verbose = 0))
# data_pred12 <- as.data.frame(grab(fit_pred12,
#                                   what = "predicted"))
# data_pred12$values <- "predicted-12h"

# ==> time step = 6
# fit_pred6 <- fit_ssm(indss,
#                      vmax = 17, # gannet speed = 61 km/h
#                      ang = NA,
#                      model = "mp",
#                      time.step = 6, # predicted values at each 6h
#                      control = ssm_control(verbose = 0))
# data_pred6 <- as.data.frame(grab(fit_pred6,
#                                  what = "predicted"))
# data_pred6$values <- "predicted-6h"

# ==> time step = 3
# fit_pred3 <- fit_ssm(indss,
#                      vmax = 17, # gannet speed = 61 km/h
#                      ang = NA,
#                      model = "mp",
#                      time.step = 3, # predicted values at each 3h
#                      control = ssm_control(verbose = 0))
# data_pred3 <- as.data.frame(grab(fit_pred3,
#                                  what = "predicted"))
# data_pred3$values <- "predicted-3h"

# ==> time step = 1
# fit_pred1 <- fit_ssm(indss,
#                      vmax = 17, # gannet speed = 61 km/h
#                      ang = NA,
#                      model = "mp",
#                      time.step = 1, # predicted values at each 1h
#                      control = ssm_control(verbose = 0))
# data_pred1 <- as.data.frame(grab(fit_pred1,
#                                  what = "predicted"))
# data_pred1$values <- "predicted-1h"

# visualisation
# cols <- viridis(6)
# for (i in unique(data_mp1$id)) {
     
# x11()
# plot(data_pred1$lon[data_pred1$id == i],
#      data_pred1$lat[data_pred1$id == i],
#      type = "b",
#      col = cols[1],
#      asp = 1,
#      main = i) # predicted data with 1h time step
# lines(data_pred3$lon[data_pred3$id == i],
#      data_pred3$lat[data_pred3$id == i],
#      type = "b",
#      col = cols[2],
#      asp = 1) # predicted data with 3h time step
# lines(data_pred6$lon[data_pred6$id == i],
#      data_pred6$lat[data_pred6$id == i],
#      type = "b",
#      col = cols[3],
#      asp = 1) # predicted data with 6h time step
# lines(data_pred12$lon[data_pred12$id == i],
#      data_pred12$lat[data_pred12$id == i],
#      type = "b",
#      col = cols[4],
#      asp = 1) # predicted data with 12h time step
# lines(data_pred24$lon[data_pred24$id == i],
#      data_pred24$lat[data_pred24$id == i],
#      type = "b",
#      col = cols[5],
#      asp = 1) # predicted data with 24h time step
# lines(data_mp1$lon[data_mp1$id == i],
#      data_mp1$lat[data_mp1$id == i],
#      type = "b",
#      col = cols[6],
#      asp = 1) # fitted data with the samle time step of original data

# legend("bottomright",
#        legend = c("fitted",
#                   "predicted - 24h",
#                   "predicted - 12h",
#                   "predicted - 6h",
#                   "predicted - 3h",
#                   "predicted - 1h"),
#        pch = 1,
#        col = rev(cols))
# }
# saveRDS(list(data_mp1,
#              data_pred24,
#              data_pred12,
#              data_pred6,
#              data_pred3,
#              data_pred1),
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_aniMotum_fitted_predicted_locs.rds")
##### ---- EXPLORATION ZONE - END ---- #####


#### ---- PART 2 -  Calcul des vitesses à partir de BD à 110km/h max ---- ####
# -------------------------------------------------------------------------- #
data_mp <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_MP_110max.rds")
summary(data_mp)
class(data_mp)
x11(); map(fit_mp, what = "fitted")

# ---- conversion en sf object
data_mp_sf <- st_as_sf(data_mp,
                       coords = c("lon", "lat"),
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
mapview(data_mp_sf, zcol = "id")

# ---- calcul des vitesses
data_ls <- split(data_mp_sf,
                 data_mp_sf$id)
length(data_ls)

# ---- distance & speed computation
speed_ls <- lapply(data_ls,
                   function(x) {
                       diag <- diag(st_distance(x)[, -1])
                       delay <- diff(x$date)
                       vit_m.min <- as.numeric(diag)/as.numeric(delay)
                       vit_km.h <- vit_m.min * 0.001 * 60
                       
                       df <- data.frame(id = x$id,
                                        date = x$date,
                                        dist_m = c(as.numeric(diag), NA),
                                        delay_min = c(as.numeric(delay), NA),
                                        speed_km.h = c(vit_km.h, NA))
                       df
                       })
# ---- summary of delay
lapply(speed_ls,
       function(x) {
           summary(x$delay/60) # delay in hour
       }) # ==> troisième quartile < 2 h, choix d'utiliser que les valeurs de vitesse avec un delay <= 120 min 


# ---- distribution of speed with a delay < 120 min
lapply(speed_ls,
       function(x) {
           x11()
           barplot(x$speed_km.h[x$delay_min <= 120],
                   main = unique(x$id))
       }) # WARNING - reste encore deux valeurs très élevée de vitesse

# ---- data compilation
speed_df <- do.call("rbind",
                    speed_ls)

data_mp <- left_join(data_mp,
                     speed_df,
                     by = c("id", "date"))
length(data_mp$speed_km.h[data_mp$speed_km.h > 110 & !is.na(data_mp$speed_km.h)])
data_mp$speed_km.h[data_mp$speed_km.h > 110 & !is.na(data_mp$speed_km.h)]
data_mp[data_mp$speed_km.h > 110 & !is.na(data_mp$speed_km.h), ]

length(data_mp$delay_min[data_mp$delay_min > 120])

# ---- Speed treatment - deletion of delay > 120 min
data_mp$speed_km.h_treat <- data_mp$speed_km.h
data_mp$speed_km.h_treat[data_mp$delay_min > 120] <- NA 

summary(data_mp$speed_km.h)
summary(data_mp$speed_km.h_treat)

x11();barplot(data_mp$speed_km.h_treat)

# ---- Retrait de l'outlier - speed_km.h_treat = 159.349
data_mp$speed_km.h_treat[data_mp$speed_km.h_treat > 159] <- NA
summary(data_mp$speed_km.h_treat)
x11();barplot(data_mp$speed_km.h_treat)

##### ---- EXPLORATION ZONE - START ---- #####
#### ---- Calcul des vitesses à partir de BD à 60km/h max ---- ####
# fit60 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data.rds")
# # conversion en sf object
# id_date_coords <- fit60[, c("id", "date", "lon", "lat")]
# fit60_sf <- st_as_sf(fit60,
#                      coords = c("lon", "lat"),
#                      crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# mapview(fit60_sf, zcol = "id")

# calcul des vitesses
# sf_ls <- split(fit60_sf,
#                fit60_sf$id)
# length(sf_ls)
# speed_ls <- lapply(sf_ls,
#                    function(x) {
#                        diag <- diag(st_distance(x)[, -1])
#                        delay <- diff(x$date)
#                        vit_m.min <- as.numeric(diag)/as.numeric(delay)
#                        vit_km.h <- vit_m.min * 0.001 * 60
                       
#                        df <- data.frame(id = x$id,
#                                         date = x$date,
#                                         dist_m = c(as.numeric(diag), NA),
#                                         delay_min = c(as.numeric(delay), NA),
#                                         speed_km.h = c(vit_km.h, NA))
#                        df
#                        })
# inspections numériques & visuelles des vitesses
# lapply(speed_ls,
#        function(x) {
#            summary(x$delay/60) # delay en heure
#        }) #

# lapply(speed_ls,
#        function(x) {
#            x11()
#            par(mfrow = c(1, 2))
#            barplot(x$speed_km.h[x$delay_min <= 120],
#                    main = unique(x$id))
#            barplot(x$speed_km.h,
#                    main = unique(x$id))
#        })
# compilation des données
# sp <- do.call("rbind",
#               speed_ls)
# head(sp)
# dim(sp)

# fit60_sp <- left_join(fit60,
#                       sp,
#                       by = c("id", "date"))
# head(fit60_sp)
##### ---- EXPLORATION ZONE - END ---- #####

#######################################################################
#### ---- PART 3 - extraction des paramètres environnementaux sous les localisations ajustées ---- ####
#######################################################################
# WARNING - UTILISER PACKAGE TERRA ET SF EN PRIORITE ****

# ---- Creation des rasters
env_folder <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R" 
list_names <- list.files(env_folder)
length(list_names)
var <- c( 'SST-', 'CHLO', 'WIND-SPEED', 'WIND-NORTH', 'WIND-EAST')
stack_var_list <- list()

for(j in 1:length(var)){
    v <- list_names[str_detect(list_names, var[j])]
    v_list <- vector()

    for(i in 1:length(v)){
         r <- terra::rast(paste(env_folder, v[i], sep = "/")) # utilisation de TERRA PCKG en priorite
     #    r <- stack(paste(env_folder, v[i], sep = '/'))
        v_list <- c(v_list, r)
    }

# stack_var_list[j] <- stack(v_list)
stack_var_list[j] <- terra::rast(v_list)
names(stack_var_list)[j] <- var[j]
}

length(stack_var_list)
warnings()

names(stack_var_list)


sst_stack <- stack_var_list[[1]]
table(duplicated(time(sst_stack)))

chlo_stack <- stack_var_list[[2]]
table(duplicated(time(chlo_stack)))

wind_speed_stack <- stack_var_list[[3]]
table(duplicated(time(wind_speed_stack)))
wind_speed_stack <- wind_speed_stack[[!duplicated(time(wind_speed_stack))]]
time(wind_speed_stack)[order(time(wind_speed_stack))] 

wind_north_stack <- stack_var_list[[4]]
table(duplicated(time(wind_north_stack)))
wind_north_stack <- wind_north_stack[[!duplicated(time(wind_north_stack))]]
time(wind_north_stack)[order(time(wind_north_stack))] 

wind_east_stack <- stack_var_list[[5]]
table(duplicated(time(wind_east_stack)))
wind_east_stack <- wind_east_stack[[!duplicated(time(wind_east_stack))]]
time(wind_east_stack)[order(time(wind_east_stack))] 


# RANGE TEMPOREL SST: 01-04-2017 12:00 -> 31-01-2019 12:00 - 489 layers
#                chlo: 01-01-2017 -> 31-01-2019 - 761 layers
#                wind_speed: 01-04-2017 -> 01-02-2019 - 1988 layers
#                wind_north: 01-04-2017 -> 01-02-2019 - 1988 layers
#                wind_east: 01-04-2017 -> 01-02-2019 - 1988 layers
# WARNING - range discontinu

# ----- > Method : Split the dataframe by date and apply the extraction by group of coordinates #
#########################################################################################################

###################
# ---- SST & chlo #
###################
length(unique(date(data_mp$date)))
class(data_mp$date)
class(time(sst_stack))

# data_mp$dt <- str_replace_all(as.character(date(data_mp$date)),
#                               '-',
#                               '.')
data_mp$dt <- as.character(date(data_mp$date))

mp_list <- split(data_mp,
                 data_mp$dt)
length(mp_list)

mp_list2 <- lapply(mp_list, function(x){

#     sst_raster <- sst_stack[[str_which(names(sst_stack), unique(x$dt))]]
    sst_raster <- sst_stack[[str_which(as.character(time(sst_stack)),
                                       unique(x$dt))]]
#     chlo_raster <- chlo_stack[[str_which(names(chlo_stack), unique(x$dt))]] 
    chlo_raster <- chlo_stack[[str_which(as.character(time(chlo_stack)),
                                         unique(x$dt))]]

    sst_extr <- terra::extract(sst_raster,
                     terra::vect(x[, c('lon', 'lat')]))
    chlo_extr <- terra::extract(chlo_raster,
                      terra::vect(x[, c('lon', 'lat')]))
    
    x$sst_loc <- sst_extr[, 2]
    x$chlo_loc <- chlo_extr[, 2]
    
   # ----- #
    print(unique(x$dt))
    
    x
})
print("ayéééé")

# ---- From Kelvin to Degree conversion of temperature & temperature explo
mp_2 <- do.call('rbind', mp_list2)
mean(mp_2$sst_loc, na.rm = T)
summary(mp_2$sst_loc)
mp_2$sst_loc_deg <- mp_2$sst_loc - 273.15
summary(mp_2$sst_loc_deg)

# ---- chlo explo
mean(mp_2$chlo_loc, na.rm = T)
summary(mp_2$chlo_loc)

#######################################
# ---- For WIND speed & orientation ####
########################################

head(mp_2, 50)
mp_2$time_loc_min <- hour(mp_2$date)*60 + minute(mp_2$date)

# raster aux 6h 
# deb      fin       raster.hour     raster.day
# 21:01 (1261) -> 03:00 (180)  ==> 00:00           J+1 or J depending on bef/aft midnight
# 03:01 (181)  -> 09:00 (540)  ==> 06:00            J
# 09:01 (541)  -> 15:00 (900)  ==> 12:00            J
# 15:01 (901)  -> 21:00 (1260) ==> 18:00            J
mp_2$loc_raster_hour <- NA
mp_2$loc_raster_hour[mp_2$time_loc_min >= 1261 | mp_2$time_loc_min <= 180] <- "00:00:00"
mp_2$loc_raster_hour[mp_2$time_loc_min >= 181 & mp_2$time_loc_min <= 540] <- "06:00:00"
mp_2$loc_raster_hour[mp_2$time_loc_min >= 541 & mp_2$time_loc_min <= 900] <- "12:00:00"
mp_2$loc_raster_hour[mp_2$time_loc_min >= 901 & mp_2$time_loc_min <= 1260] <- "18:00:00"

mp_2$loc_raster_date <- ifelse(mp_2$time_loc_min >= 1261,
                           as.character(date(mp_2$date)+1),
                           as.character(date(mp_2$date)))

mp_2$loc_raster_layer <- paste(mp_2$loc_raster_date,
                               mp_2$loc_raster_hour,
                               sep = ' ')


mp_2_list <- split(mp_2,
                   mp_2$loc_raster_layer)
length(mp_2_list)


fl3 <- lapply(mp_2_list, function(x) {
# ----- extraction at location #
    speed_raster <- wind_speed_stack[[str_which(as.character(time(wind_speed_stack)),
                                       unique(x$loc_raster_layer))]]
    
    north_raster <- wind_north_stack[[str_which(as.character(time(wind_north_stack)),
                                       unique(x$loc_raster_layer))]]
    
    east_raster <- wind_east_stack[[str_which(as.character(time(wind_east_stack)),
                                       unique(x$loc_raster_layer))]]
# ----- fusion with df #
    wind_speed_extr <- terra::extract(speed_raster,
                            terra::vect(x[, c("lon", "lat")]))
    wind_north_extr <- terra::extract(north_raster,
                            terra::vect(x[, c("lon", "lat")]))
    wind_east_extr <- terra::extract(east_raster,
                           terra::vect(x[, c("lon", "lat")]))
    
    # ----- #
    x$wind_speed_loc <- wind_speed_extr[, 2]
    x$wind_north_loc <- wind_north_extr[, 2]
    x$wind_east_loc <- wind_east_extr[, 2]
# ----- #
# extraction over several pixel
# ----- #
 # ---- zone tampon de 200km autour des locs ---- #
    latlon_sf <- st_as_sf(x,
                          coords = c("lon", "lat"),
                          crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    utm_sf <- st_transform(latlon_sf,
                           crs = 32743) # conversion pour ajout zone tampon en m
    tamp <- st_buffer(utm_sf,
                      dist = 200000) # ajout zone tampon de 200 km (200 000 m)
    latlon_buff <- st_transform(tamp,
                              crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # reconversion en latlon pour extraire du raster, lui aussi en latlon
    
# ---- raster cropping & masking only for vizualisation
#     cr <- terra::crop(speed_raster,
#                       latlon_buff,
#                       snap = "out")
# plot(cr)
# plot(st_geometry(st_transform(utm_sf, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")), add = T)
# plot(st_geometry(latlon_buff), add = T)
# plot(terra::mask(cr, latlon_buff))

# ---- values extraction - ICI independant de crop() & mask()
speed_zone_tamp <- terra::extract(speed_raster,
                                  latlon_buff,
                                  exact = T) # exact = True donne la portion du pixel recouvert
north_zone_tamp <- terra::extract(north_raster,
                                  latlon_buff,
                                  exact = T)
east_zone_tamp <- terra::extract(east_raster,
                                 latlon_buff,
                                 exact = T)

x$wind_speed_200km <- sum(speed_zone_tamp$wind_speed_43*speed_zone_tamp$fraction)/sum(speed_zone_tamp$fraction)
x$wind_north_200km <- sum(north_zone_tamp$northward_wind_43*north_zone_tamp$fraction)/sum(north_zone_tamp$fraction)
x$wind_east_200km <- sum(east_zone_tamp$eastward_wind_43*east_zone_tamp$fraction)/sum(east_zone_tamp$fraction)



# ---- #
    print(unique(x$loc_raster_layer))
# ----- #
    x
})
print("ayééé")

mp_3 <- do.call("rbind",
                fl3)
dim(mp_3)
mean(mp_3$wind_speed_loc, na.rm = T)
summary(mp_3$wind_speed_loc, na.rm = T)

mean(mp_3$wind_speed_200km, na.rm = T)
summary(mp_3$wind_speed_200km, na.rm = T)


# ----- computation of wind speed & direction #
# abs_wind_sp <- sqrt(u^2 + v^2)
# wind_dir <- 180 * atan2(v, u) / pi # with u = east and v = north AND atan2 gives direction in radian, then *180/pi allows the conversion in degree from -180 to 180
# In addition, HERE the atan2 gives the angle with METEOROLOGICAL convention
# N = 0 = 360, E = 90, S = 180, W = 270

# for locs
mp_3$wind_meteo_dir_loc <- 180 * atan2(mp_3$wind_north_loc, mp_3$wind_east_loc) / pi
mp_3$abs_wind_spd_loc <- sqrt(mp_3$wind_east_loc^2 + mp_3$wind_north_loc^2)

# for zone tampon
mp_3$wind_meteo_dir_200km <- 180 * atan2(mp_3$wind_north_200km, mp_3$wind_east_200km) / pi
mp_3$abs_wind_spd_200km <- sqrt(mp_3$wind_east_200km^2 + mp_3$wind_north_200km^2)

head(mp_3)
tail(mp_3)
summary(mp_3$wind_speed_loc * 0.001 * 3600)
summary(mp_3$wind_speed_200km * 0.001 * 3600)

summary(mp_3$abs_wind_spd_loc * 0.001 * 3600)
summary(mp_3$abs_wind_spd_200km * 0.001 * 3600)


# save the database
# saveRDS(mp_3,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_110max.rds")

#########################################################
#### ---- PART 4 - Kernels avec locs corrigees ---- ####
########################################################

#### ---- ADULT GLS
ad_gls <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_2008-2009_clean_from_Audrey.txt",
                     h = T,
                     sep = "\t")
head(ad_gls)
table(ad_gls$STATUT)

ad_nr <- ad_gls[ad_gls$STATUT == "NR", ]

# spatial object
xy <- ad_nr[,c('LON', 'LAT')]
ad_nr_sp <- SpatialPointsDataFrame(coords = xy,
                                 data = ad_nr,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
ad_nr_UTM <- spTransform(ad_nr_sp,
                         CRS('+init=epsg:32743'))
# kernel computation UTM
KUD_a <- kernelUD(ad_nr_UTM,
                  h = 'href')
KUD_a@h # 201454.8 m

KUDvol_a <- getvolumeUD(KUD_a)

ver90_a <- getverticeshr(KUDvol_a, 90)
ver50_a <- getverticeshr(KUDvol_a, 50)
ver25_a <- getverticeshr(KUDvol_a, 25)

mapview(list(ver90_a, ver50_a, ver25_a))

# kernel computation LATLON
KUD_a <- kernelUD(ad_nr_sp,
                  h = 'href')
KUD_a@h # 201454.8 m

KUDvol_a <- getvolumeUD(KUD_a)

ver90_a <- getverticeshr(KUDvol_a, 90)
ver50_a <- getverticeshr(KUDvol_a, 50)
ver25_a <- getverticeshr(KUDvol_a, 25)

mapview(list(ver90_a, ver50_a, ver25_a))

# saveRDS(list(ver90_a, ver50_a, ver25_a),
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/MS_DATA/PTEBAR_AD_GLS_kernels.rds")

#################################################################
#### ---- PART 5 - Direction que prennent les oiseaux  ---- ####
################################################################

loc <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_110max.rds")
dim(loc)
class(loc$date)

library(trajr)

l <- split(loc, loc$id)

dir <- lapply(l, function(x){
    t <- as.data.frame(x)
    # conversion sp object
    t_sp <- SpatialPoints(t[, c("lon", "lat")],
                          proj4string = CRS("+proj=longlat"))
    # obtention UTM coord
    t_UTM <- spTransform(t_sp,
                         CRS("+init=epsg:32743"))
    # obtention object trajectory
    coord <- data.frame(x = t_UTM$lon,
                        y = t_UTM$lat,
                        date = as.numeric(t$date))
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

dir2 <- do.call("rbind", dir)

# ---- save the database
# saveRDS(dir2,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_bird_dir_110max.rds")

#################################################################
#### ---- PART 6 - Difference entre l'orientation des oiseaux et celui du vent  ---- ####
################################################################
loc <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_bird_dir_110max.rds")
names(loc)
head(loc$wind_meteo_dir)
# --------------- #
loc$dir_bird_deg0_360 <- ifelse(loc$dir_bird_deg >= 0,
                                loc$dir_bird_deg,
                                360 + loc$dir_bird_deg)

loc$wind_meteo_dir0_360_loc <- ifelse(loc$wind_meteo_dir_loc >= 0,
                                  loc$wind_meteo_dir_loc,
                                  360 + loc$wind_meteo_dir_loc)

loc$wind_meteo_dir0_360_200km <- ifelse(loc$wind_meteo_dir_200km >= 0,
                                  loc$wind_meteo_dir_200km,
                                  360 + loc$wind_meteo_dir_200km)

# --------------- #
loc$diff_wind_bird_loc <- (loc$dir_bird_deg0_360 - loc$wind_meteo_dir0_360_loc) %% 360
loc$diff_wind_bird_200km <- (loc$dir_bird_deg0_360 - loc$wind_meteo_dir0_360_200km) %% 360

summary(loc$diff_wind_bird_loc) # **** WARNING **** Données circulaires 
summary(loc$diff_wind_bird_200km) # **** WARNING **** Données circulaires 


# ---- visualisation
x11(); par(mfrow = c(3, 3))
hist(loc$diff_wind_bird_loc, breaks = 36, main = "Global", col = "#178a17")
hist(loc$diff_wind_bird_200km, breaks = 36, main = "Global", add = T, col = "#00ddff51")

loc_month <- split(loc, month(loc$date))
for(i in 2:9){
     hist(loc_month[[i]]$diff_wind_bird_loc,
          breaks = 36,
          main = unique(month(loc_month[[i]]$date,
                              label = T,
                              abbr = F)))
     hist(loc_month[[i]]$diff_wind_bird_200km,
          breaks = 36,
          add = T,
          col = "#00ddff51")
}

# ---- Wind orientation distribution at locs
hist(loc$wind_meteo_dir0_360_loc[lubridate::month(loc$date) == 4],
     breaks = 36,
     col = "#e6891f82",
     freq = F)
lines(density(loc$wind_meteo_dir0_360_loc[lubridate::month(loc$date) == 4]),
      lwd = 2,
      col = "sienna3")

# circular mean 
library(circular)
ang_circ <- circular(loc$wind_meteo_dir0_360_loc[lubridate::month(loc$date) == 4],
                     type = "angles",
                     units = "degrees",
                     modulo = "2pi")
mean.circular(ang_circ, na.rm = T) # 168.436°

# ---- Wind orientation distribution at 200km
hist(loc$wind_meteo_dir0_360_200km[lubridate::month(loc$date) == 4],
     breaks = 36,
     col = "#e6891f82",
     freq = F)
lines(density(loc$wind_meteo_dir0_360_200km[lubridate::month(loc$date) == 4]),
      lwd = 2,
      col = "sienna3")

# circular mean 
ang_circ <- circular(loc$wind_meteo_dir0_360_200km[lubridate::month(loc$date) == 4],
                     type = "angles",
                     units = "degrees",
                     modulo = "2pi")
mean.circular(ang_circ, na.rm = T) # 0.161934°

# ---- Bird orientation distribution
hist(loc$dir_bird_deg0_360[lubridate::month(loc$date) == 4],
     breaks = 36,
     freq = F,
     col = "#0baea0aa",
     add = T)
lines(density(loc$dir_bird_deg0_360[lubridate::month(loc$date) == 4]),
      lwd = 2,
      col = "#076864")

# ---- circular mean
ang_circ <- circular(loc$dir_bird_deg0_360[lubridate::month(loc$date) == 4],
                     type = "angles",
                     units = "degrees",
                     modulo = "2pi")
mean.circular(ang_circ, na.rm = T) # 102.6694°



############################## STOP HERE FOR REVISION OF SCRIPT ###############################
################ NEED TO CHECK VALUES FOR 200KM ZONE #####################################
# ----- Creation of group of bird per year and track type
loc$group <- NA
loc$group[loc$id %in% c("162070",
                          "162072",
                          "162073",
                          "166561",
                          "166563")] <- "2018-Nord"
loc$group[loc$id %in% c("166564",
                          "166565")] <- "2018-Sud"
loc$group[loc$id %in% c("166566",
                          "166568",
                          "166569",
                          "166572")] <- "2017"
table(loc$group, useNA = "always")

# ---- Bird orientation distribution for 2018-Nord
x11()
hist(loc$dir_bird_deg0_360[lubridate::month(loc$date) == 4 & loc$group == "2018-Nord"],
     breaks = 36,
     freq = F,
     col = "#0baea0aa")
lines(density(loc$dir_bird_deg0_360[lubridate::month(loc$date) == 4 & loc$group == "2018-Nord"]),
      lwd = 2,
      col = "#076864")

# ---- circular mean
ang_circ <- circular(loc$dir_bird_deg0_360[lubridate::month(loc$date) == 4  & loc$group == "2018-Nord"],
                     type = "angles",
                     units = "degrees",
                     modulo = "2pi")
mean.circular(ang_circ, na.rm = T) # 116.4137°

# ---- 
loc_group <- split(loc, loc$group)

lapply(loc_group, function(x){
    x11()
#     png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/Wind_bird_diff_orientation/DIFF_bird_wind_",
#               unique(x$group),
#               ".png",
#               sep = ""),
#         res = 300,
#         width = 15,
#         height = 20,
#         pointsize = 12,
#         units = "cm",
#         bg = "white")
    
    hist(x$diff_wind_bird[month(x$date) == 4],
         col = "#2812c8ae",
         breaks = 36,
         freq = F,
         main = paste("diff angle bird-wind APRIL", unique(x$group), sep = " "),
         xlab = "Angle (°)")
    lines(density(x$diff_wind_bird[month(x$date) == 4]),
         col = "#041c38",
         lwd = 2)
    
    x11()
#     png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/Wind_bird_diff_orientation/ORIENTATION_bird_wind_",
#               unique(x$group),
#               ".png",
#               sep = ""),
#         res = 300,
#         width = 15,
#         height = 20,
#         pointsize = 12,
#         units = "cm",
#         bg = "white")
    hist(x$wind_meteo_dir0_360[lubridate::month(x$date) == 4],
         breaks = 36,
         col = "#e6891f82",
         freq = F,
         main = paste("angle bird-wind APRIL", unique(x$group), sep = " "),
         xlab = "Angle (°)")
    lines(density(x$wind_meteo_dir0_360[lubridate::month(x$date) == 4]),
          lwd = 2,
          col = "sienna3")
    
    hist(x$dir_bird_deg0_360[lubridate::month(x$date) == 4],
         breaks = 36,
         freq = F,
         col = "#0baea0aa",
         add = T)
    lines(density(x$dir_bird_deg0_360[lubridate::month(x$date) == 4]),
          lwd = 2,
          col = "#076864")
    legend("topright",
           legend = c("wind orientation", "bird orientation"),
           fill = c("#e6891f82", "#0baea0aa"),
           border = c("#e6891f82", "#0baea0aa"),
           bty = "n")
    
    graphics.off()
})


# ---- save the database
# saveRDS(loc,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_diff_wind_bird_dir_110max.rds")

####################################################################
#### ---- PART 6 - Zoom Ouest OI lors départ des oiseaux  ---- ####
################################################################### 
loc <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_diff_wind_bird_dir_110max.rds")
summary(loc)

loc$week_num <- lubridate::isoweek(loc$date)
table(loc$week_num)

# ------------------------------------------ #
#### bird restriction - group 2018_Nord ####
# ------------------------------------------ #
dep_2018N <- loc[loc$group == "2018-Nord", ]
unique(dep_2018N$id)

# vitesse moyenne des oiseaux par semaine
# ---------------------------------------
# moyenne calculée à partir des vitesses obtenues dans un delai de moins de 2 h

l1 <- split(dep_2018N, dep_2018N$week_num)
speed_week_ls <- lapply(l1,
                     function(x) {
                         wk <- unique(x$week_num)
                         bd_spd <- mean(x$speed_km.h_treat,
                                        na.rm = T) # vitesse pour délai < 120 min
                         bd_spd_sd <- sd(x$speed_km.h_treat,
                                        na.rm = T) 
                         df <- data.frame(wk = wk,
                                          bd_spd = bd_spd,
                                          bd_spd_sd = bd_spd_sd)
                         df
                     })

speed_week <- do.call("rbind",
                      speed_week_ls) # vitesse moyenne de tous les oiseaux de 2018N par semaine
x11()
plot(speed_week$wk[-1],
     speed_week$bd_spd[-1],
     typ = "b",
     xlab = "week number",
     ylab = "mean speed of birds 2018 N(km/h)")

# summary(lm(bd_spd ~ wk, data = speed_week))

# ---- Wind roses
library(openair)
x11()
windRose(mydata = dep_2018N[month(dep_2018N$date) == 4,],
         wd = "dir_bird_deg0_360",
         ws = "speed_km.h_treat",
         type = "week_num",
        #  breaks = c(0, 2, 5, 8, 11, 17), # for m/s
        breaks = c(0, 15, 20, 25, 30, 35, 40, 50),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
        #  key.footer = "WSP (m/s)", # for m/s
        key.footer = "bird speed (km/h)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D", direction = -1))

x11()
windRose(mydata = dep_2018N[month(dep_2018N$date) == 4, ],
         wd = "wind_meteo_dir0_360",
         ws = "wind_speed",
         type = "week_num",
        #  breaks = c(0, 2, 5, 8, 11, 17), # for m/s
        breaks = c(0, 15, 20, 25, 30, 35, 40, 50),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
        #  key.footer = "WSP (m/s)", # for m/s
        key.footer = "bird speed (km/h)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D", direction = -1))
# ----> probleme d'orientation sur les graphiques

# ---- Wind extraction dans la zone OI visee
# -------------------------------------------- #
env_folder <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2018" 

# ---- deletion of duplicated rasters
# East wind 2018 #
windE2018_names <- list.files(env_folder, full.names = T)[str_detect(list.files(env_folder), "eastward_wind-2018")]
windE2018 <- terra::rast(windE2018_names)
time(windE2018)[duplicated(time(windE2018))]
wE2018 <- windE2018[[!duplicated(time(windE2018))]]
time(wE2018)[duplicated(time(wE2018))]

# North wind 2018 #
windN2018_names <- list.files(env_folder, full.names = T)[str_detect(list.files(env_folder), "northward_wind-2018")]
windN2018 <- terra::rast(windN2018_names)
time(windN2018)[duplicated(time(windN2018))]
wN2018 <- windN2018[[!duplicated(time(windN2018))]]
time(wN2018)[duplicated(time(wN2018))]

# Speed wind 2018 #
windS2018_names <- list.files(env_folder, full.names = T)[str_detect(list.files(env_folder), "wind_speed-2018")]
windS2018 <- terra::rast(windS2018_names)
time(windS2018)[duplicated(time(windS2018))]
wS2018 <- windS2018[[!duplicated(time(windS2018))]]
time(wS2018)[duplicated(time(wS2018))]

# Bird group 2018 N #
# From S14 to S22
dep_2018N <- as.data.frame((loc[loc$group == "2018-Nord" & loc$week_num %in% 14:22, ]))
summary(dep_2018N); dim(dep_2018N)
unique(dep_2018N$id)
unique(dep_2018N$week_num)
col_pt <- c("#ebed7f", "#a0ed7f", "#7fedde", "#7fa9ed", "#ed7fe4")
col_pt_df <- data.frame(id = unique(dep_2018N$id),
                        col_pt = col_pt)
col_tck <- c("#eeefb6d2", "#c8edb8", "#d0f3ee", "#c3d5f1", "#eecaeb")

dep_2018N <- left_join(dep_2018N,
                       col_pt_df,
                       by = "id")
dep_2018N <- left_join(dep_2018N,
                       data.frame(id = unique(dep_2018N$id),
                                  col_tck = col_tck),
                       by = "id")

wk_ls <- split(dep_2018N, dep_2018N$week_num)

pt_ls <- lapply(wk_ls,
                function(x){
                    SpatialPointsDataFrame(coords = x[, c("lon", "lat")],
                                           data = x,
                                           proj4string = CRS("+init=epsg:4326"))
                })

tck_ls <- lapply(wk_ls,
                 function(x){
                     argos_sp <- SpatialPointsDataFrame(coords = x[, c("lon", "lat")],
                                           data = x,
                                           proj4string = CRS("+init=epsg:4326"))
                     argos_sp_list <- split(argos_sp, argos_sp$id)
                     track <- lapply(argos_sp_list,
                                     function(x) {
                                         Lines(list(Line(coordinates(x))),
                                               x$id[1L])
                                         })
                     lines <- SpatialLines(track)
                     data <- data.frame(id = unique(argos_sp$id),
                                        wk = unique(x$week_num))
                     data <- left_join(data,
                                       data.frame(id = unique(dep_2018N$id),
                                                  col_tck = col_tck),
                                       by = "id")
                     rownames(data) <- data$id
                     argos_lines <- SpatialLinesDataFrame(lines, data)
                     })

#########################

# ---- #
nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)

# ---- HUGE LOOP ---- #
wind_caracteristic <- data.frame(wk_num = NA,
                                 wd_spd_mean = NA,
                                 wd_spd_sd = NA)
for (i in 10:22){
    print(i)
# BIRD #
bird_pt <- pt_ls[[as.character(i)]]

# track
if (i >= 14) {
t <- as.data.frame(dep_2018N[dep_2018N$week_num %in% 10:i,])
t_sp <- SpatialPointsDataFrame(coords = t[, c("lon", "lat")],
                                           data = t,
                                           proj4string = CRS("+init=epsg:4326"))
tt <- split(t_sp, t_sp$id)
track <- lapply(tt,
                function(x) {
                    Lines(list(Line(coordinates(x))),
                          x$id[1L])
                    })
tt_lines <- SpatialLines(track)
} else {
    tt_lines <- NULL
}

# WIND #
extend <- extent(40, 60, -25, 0) # xmin, xmax, ymin, ymax

e <- crop(mean(wE2018[[isoweek(time(wE2018)) == i]]),
          extend)
n <- crop(mean(wN2018[[isoweek(time(wN2018)) == i]]),
          extend)
s <- crop(mean(wS2018[[isoweek(time(wS2018)) == i]]),
          extend)

wd <- c(i,
        mean(values(s), na.rm = T),
        sd(values(s), na.rm = T))
wind_caracteristic <- rbind(wind_caracteristic, wd)

# ---- #
x11()
# png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/wind_shift_RUN-MADA/RUN-MADA_wind_2018_week_",
#     i,
#     ".png",
#     sep = ""),
#     res = 300,
#     width = 30,
#     height = 40,
#     pointsize = 4,
#     units = "cm",
#     bg = "white")

print(rasterVis::vectorplot(raster::stack(raster(e), raster(n)),
               isField = 'dXY',
               units = "degrees",
               narrows = 600,
               lwd.arrows = 2,
               aspX = 0.1,
               region = raster(s),
               at = my_at,
               col.regions = my_cols,
               colorkey = list(labels = list(cex = 1.5),
                               title = list("wind speed (km/h)",
                                            cex = 2,
                                            vjust = 0)),
               main = list(label = paste("week # ", i, " 2018", sep = ""),
                           cex = 3),
               xlab = list(label = "Longitude", 
                           cex = 2),
               ylab = list(label = "Latitude",
                           cex = 2),
               scales = list(x = list(cex = 1.5),
                             y = list(cex = 1.5))) +
      layer(sp.polygons(ne_countries(),
                    col = "#e3d0d0",
                    fill = "#e3d0d0")))

# ----- #
# x11()
png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/wind_shift_RUN-MADA/RUN-MADA_wind_2018_bird_group_2018N_week_110max_",
    i,
    ".png",
    sep = ""),
    res = 300,
    width = 30,
    height = 40,
    pointsize = 4,
    units = "cm",
    bg = "white")

print(rasterVis::vectorplot(raster::stack(raster(e), raster(n)),
               isField = 'dXY',
               units = "degrees",
               narrows = 600,
               lwd.arrows = 2,
               aspX = 0.1,
               region = raster(s),
               at = my_at,
               col.regions = my_cols,
               colorkey = list(labels = list(cex = 1.5),
                               title = list("wind speed (km/h)",
                                            cex = 2,
                                            vjust = 0)),
               main = list(label = paste("week # ", i, " 2018", sep = ""),
                           cex = 3),
               xlab = list(label = "Longitude",
                           cex = 2),
               ylab = list(label = "Latitude",
                           cex = 2),
               scales = list(x = list(cex = 1.5),
                             y = list(cex = 1.5))) +
layer(c(sp.points(pt_ls[[as.character(i)]],
          col = pt_ls[[as.character(i)]]$col_pt,
          cex = 3,
          lwd = 3),
        sp.lines(tt_lines,
                 col = col_tck,
                 lwd = 3),
        sp.polygons(ne_countries(),
                    col = "#e3d0d0",
                    fill = "#e3d0d0"))
)
)

# ----- #
# x11()
png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/wind_shift_RUN-MADA/RUN-MADA_wind_n_birds_2018_week_110max_",
    i,
    "_angles_for_raster.png",
    sep = ""),
    res = 300,
    width = 20,
    height = 15,
    pointsize = 12,
    units = "cm",
    bg = "white")
par(mfrow = c(1, 2))
ang <- 180 * atan2(n, e) / pi
ang_360 <- ifelse(values(ang) >= 0,
                  values(ang),
                  360 + values(ang))
hist(ang_360,
     breaks = 36,
     main = paste("week # ", i, " 2018", sep = ""),
     xlab = "wind orientation (0°/360°)",
     freq = F,
     xlim = c(0, 360))
lines(density(ang_360, na.rm = T),
      lwd = 2,
      col = "#0882ed")

# circular mean for wind
ang_circ <- circular(ang_360,
                     type = "angles",
                     units = "degrees",
                     modulo = "2pi")
print(mean.circular(ang_circ,
                    na.rm = T))



if(i > 14) {
hist(pt_ls[[as.character(i)]]$dir_bird_deg0_360,
     breaks = 36,
     main = paste("week # ", i, " 2018", sep = ""),
     xlab = "bird orientation (0°/360°)",
     freq = F,
     xlim = c(0, 360))

lines(density(pt_ls[[as.character(i)]]$dir_bird_deg0_360, na.rm = T),
      lwd = 2,
      col = "#0882ed")}
# graphics.off()
}

# ---- Evolution de la vitesse moyenne des vents dans la zone de Mada de S10 à S22
# ---- Ajout de la vitesse moyenne des oiseaux 2018N sur la même période (speed_week)
x11()
plot(wind_caracteristic$wk_num,
     wind_caracteristic$wd_spd_mean,
     xlab = "week number 2018",
     ylab = "Mean wind speed (m/s)",
     type = "b",
     bty = "n")
text(x = 12.5,
     y = 7.2,
     "wind inversion")
arrows(x0 = 12.5,
       y0 = 4.5,
       x1 = 12.5,
       y1 = 7,
       code = 1)
par(new = T)
plot(speed_week$wk[speed_week$wk %in% 15:22],
     speed_week$bd_spd[speed_week$wk %in% 15:22],
     type = "b",
     xlab = "",
     ylab = "",
     xlim = c(10, 22),
     bty = "n",
     col = "darkorange")

# ---- calcul de la distance parcourue relative par semaine

dep_2018N_sf_latlon <- st_as_sf(dep_2018N,
                         coords = c("lon", "lat"),
                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
mapview(dep_2018N_sf_latlon)

dep_2018N_sf_UTM <- st_transform(dep_2018N_sf_latlon,
                                 crs = 32743)
mapview(dep_2018N_sf_UTM)

# distance
k <- split(dep_2018N_sf_UTM,
           list(dep_2018N_sf_UTM$week_num,
                dep_2018N_sf_UTM$id))

k_list <- lapply(k,
                 function(x) {
                     wk <- unique(x$week_num)
                     id <- unique(x$id)
                     dista <- as.numeric(st_distance(x[1,],
                                          x[dim(x)[1], ]))
                     
                     data.frame(id = id,
                                wk_num = wk,
                                dist_m = dista)
                 })

################
# ---- distance parcourue par les oiseaux par semaine
# en partant du dernier point de la semaine précédente
# et recalcul de la vitesse en partant de cette distance

dist_wk_bd <- data.frame()

for(i in 1:length(k)){
    
    wk <- unique(k[[i]]$week_num)
    id <- unique(k[[i]]$id)
    

    
    if(wk > 14){
            pres <- k[[i]]
            past <- k[[i-1]]
        dista <- st_distance(pres[dim(pres)[1], ],
                             past[dim(past)[1],])
        bd_speed_mean  <- mean(k[[i]]$speed_km.h_treat, na.rm = T)
        bd_speed_sd  <- sd(k[[i]]$speed_km.h_treat, na.rm = T)
        
    } else {
        dista <- NA
        bd_speed_mean <- NA
        bd_speed_sd <- NA
    }
    
    dist_wk_bd <- rbind(dist_wk_bd,
                        c(id, wk, as.numeric(dista), bd_speed_mean, bd_speed_sd))
}


names(dist_wk_bd) <- c("id", "week_num", "dist_m", "bd_spd_mean_km.h", "bd_spd_sd")
dist_wk_bd <- apply(dist_wk_bd,
                    2,
                    as.numeric)
summary(dist_wk_bd)
dist_wk_bd <- as.data.frame(dist_wk_bd)

# ---- mean dist per week
dist_mean_wk <- aggregate(dist_m ~ week_num,
                          data = dist_wk_bd,
                          mean,
                          na.rm = T)

# ---- cumul dist per week
dist_cum_wk <- aggregate(dist_m ~ week_num,
                         data = dist_wk_bd,
                         sum,
                         na.rm = T)
# ---- mean speed of bird per week
spd_brd_mean_wk <- aggregate(bd_spd_mean_km.h ~ week_num,
                             data = dist_wk_bd,
                             mean,
                             na.rm = T)

# ---- VISU
# 1 - comparaison des deux calculs de vitesse
# ---------------------------------------------
plot(speed_week$wk[speed_week$wk %in% 15:22],
     speed_week$bd_spd[speed_week$wk %in% 15:22],
     type = "b",
     xlab = "Week number 2018",
     ylab = "Mean speed bird - computation 1", # original data
     xlim = c(15, 22),
     ylim = c(5, 23),
     bty = "n",
     col = "darkorange")
# arrows(x0 = speed_week$wk[speed_week$wk %in% 15:22],
#        y0 = speed_week$bd_spd[speed_week$wk %in% 15:22],
#        x1 = speed_week$wk[speed_week$wk %in% 15:22],
#        y1 = speed_week$bd_spd[speed_week$wk %in% 15:22] + speed_week$bd_spd_sd[speed_week$wk %in% 15:22],
#        col = "darkorange",
#        code = 0)
lines(spd_brd_mean_wk$week_num,
      spd_brd_mean_wk$bd_spd_mean_km.h) # calcul de vitesse par semaine avec l'objet dist_wk_bd
x11();
plot(dist_mean_wk$week_num,
     dist_mean_wk$dist_m,
     type = "b",
     asp = )
par(new = T)
plot(dist_cum_wk$week_num,
     dist_cum_wk$dist_m,
     type = "b",
     col = "grey")

# 2 - overlap distance parcourue, vitesse moyenne et vitesse du vent dans la zone
# -------------------------------------------------------------------------------

# VISU - distance parcourue en fn vitesse du vent
# ------------------------------------
x11()
par(mar = c(4.1, 4.4, 4.1, 4.4))

plot(dist_mean_wk$week_num,
     dist_mean_wk$dist_m/1000,
     type = "h",
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n",
     xlim = c(10, 22),
     bty = "n",
     lwd = 5,
     col ="#68decc")
axis(side = 4,
     lwd = 1,
     las = 2,
     cex.axis = 1)
mtext("Mean travelled distance (km)",
      side = 4,
      line = 3,
      col = "#68decc")
par(new = T)
plot(wind_caracteristic$wk_num,
     wind_caracteristic$wd_spd_mean,
     xlab = "week number 2018",
     ylab = "",
     type = "b",
     bty = "n",
     xlim = c(10, 22),
     ylim = c(3.5, 14),
     lwd = 3,
     col = "#2b9aa6")
mtext("Mean wind speed (km/h)",
      side = 2,
      line = 3,
      col = "#2b9aa6")
text(x = 12.5,
     y = 12,
     "wind inversion")
arrows(x0 = 12.5,
       y0 = 5,
       x1 = 12.5,
       y1 = 11,
       code = 1)

# VISU - vitesse des oiseaux en fn du vent
# --------------------------------------
x11()
par(mar = c(4.1, 4.4, 4.1, 4.4))

plot(spd_brd_mean_wk$week_num,
     spd_brd_mean_wk$bd_spd_mean_km.h,
     xlab = "week number 2018",
     ylab = "",
     type = "b",
     bty = "n",
     xlim = c(10, 22),
     ylim = c(3.5, 14),
     yaxt = "n",
     xaxt = "n",
     lwd = 3,
     col = "#68decc")
axis(side = 4,
     lwd = 1,
     las = 2,
     cex.axis = 1)
mtext("Mean speed of birds (km/h)",
      side = 4,
      line = 3,
      col = "#68decc")

par(new = T)
plot(wind_caracteristic$wk_num,
     wind_caracteristic$wd_spd_mean,
     type = "b",
     xlab = "",
     ylab = "",
     xlim = c(10, 22),
     ylim = c(3.5, 14),
     bty = "n",
     lwd = 3,
     col = "#2b9aa6")
mtext("Mean wind speed (km/h)",
      side = 2,
      line = 3,
      col = "#2b9aa6")
text(x = 12.5,
     y = 12,
     "wind inversion")
arrows(x0 = 12.5,
       y0 = 5,
       x1 = 12.5,
       y1 = 11,
       code = 1)

# ---- distance oiseaux vs. vitesse vents
plot(wind_caracteristic$wd_spd_mean[wind_caracteristic$wk_num %in% 14:22],
     dist_wk_bd$dist_m[dist_wk_bd$id == "162070"]/1000,
     type = "b")

#### FAIRE LA MEME CHOSE MAIS AVEC DES CROPS DE RASTERS AUTOUR DES LOCS PAR SEMAINE (from 15 to 22) ####
# Zone tampon de 200km et moyenne PONDEREE (si pixel non entier) des pixels couverts par cette zone
dep <- dep_2018N_sf_latlon[dep_2018N_sf_latlon$week_num != 14, ]
table(dep$week_num)


# necessite de moyenner les rasters en fonction 

for(i in 1:length(dep$id)) {
     test <- dep[i, ]
     testUTM <- st_transform(test,
                             crs = 32743) # conversion pour ajout zone tampon en m
     test1 <- st_buffer(testUTM,
                        dist = 200000) # ajout zone tampon de 200 km (200 000 m)
     test1latlon <- st_transform(test1,
                                 crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # reconversion en latlon pour extraire du raster, lui aussi en latlon
     cr <- terra::crop(s, # raster vitesse mais dois coincider avec le point - numero de week
                       test1latlon,
                       snap = "out")
plot(cr)
plot(st_geometry(test), add = T)
plot(st_geometry(test1latlon), add = T)
df <- terra::extract(s,
                     test1latlon,
                     exact = T) # on the way !!!!

m_pond <- sum(df$mean*df$fraction)/sum(df$fraction)
print() # moyenne des valeurs de pixels ponderee par la surface reouverte par le polygone

}


# ll <- split(dep,
#             list(dep$id,
#                  dep$week_num))
# length(ll)
# names(ll)

# dfff <- data.frame(id = NULL,
#                    week = NULL,
#                    spd_mn = NULL,
#                    spd_sd = NULL)

# for(i in 1:length(ll)){

#     test <- terra::crop(s,
#                         ll[[i]],
#                         snap = "out")
#     x11()
#     plot(test,
#          main = paste("ID",
#                       unique(ll[[i]]$id),
#                       " - WEEK",
#                       unique(ll[[i]]$week_num),
#                       sep = ""))
#     plot(st_geometry(ll[[i]]),
#          add = T)
    
#     print(mean(values(test), na.rm = T))
#     dfff <- rbind(dfff,
#                   c(unique(ll[[i]]$id),
#                     unique(ll[[i]]$week_num),
#                     mean(values(test), na.rm = T),
#                     sd(values(test), na.rm = T)))
# }

#### A REFAIRE MAIS AVEC EXTRACT AVEC UNE ZONE TAMPON ####
# EXAMPLE 
pts <- ll[[20]]
plot(st_geometry(pts))

x11(); par(mfrow = c(3, 4))
for(i in 1:12){
     test <- ll[[20]][i, ]
     testUTM <- st_transform(test,
                             crs = 32743)
     test1 <- st_buffer(testUTM,
                        dist = 200000) # 200 km
test1latlon <- st_transform(test1,
                            crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
cr <- terra::crop(s,
                  test1latlon,
                  snap = "out")
plot(cr)
plot(st_geometry(test), add = T)
plot(st_geometry(test1latlon), add = T)
df <- terra::extract(s,
                     test1latlon,
                     exact = T) # on the way !!!!

print(sum(df$mean*df$fraction)/sum(df$fraction)) # moyenne des valeurs de pixels ponderee par la surface reouverte par le polygone

}

# sur toutes les localisations 
################ ***** MAIS FAUT IL LE FAIRE LORS DE L EXTRACTION DES VALEURS AUX 6 HEURES PRES ***** #########################

x11()
plot(data_mp1$lon[data_mp1$id == 162073],
     data_mp1$lat[data_mp1$id == 162073],
     type = "b")
lines(data_mp2$lon[data_mp2$id == 162073],
     data_mp2$lat[data_mp2$id == 162073],
     col = "grey")








































# -------------------------------------------------- #
#### **** bird restriction - group 2018_SUD **** ####
# ------------------------------------------------- #


dep_2018S <- loc[loc$group == "2018-Sud", ]
unique(dep_2018S$id)

# vitesse moyenne des oiseaux par semaine
# ---------------------------------------
# moyenne calculée à partir des vitesses obtenues dans un delai de moins de 2 h

l1 <- split(dep_2018S, dep_2018S$week_num)
speed_week_ls <- lapply(l1,
                     function(x) {
                         wk <- unique(x$week_num)
                         bd_spd <- mean(x$speed_km.h_treat,
                                        na.rm = T) # vitesse pour délai < 120 min
                         bd_spd_sd <- sd(x$speed_km.h_treat,
                                        na.rm = T) 
                         df <- data.frame(wk = wk,
                                          bd_spd = bd_spd,
                                          bd_spd_sd = bd_spd_sd)
                         df
                     })

speed_week_2018S <- do.call("rbind",
                      speed_week_ls) # vitesse moyenne de tous les oiseaux de 2018N par semaine
x11()
plot(speed_week_2018S$wk[-1],
     speed_week_2018S$bd_spd[-1],
     typ = "b",
     xlab = "week number",
     ylab = "mean speed of birds 2018 S(km/h)")

summary(lm(bd_spd ~ wk, data = speed_week_2018S))

# Wind extraction
# -------------- #
env_folder <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2018" 

# East wind 2018 #
windE2018_names <- list.files(env_folder, full.names = T)[str_detect(list.files(env_folder), "eastward_wind-2018")]
windE2018 <- terra::rast(windE2018_names)
time(windE2018)[duplicated(time(windE2018))]
wE2018 <- windE2018[[!duplicated(time(windE2018))]]
time(wE2018)[duplicated(time(wE2018))]

# North wind 2018 #
windN2018_names <- list.files(env_folder, full.names = T)[str_detect(list.files(env_folder), "northward_wind-2018")]
windN2018 <- terra::rast(windN2018_names)
time(windN2018)[duplicated(time(windN2018))]
wN2018 <- windN2018[[!duplicated(time(windN2018))]]
time(wN2018)[duplicated(time(wN2018))]

# Speed wind 2018 #
windS2018_names <- list.files(env_folder, full.names = T)[str_detect(list.files(env_folder), "wind_speed-2018")]
windS2018 <- terra::rast(windS2018_names)
time(windS2018)[duplicated(time(windS2018))]
wS2018 <- windS2018[[!duplicated(time(windS2018))]]
time(wS2018)[duplicated(time(wS2018))]

# Bird group 2018 S #
# From S14 to S22
dep_2018S <- as.data.frame((loc[loc$group == "2018-Sud" & loc$week_num %in% 14:22, ]))
summary(dep_2018S); dim(dep_2018S)
unique(dep_2018S$id)
unique(dep_2018S$week_num)
col_pt <- c("#ebed7f", "#7fedde")
col_pt_df <- data.frame(id = unique(dep_2018S$id),
                        col_pt = col_pt)
col_tck <- c("#eeefb6d2", "#d0f3ee")

dep_2018S <- left_join(dep_2018S,
                       col_pt_df,
                       by = "id")
dep_2018S <- left_join(dep_2018S,
                       data.frame(id = unique(dep_2018S$id),
                                  col_tck = col_tck),
                       by = "id")

wk_ls <- split(dep_2018S, dep_2018S$week_num)

pt_ls <- lapply(wk_ls,
                function(x){
                    SpatialPointsDataFrame(coords = x[, c("lon", "lat")],
                                           data = x,
                                           proj4string = CRS("+init=epsg:4326"))
                })

tck_ls <- lapply(wk_ls,
                 function(x){
                     argos_sp <- SpatialPointsDataFrame(coords = x[, c("lon", "lat")],
                                           data = x,
                                           proj4string = CRS("+init=epsg:4326"))
                     argos_sp_list <- split(argos_sp, argos_sp$id)
                     track <- lapply(argos_sp_list,
                                     function(x) {
                                         Lines(list(Line(coordinates(x))),
                                               x$id[1L])
                                         })
                     lines <- SpatialLines(track)
                     data <- data.frame(id = unique(argos_sp$id),
                                        wk = unique(x$week_num))
                     data <- left_join(data,
                                       data.frame(id = unique(dep_2018S$id),
                                                  col_tck = col_tck),
                                       by = "id")
                     rownames(data) <- data$id
                     argos_lines <- SpatialLinesDataFrame(lines, data)
                     })

#########################

# ---- #
nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)

# ---- HUGE LOOP ---- #
wind_caracteristic_2018S <- data.frame(wk_num = NA,
                                 wd_spd_mean = NA,
                                 wd_spd_sd = NA)
for (i in 10:22){
    print(i)
# BIRD #
bird_pt <- pt_ls[[as.character(i)]]

# track
if (i >= 14) {
t <- as.data.frame(dep_2018S[dep_2018S$week_num %in% 10:i,])
t_sp <- SpatialPointsDataFrame(coords = t[, c("lon", "lat")],
                                           data = t,
                                           proj4string = CRS("+init=epsg:4326"))
tt <- split(t_sp, t_sp$id)
track <- lapply(tt,
                function(x) {
                    Lines(list(Line(coordinates(x))),
                          x$id[1L])
                    })
tt_lines <- SpatialLines(track)
} else {
    tt_lines <- NULL
}

# WIND #
extend <- extent(50, 80, -35, 0) # xmin, xmax, ymin, ymax

e <- crop(mean(wE2018[[isoweek(time(wE2018)) == i]]),
          extend)
n <- crop(mean(wN2018[[isoweek(time(wN2018)) == i]]),
          extend)
s <- crop(mean(wS2018[[isoweek(time(wS2018)) == i]]),
          extend)

wd <- c(i,
        mean(values(s), na.rm = T),
        sd(values(s), na.rm = T))
wind_caracteristic_2018S <- rbind(wind_caracteristic_2018S, wd)

# ---- #
# x11()
png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/wind_shift_RUN-MADA/GROUP_2018S/RUN-MADA_wind_2018_week_",
    i,
    ".png",
    sep = ""),
    res = 300,
    width = 30,
    height = 40,
    pointsize = 4,
    units = "cm",
    bg = "white")

print(rasterVis::vectorplot(raster::stack(raster(e), raster(n)),
               isField = 'dXY',
               units = "degrees",
               narrows = 600,
               lwd.arrows = 2,
               aspX = 0.1,
               region = raster(s),
               at = my_at,
               col.regions = my_cols,
               colorkey = list(labels = list(cex = 1.5),
                               title = list("wind speed (km/h)",
                                            cex = 2,
                                            vjust = 0)),
               main = list(label = paste("week # ", i, " 2018", sep = ""),
                           cex = 3),
               xlab = list(label = "Longitude", 
                           cex = 2),
               ylab = list(label = "Latitude",
                           cex = 2),
               scales = list(x = list(cex = 1.5),
                             y = list(cex = 1.5))) +
      layer(sp.polygons(ne_countries(),
                    col = "#e3d0d0",
                    fill = "#e3d0d0")))

# ----- #
# x11()
png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/wind_shift_RUN-MADA/GROUP_2018S/RUN-MADA_wind_2018_week_",
    i,
    ".png",
    sep = ""),
    res = 300,
    width = 30,
    height = 40,
    pointsize = 4,
    units = "cm",
    bg = "white")

print(rasterVis::vectorplot(raster::stack(raster(e), raster(n)),
               isField = 'dXY',
               units = "degrees",
               narrows = 600,
               lwd.arrows = 2,
               aspX = 0.1,
               region = raster(s),
               at = my_at,
               col.regions = my_cols,
               colorkey = list(labels = list(cex = 1.5),
                               title = list("wind speed (km/h)",
                                            cex = 2,
                                            vjust = 0)),
               main = list(label = paste("week # ", i, " 2018", sep = ""),
                           cex = 3),
               xlab = list(label = "Longitude",
                           cex = 2),
               ylab = list(label = "Latitude",
                           cex = 2),
               scales = list(x = list(cex = 1.5),
                             y = list(cex = 1.5))) +
layer(c(sp.points(pt_ls[[as.character(i)]],
          col = pt_ls[[as.character(i)]]$col_pt,
          cex = 3,
          lwd = 3),
        sp.lines(tt_lines,
                 col = col_tck,
                 lwd = 3),
        sp.polygons(ne_countries(),
                    col = "#e3d0d0",
                    fill = "#e3d0d0"))
)
)

# ----- #
# x11()
png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/wind_shift_RUN-MADA/GROUP_2018S/RUN-MADA_wind_2018_week_",
    i,
    "_angles_for_raster.png",
    sep = ""),
    res = 300,
    width = 20,
    height = 15,
    pointsize = 12,
    units = "cm",
    bg = "white")
par(mfrow = c(1, 2))
ang <- 180 * atan2(n, e) / pi
ang_360 <- ifelse(values(ang) >= 0,
                  values(ang),
                  360 + values(ang))
hist(ang_360,
     breaks = 36,
     main = paste("week # ", i, " 2018", sep = ""),
     xlab = "wind orientation (0°/360°)",
     freq = F,
     xlim = c(0, 360))
lines(density(ang_360, na.rm = T),
      lwd = 2,
      col = "#0882ed")
if(i > 14) {
hist(pt_ls[[as.character(i)]]$dir_bird_deg0_360,
     breaks = 36,
     main = paste("week # ", i, " 2018", sep = ""),
     xlab = "bird orientation (0°/360°) - 2018 S",
     freq = F,
     xlim = c(0, 360))

lines(density(pt_ls[[as.character(i)]]$dir_bird_deg0_360, na.rm = T),
      lwd = 2,
      col = "#0882ed")}
graphics.off()
}

# Evolution de la vitesse moyenne des vents dans la zone
# Ajout de la vitesse moyenne des oiseaux 2018S sur la même période (speed_week)
x11()
plot(wind_caracteristic_2018S$wk_num,
     wind_caracteristic_2018S$wd_spd_mean,
     xlab = "week number 2018",
     ylab = "Mean wind speed (m/s)",
     type = "b",
     bty = "n")
text(x = 12.5,
     y = 8,
     "wind inversion")
arrows(x0 = 12.5,
       y0 = 6.25,
       x1 = 12.5,
       y1 = 7.75,
       code = 1)
par(new = T)
plot(speed_week_2018S$wk[speed_week_2018S$wk %in% 15:22],
     speed_week_2018S$bd_spd[speed_week_2018S$wk %in% 15:22],
     type = "b",
     xlab = "",
     ylab = "",
     xlim = c(10, 22),
     bty = "n",
     col = "darkorange")

# calcul de la distance parcourue relative par semaine

dep_2018S_sf_latlon <- st_as_sf(dep_2018S,
                         coords = c("lon", "lat"),
                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
mapview(dep_2018S_sf_latlon)

dep_2018S_sf_UTM <- st_transform(dep_2018S_sf_latlon,
                                 crs = 32743)
mapview(dep_2018S_sf_UTM)

# distance
dep_2018S_sf_UTM$spl <- paste(dep_2018S_sf_UTM$id, dep_2018S_sf_UTM$week_num)
k <- split(dep_2018S_sf_UTM,
           dep_2018S_sf_UTM$spl)

k_list <- lapply(k,
                 function(x) {
                     wk <- unique(x$week_num)
                     id <- unique(x$id)
                     dista <- as.numeric(st_distance(x[1,],
                                          x[dim(x)[1], ]))
                     
                     data.frame(id = id,
                                wk_num = wk,
                                dist_m = dista)
                 })

################
# distance parcourue par les oiseaux par semaine
# en partant du dernier point de la semaine précédente
# et recalcul de la vitesse en partant de cette distance

dist_wk_bd <- data.frame()

for(i in 1:length(k)){
     
     print(i)
    
    wk <- unique(k[[i]]$week_num)
    id <- unique(k[[i]]$id)
    

    
    if(wk > 14){
            pres <- k[[i]]
            past <- k[[i-1]]
        dista <- st_distance(pres[dim(pres)[1], ],
                             past[dim(past)[1],])
        bd_speed_mean  <- mean(k[[i]]$speed_km.h_treat, na.rm = T)
        bd_speed_sd  <- sd(k[[i]]$speed_km.h_treat, na.rm = T)
        
    } else {
        dista <- NA
        bd_speed_mean <- NA
        bd_speed_sd <- NA
    }
    
    dist_wk_bd <- rbind(dist_wk_bd,
                        c(id, wk, as.numeric(dista), bd_speed_mean, bd_speed_sd))
}


names(dist_wk_bd) <- c("id", "week_num", "dist_m", "bd_spd_mean_km.h", "bd_spd_sd")
dist_wk_bd <- apply(dist_wk_bd,
                    2,
                    as.numeric)
summary(dist_wk_bd)
dist_wk_bd <- as.data.frame(dist_wk_bd)

# mean dist per week
dist_mean_wk <- aggregate(dist_m ~ week_num,
                          data = dist_wk_bd,
                          mean,
                          na.rm = T)

# cumul dist per week
dist_cum_wk <- aggregate(dist_m ~ week_num,
                         data = dist_wk_bd,
                         sum,
                         na.rm = T)
# mean speed of bird per week
spd_brd_mean_wk <- aggregate(bd_spd_mean_km.h ~ week_num,
                             data = dist_wk_bd,
                             mean,
                             na.rm = T)

# VISU
# 1 - comparaison des deux calculs de vitesse
# ---------------------------------------------
plot(speed_week_2018S$wk[speed_week_2018S$wk %in% 15:22],
     speed_week_2018S$bd_spd[speed_week_2018S$wk %in% 15:22],
     type = "b",
     xlab = "Week number 2018",
     ylab = "Mean speed bird 2018S- computation 1", # original data
     xlim = c(15, 22),
     ylim = c(5, 23),
     bty = "n",
     col = "darkorange")
# arrows(x0 = speed_week$wk[speed_week$wk %in% 15:22],
#        y0 = speed_week$bd_spd[speed_week$wk %in% 15:22],
#        x1 = speed_week$wk[speed_week$wk %in% 15:22],
#        y1 = speed_week$bd_spd[speed_week$wk %in% 15:22] + speed_week$bd_spd_sd[speed_week$wk %in% 15:22],
#        col = "darkorange",
#        code = 0)
lines(spd_brd_mean_wk$week_num,
      spd_brd_mean_wk$bd_spd_mean_km.h) # calcul de vitesse par semaine avec l'objet dist_wk_bd
x11();
plot(dist_mean_wk$week_num,
     dist_mean_wk$dist_m,
     type = "b",
     asp = )
par(new = T)
plot(dist_cum_wk$week_num,
     dist_cum_wk$dist_m,
     type = "b",
     col = "grey")

# 2 - overlap distance parcourue, vitesse moyenne et vitesse du vent dans la zone
# -------------------------------------------------------------------------------

# VISU - distance parcourue en fn vitesse du vent
# ------------------------------------
x11()
par(mar = c(4.1, 4.4, 4.1, 4.4))

plot(dist_mean_wk$week_num,
     dist_mean_wk$dist_m/1000,
     type = "h",
     xlab = "",
     ylab = "",
     yaxt = "n",
     xaxt = "n",
     xlim = c(10, 22),
     bty = "n",
     lwd = 5,
     col ="#68decc")
axis(side = 4,
     lwd = 1,
     las = 2,
     cex.axis = 1)
mtext("Mean travelled distance 2018S (km)",
      side = 4,
      line = 3,
      col = "#68decc")
par(new = T)
plot(wind_caracteristic_2018S$wk_num,
     wind_caracteristic_2018S$wd_spd_mean,
     xlab = "week number 2018",
     ylab = "",
     type = "b",
     bty = "n",
     xlim = c(10, 22),
     ylim = c(3.5, 14),
     lwd = 3,
     col = "#2b9aa6")
mtext("Mean wind speed (km/h)",
      side = 2,
      line = 3,
      col = "#2b9aa6")
text(x = 12.5,
     y = 12,
     "wind inversion")
arrows(x0 = 12.5,
       y0 = 7,
       x1 = 12.5,
       y1 = 11,
       code = 1)

# VISU - vitesse des oiseaux en fn du vent
# --------------------------------------
x11()
par(mar = c(4.1, 4.4, 4.1, 4.4))

plot(spd_brd_mean_wk$week_num,
     spd_brd_mean_wk$bd_spd_mean_km.h,
     xlab = "week number 2018",
     ylab = "",
     type = "b",
     bty = "n",
     xlim = c(10, 22),
     ylim = c(3.5, 20),
     yaxt = "n",
     xaxt = "n",
     lwd = 3,
     col = "#68decc")
axis(side = 4,
     lwd = 1,
     las = 2,
     cex.axis = 1)
mtext("Mean speed of birds (km/h)",
      side = 4,
      line = 3,
      col = "#68decc")

par(new = T)
plot(wind_caracteristic_2018S$wk_num,
     wind_caracteristic_2018S$wd_spd_mean,
     type = "b",
     xlab = "",
     ylab = "",
     xlim = c(10, 22),
     ylim = c(3.5, 14),
     bty = "n",
     lwd = 3,
     col = "#2b9aa6")
mtext("Mean wind speed (km/h)",
      side = 2,
      line = 3,
      col = "#2b9aa6")
text(x = 12.5,
     y = 12,
     "wind inversion")
arrows(x0 = 12.5,
       y0 = 7,
       x1 = 12.5,
       y1 = 11,
       code = 1)


ang_circ <- circular(dep_2018N$dir_bird_deg0_360,
                     type = "angles",
                     units = "degrees",
                     modulo = "2pi")
mean.circular(ang_circ,
              na.rm =T)
