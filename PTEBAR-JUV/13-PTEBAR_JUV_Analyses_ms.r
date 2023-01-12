rm(list = ls())
#### ---- Load packages ------ ####
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")
require(aniMotum)
require(patchwork)


#### ---- Load data ---- ####
argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
head(argos)
dim(argos)
table(argos$Vessel)

ind <- argos[!(argos$Vessel %in% c("166570",
                                   "166571",
                                   "166573",
                                   "162071")), ]
inds <- droplevels(ind)
dim(inds)
table(inds$Vessel)
table(inds$Vessel, year(inds$deploy))

inds$Class[inds$Class == "U"] <- 3
table(inds$Class)

# Formatage pour SSM analyses
indss <- inds[, c("Vessel", "Date", "Class", "Longitude", "Latitude")]
names(indss) <- c("id", "date", "lc", "lon", "lat")
head(indss)

###################################################
#### ---- PART 1 - move persistence model ---- ####
##################################################

# max 100 km/h
# fit_mp <- fit_ssm(indss,
#                   vmax = 28, # env. 100 km/h
#                   ang = NA,
#                   model = "mp",
#                   time.step = NA, # to obtain only the fitted values
#                   control = ssm_control(verbose = 0))

# fit_mp
# summary(fit_mp)
# data_mp <- grab(fit_mp,
#                 what = "fitted")
# x11(); plot(fit_mp, type = 2)

#### ---- Production des figures ---- ####
# ---- inspections visuelles des modèles
# for(i in fit_mp$id) {
#      ind <- dplyr::filter(fit_mp, id == i)
#      res.rw <- osar(ind)
# x11()
# png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/Analyses/Figures/SSM_MP_FITTED_",
#               i,
#               ".png",
#               sep = ""),
#     res = 300,
#     width = 30,
#     height = 30,
#     pointsize = 12,
#     unit = "cm",
#     bg = "transparent")
# print((plot(res.rw, type = "ts") | plot(res.rw, type = "qq")) / 
#   (plot(res.rw, type = "acf") | plot_spacer()))
# dev.off()
# }

####
# max 60 km/h
fit_mp1 <- fit_ssm(indss,
                  vmax = 17, # gannet speed = 61 km/h
                  ang = NA,
                  model = "mp",
                  time.step = NA, # to obtain only the fitted values
                  control = ssm_control(verbose = 0))

fit_mp1
summary(fit_mp1)
data_mp1 <- grab(fit_mp1,
                 what = "fitted")
# saveRDS(data_mp1,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data.rds")

#### ---- Calcul des vitesses à partir de BD à 100km/h max ---- ####
# summary(data_mp)
# class(data_mp)
# x11(); map(fit_mp, what = "fitted")

# # conversion en sf object
# data_mp_sf <- st_as_sf(data_mp,
#                        coords = c("lon", "lat"),
#                        crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# mapview(data_mp_sf, zcol = "id")

# # calcul des vitesses
# data_ls <- split(data_mp_sf,
#                  data_mp_sf$id)
# length(data_ls)
# speed_ls <- lapply(data_ls,
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

# lapply(speed_ls,
#        function(x) {
#            summary(x$delay/60)
#        })

# lapply(speed_ls,
#        function(x) {
#            x11()
#            barplot(x$speed_km.h[x$delay_min <= 120],
#                    main = unique(x$id))
#        })

# # compilation des données
# speed_df <- do.call("rbind",
#                     speed_ls)
# data_mp_sf2 <- left_join(data_mp_sf,
#                          speed_df,
#                          by = c("id", "date"))
# data_mp_sf2[data_mp_sf2$speed_km.h > 110, ]

#### ---- Calcul des vitesses à partir de BD à 60km/h max ---- ####
fit60 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data.rds")
# conversion en sf object
id_date_coords <- fit60[, c("id", "date", "lon", "lat")]
fit60_sf <- st_as_sf(fit60,
                     coords = c("lon", "lat"),
                     crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
mapview(fit60_sf, zcol = "id")

# calcul des vitesses
sf_ls <- split(fit60_sf,
               fit60_sf$id)
length(sf_ls)
speed_ls <- lapply(sf_ls,
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
# inspections numériques & visuelles des vitesses
lapply(speed_ls,
       function(x) {
           summary(x$delay/60) # delay en heure
       }) # ==> troisième quartile < 2 h, choix d'utiliser que les valeurs de vitesse avec un delay <= 2h 

lapply(speed_ls,
       function(x) {
           x11()
           par(mfrow = c(1, 2))
           barplot(x$speed_km.h[x$delay_min <= 120],
                   main = unique(x$id))
           barplot(x$speed_km.h,
                   main = unique(x$id))
       })
# compilation des données
sp <- do.call("rbind",
              speed_ls)
head(sp)
dim(sp)

fit60_sp <- left_join(fit60,
                      sp,
                      by = c("id", "date"))
head(fit60_sp)
# Traitement de la vitesse
fit60_sp$speed_km.h_treat <- fit60_sp$speed_km.h
fit60_sp$speed_km.h_treat[fit60_sp$delay_min > 120] <- NA 

summary(fit60_sp$speed_km.h)
summary(fit60_sp$speed_km.h_treat)

#######################################################################
#### ---- PART 2 - extraction des paramètres environnementaux sous les localisations ajustées ---- ####
#######################################################################


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
        r <- stack(paste(env_folder, v[i], sep = '/'))
        v_list <- c(v_list, r)
    }

stack_var_list[j] <- stack(v_list)
names(stack_var_list)[j] <- var[j]
}

warnings()

names(stack_var_list)


sst_stack <- stack_var_list[[1]]
chlo_stack <- stack_var_list[[2]]
wind_speed_stack <- stack_var_list[[3]]
wind_north_stack <- stack_var_list[[4]]
wind_east_stack <- stack_var_list[[5]]

# ----- > Method : Split the dataframe by date and apply the extraction by group of coordinates #
#########################################################################################################

###################
# ---- SST & chlo #
###################
length(unique(date(fit60_sp$date)))

fit60_sp$dt <- str_replace_all(as.character(date(fit60_sp$date)),
                               '-',
                               '.')
fit60_list <- split(fit60_sp,
                    fit60_sp$dt)
length(fit60_list)

fit60_list2 <- lapply(fit60_list, function(x){

    sst_raster <- sst_stack[[str_which(names(sst_stack), unique(x$dt))]]
    chlo_raster <- chlo_stack[[str_which(names(chlo_stack), unique(x$dt))]] 

    x$sst <- extract(sst_raster,
                     as.data.frame(x[, c('lon', 'lat')]))
    x$chlo <- extract(chlo_raster,
                      as.data.frame(x[, c('lon', 'lat')]))
    
    # ----- #
    print(unique(x$dt))
    
    x
})
print("ayéééé")

fit60_2 <- do.call('rbind', fit60_list2)
mean(fit60_2$sst, na.rm = T)
summary(fit60_2$sst)
fit60_2$sst_deg <- fit60_2$sst - 273.15
summary(fit60_2$sst_deg)

mean(fit60_2$chlo, na.rm = T)
summary(fit60_2$chlo)

names(fit60_2)
#######################"################
# ---- For WIND speed & orientation ####
########################################

# Deletion of duplicated layers in raster
# ---> wind_east_stack #
########################

str_length(names(wind_east_stack))
j <- names(wind_east_stack)[str_length(names(wind_east_stack)) > 20]
east_wind_deletion <- j[str_detect(j, '.00.2')]
dim(wind_east_stack)
wind_east_stack2 <- dropLayer(wind_east_stack,
                             match(east_wind_deletion,
                                   names(wind_east_stack)))
dim(wind_east_stack2)
names(wind_east_stack2)

# ---> wind_north_stack #
#########################
str_length(names(wind_north_stack))
k <- names(wind_north_stack)[str_length(names(wind_north_stack)) > 20]
north_wind_deletion <- k[str_detect(k, '.00.2')]
dim(wind_north_stack)
wind_north_stack2 <- dropLayer(wind_north_stack,
                              match(north_wind_deletion, 
                                    names(wind_north_stack)))
dim(wind_north_stack2)

# ---> wind_speed_stack #
#########################
str_length(names(wind_speed_stack))
l <- names(wind_speed_stack)[str_length(names(wind_speed_stack)) > 20]
speed_wind_deletion <- l[str_detect(l, '.00.2')]
dim(wind_speed_stack)
wind_speed_stack2 <- dropLayer(wind_speed_stack,
                              match(speed_wind_deletion,
                                    names(wind_speed_stack)))
dim(wind_speed_stack2)

# ----- #
head(fit60_2, 50)
fit60_2$minutes <- hour(fit60_2$date)*60 + minute(fit60_2$date)

# raster aux 6h 
# deb      fin       raster.hour     raster.day
# 21:01 (1261) -> 03:00 (180)  ==> 00:00           J+1 or J depending on bef/aft midnight
# 03:01 (181)  -> 09:00 (540)  ==> 06:00            J
# 09:01 (541)  -> 15:00 (900)  ==> 12:00            J
# 15:01 (901)  -> 21:00 (1260) ==> 18:00            J
fit60_2$raster_hour <- NA
fit60_2$raster_hour[fit60_2$minutes >= 1261 | fit60_2$minutes <= 180] <- "00.00"
fit60_2$raster_hour[fit60_2$minutes >= 181 & fit60_2$minutes <= 540] <- "06.00"
fit60_2$raster_hour[fit60_2$minutes >= 541 & fit60_2$minutes <= 900] <- "12.00"
fit60_2$raster_hour[fit60_2$minutes >= 901 & fit60_2$minutes <= 1260] <- "18.00"

fit60_2$raster_date <- ifelse(fit60_2$minutes >= 1261,
                             as.character(date(fit60_2$date)+1),
                             as.character(date(fit60_2$date)))

fit60_2$raster_layer <- paste(str_replace_all(fit60_2$raster_date, '-', '.'),
                             fit60_2$raster_hour,
                             sep = '.')

fit60_2_list <- split(fit60_2,
                      fit60_2$raster_layer)
length(fit60_2_list)

fl3 <- lapply(fit60_2_list, function(x) {
# ----- #
    speed_raster <- wind_speed_stack[[str_which(names(wind_speed_stack),
                                                unique(x$raster_layer))]]
    north_raster <- wind_north_stack[[str_which(names(wind_north_stack),
                                                unique(x$raster_layer))]]
    east_raster <- wind_east_stack[[str_which(names(wind_east_stack),
                                              unique(x$raster_layer))]]
# ----- #
    x$wind_speed <- extract(speed_raster,
                            as.data.frame(x[, c("lon", "lat")]))
    x$wind_north <- extract(north_raster,
                            as.data.frame(x[, c("lon", "lat")]))
    x$wind_east <- extract(east_raster,
                           as.data.frame(x[, c("lon", "lat")]))
# ----- #
    print(unique(x$raster_layer))
# ----- #
    x
})
print("ayééé")

fit60_3 <- do.call("rbind",
                   fl3)
dim(fit60_3)
warnings() #### CHECK FOR WARNINGS HERE #####
mean(fit60_3$wind_speed, na.rm = T)
summary(fit60_3$wind_speed)
names(fit60_3)

# computation of wind speed & direction
# abs_wind_sp <- sqrt(u^2 + v^2)
# wind_dir <- 180 * atan2(v, u) / pi # with u = east and v = north AND atan2 gives direction in radian, then *180/pi allows the conversion in degree from -180 to 180
# In addition, HERE the atan2 gives the angle with METEOROLOGICAL convention
# N = 0 = 360, E = 90, S = 180, W = 270

fit60_3$wind_meteo_dir <- 180 * atan2(fit60_3$wind_north, fit60_3$wind_east) / pi

fit60_3$abs_wind_spd <- sqrt(fit60_3$wind_east^2 + fit60_3$wind_north^2)

head(as.data.frame(fit60_3))
summary(fit60_3$wind_speed * 0.001 * 3600)
summary(fit60_3$abs_wind_spd * 0.001 * 3600)

# save the database
# saveRDS(fit60_3,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param.rds")

#########################################################
#### ---- PART 3 - Kernels avec locs corrigees ---- ####
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
#### ---- PART 4 - Direction que prennent les oiseaux  ---- ####
################################################################

loc <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param.rds")
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

# save the database
# saveRDS(dir2,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_bird_dir.rds")

#################################################################
#### ---- PART 5 - Difference entre l'orientation des oiseaux et celui du vent  ---- ####
################################################################
loc <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_bird_dir.rds")
names(loc)
head(loc$wind_meteo_dir)
# --------------- #
loc$dir_bird_deg0_360 <- ifelse(loc$dir_bird_deg >= 0,
                                loc$dir_bird_deg,
                                360 + loc$dir_bird_deg)

loc$wind_meteo_dir0_360 <- ifelse(loc$wind_meteo_dir >= 0,
                                  loc$wind_meteo_dir,
                                  360 + loc$wind_meteo_dir)

loc$diff_wind_bird <- (loc$dir_bird_deg0_360 - loc$wind_meteo_dir0_360) %% 360

summary(loc$diff_wind_bird) # **** WARNING **** Données circulaires 

# visualisation
x11()
hist(loc$diff_wind_bird, breaks = 36)
hist(loc$diff_wind_bird[lubridate::month(loc$date) == 4], breaks = 36)
hist(loc$diff_wind_bird[lubridate::month(loc$date) == 5], breaks = 36)
hist(loc$diff_wind_bird[lubridate::month(loc$date) == 6], breaks = 36)
hist(loc$diff_wind_bird[lubridate::month(loc$date) == 7], breaks = 36)
hist(loc$diff_wind_bird[lubridate::month(loc$date) == 8], breaks = 36)
hist(loc$diff_wind_bird[lubridate::month(loc$date) == 9], breaks = 36)
hist(loc$diff_wind_bird[lubridate::month(loc$date) == 10], breaks = 36)
hist(loc$diff_wind_bird[lubridate::month(loc$date) == 11], breaks = 36)

x11()
plot(loc$diff_wind_bird[month(loc$date) == 4])

hist(loc$wind_meteo_dir0_360[lubridate::month(loc$date) == 4],
     breaks = 36,
     col = "#e6891f82",
     freq = F)
lines(density(loc$wind_meteo_dir0_360[lubridate::month(loc$date) == 4]),
      lwd = 2,
      col = "sienna3")
hist(loc$dir_bird_deg0_360[lubridate::month(loc$date) == 4],
     breaks = 36,
     freq = F,
     col = "#0baea0aa",
     add = T)
lines(density(loc$dir_bird_deg0_360[lubridate::month(loc$date) == 4]),
      lwd = 2,
      col = "#076864")

# ----- #
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


loc_group <- split(loc, loc$group)

lapply(loc_group, function(x){
    x11()
    hist(x$diff_wind_bird[month(x$date) == 4],
         col = "#2812c8ae",
         breaks = 36,
         freq = F,
         main = paste("diff angle bird-wind", unique(x$group), sep = " "),
         xlab = "Angle (°)")
    lines(density(x$diff_wind_bird[month(x$date) == 4]),
         col = "#041c38",
         lwd = 2)
    
    x11()
    hist(x$wind_meteo_dir0_360[lubridate::month(x$date) == 4],
         breaks = 36,
         col = "#e6891f82",
         freq = F,
         main = paste("angle bird-wind", unique(x$group), sep = " "),
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
})


# save the database
# saveRDS(loc,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_diff_wind_bird_dir.rds")
