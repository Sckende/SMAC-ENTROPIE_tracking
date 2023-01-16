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

####################################################################
#### ---- PART 6 - Zoom Ouest OI lors départ des oiseaux  ---- ####
################################################################### 
loc <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_diff_wind_bird_dir.rds")
summary(loc)

loc$week_num <- lubridate::isoweek(loc$date)
table(loc$week_num)

# bird restriction - group 2018_Nord
dep_2018N <- loc[loc$group == "2018-Nord", ]
unique(dep_2018N$id)

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
                      speed_week_ls)
x11()
plot(speed_week$wk[-1],
     speed_week$bd_spd[-1],
     typ = "b",
     xlab = "week number",
     ylab = "mean speed of birds (km/h)")

summary(lm(bd_spd ~ wk, data = speed_week))

# bird restriction - group 2018_Nord WITH NO TIME RESTRICTION
loc_2018N <- loc[loc$group == "2018-Nord", ]

l2 <- split(loc_2018N, loc_2018N$week_num)
speed_week_ls2 <- lapply(l2,
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

speed_week2 <- do.call("rbind",
                      speed_week_ls2)
x11()
plot(speed_week2$wk[3:38],
     speed_week2$bd_spd[3:38],
     typ = "b",
     xlab = "week number",
     ylab = "mean speed of birds (km/h)")

summary(lm(bd_spd ~ wk, data = speed_week2))


# Wind roses
library(openair)
x11()
windRose(mydata = dep_2018N,
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
windRose(mydata = dep_2018N,
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

# ---- #
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

# Bird group 2018 N #
# From S14 to S22
dep_2018N <- as.data.frame((loc[loc$group == "2018-Nord" & loc$week_num %in% 14:22, ]))
summary(dep_2018N); dim(dep_2018N)
unique(dep_2018N$id)
unique(dep_2018N$week_num)
col_pt <- c("#ebed7f", "#a0ed7f", "#7fedde", "#7fa9ed", "#ed7fe4")
col_tck <- c("#eeefb6d2", "#c8edb8", "#d0f3ee", "#c3d5f1", "#eecaeb")

# dep_2018N$col_pt <- left_join(dep_2018N,
#                               data.frame(id = unique(dep_2018N$id),
#                                          col_pt = col_pt),
#                               by = "id")
# dep_2018N$col_tck <- left_join(dep_2018N,
#                               data.frame(id = unique(dep_2018N$id),
#                                          col_tck = col_tck),
#                               by = "id")

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
# x11()
png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/wind_shift_RUN-MADA/RUN-MADA_wind_2018_week_",
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
                             y = list(cex = 1.5))))

# ----- #
# x11()
png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/wind_shift_RUN-MADA/RUN-MADA_wind_2018_bird_group_2018N_week_",
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
          col = pt_ls[[as.character(i)]]$col_pt$col_pt,
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
png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/wind_shift_RUN-MADA/RUN-MADA_wind_2018_week_",
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
hist(ang,
     breaks = 36,
     main = paste("week # ", i, " 2018", sep = ""),
     xlab = "wind orientation (-180°/180°)")
ang_360 <- ifelse(values(ang) >= 0,
                  values(ang),
                  360 + values(ang))
hist(ang_360,
     breaks = 36,
     main = paste("week # ", i, " 2018", sep = ""),
     xlab = "wind orientation (0°/360°)")
graphics.off()
}


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

# calcul de la distance parcourue relative par semaine

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

# ******* A REFAIRE MAIS EN PARTANT DU DERNIER POINT DE LA SEMAINE D AVANT ************ ################

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

# mean dist per week
dist_wk <- aggregate(dist_m ~ week_num,
                    data = dist_wk_bd,
                    mean,
                    na.rm = T)

# mean speed of bird per week
spd_brd_wk <- aggregate(bd_spd_mean_km.h ~ week_num,
                    data = dist_wk_bd,
                    sum,
                    na.rm = T)

x11()
plot(dist_wk$week_num,
     dist_wk$dist_m/1000,
     type = "h",
     xlab = "",
     ylab = "Travelled distance/week (km)",
     xlim = c(10, 22),
     bty = "n",
     lwd = 5,
     col ="#138473")
par(new = T)
plot(wind_caracteristic$wk_num,
     wind_caracteristic$wd_spd_mean,
     xlab = "week number 2018",
     ylab = "",
     type = "b",
     bty = "n",
     xlim = c(10, 22),
     yaxt = "n",
     xaxt = "n",
     lwd = 3,
     col = "#e66916")
axis(side = 4,
     lwd = 1,
     las = 2,
     cex.axis = 1)
text(x = 12.5,
     y = 7.2,
     "wind inversion")
arrows(x0 = 12.5,
       y0 = 4.5,
       x1 = 12.5,
       y1 = 7,
       code = 1)
# par(new = T)
# plot(spd_brd_wk$week_num,
#      spd_brd_wk$bd_spd_mean_km.h,
#      xlab = "",
#      ylab = "",
#      type = "b",
#      bty = "n",
#      xlim = c(10, 22),
#      yaxt = "n",
#      xaxt = "n",
#      lwd = 3,
#      col = "#16e6ca") # pas tres parlant



# distance oiseaux vs. vitesse vents
plot(wind_caracteristic$wd_spd_mean[wind_caracteristic$wk_num %in% 15:22],
     dist_wk$dist_m/1000)

bd_dist <- dist_wk$dist_m/1000
wd_spd <- wind_caracteristic$wd_spd_mean[wind_caracteristic$wk_num %in% 15:22]
summary(lm(bd_dist ~ wd_spd))
