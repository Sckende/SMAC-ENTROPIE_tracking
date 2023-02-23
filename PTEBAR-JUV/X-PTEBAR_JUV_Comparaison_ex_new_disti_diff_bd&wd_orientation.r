# FIRST DF TO COMPARE 
comp <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
names(comp)

comp2 <- comp[comp$Vessel %in% c(166569, 
                                166572,
                                166564,
                                166565,
                                162070,
                                162072,
                                162073,
                                166561,
                                166563),]
comp2 <- droplevels(comp2)
unique(comp2$Vessel)

comp2$diff_dir <- (comp2$bird_0_360_METEO_TOWARD - comp2$wind_dir_0_360) %% 360

# SECOND DF TO COMPARE
loc <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_bird_dir_110max.rds")
names(loc)
head(loc$wind_meteo_dir_loc)
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

# GLOBAL
x11(); par(mfrow = c(1, 2))
hist(comp2$diff_dir, freq = FALSE, breaks = 36, main = "diff direct° bd-wd - no SSM - global", xlab = "angle (°)")
lines(density(comp2$diff_dir, na.rm = T))

hist(loc$diff_wind_bird_loc, freq = FALSE, breaks = 36, main = "diff direct° bd-wd - with SSM - global", xlab = "angle (°)")
lines(density(loc$diff_wind_bird_loc, na.rm = T))

# APRIL

x11(); par(mfrow = c(2, 2))
hist(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 4], freq = FALSE, breaks = 36)
lines(density(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 4], na.rm = T))

hist(comp2$wind_dir_0_360[month(comp2$Date) == 4], freq = FALSE, breaks = 36)
lines(density(comp2$wind_dir_0_360[month(comp2$Date) == 4], na.rm = T))


hist(loc$dir_bird_deg0_360[month(loc$date) == 4], freq = FALSE, breaks = 36)
lines(density(loc$dir_bird_deg0_360[month(loc$date) == 4], na.rm = T))

hist(loc$wind_meteo_dir0_360_loc[month(loc$date) == 4], freq = FALSE, breaks = 36)
lines(density(loc$wind_meteo_dir0_360_loc[month(loc$date) == 4], na.rm = T))

# ---- #
length(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 4])
length(loc$dir_bird_deg0_360[month(loc$date) == 4])

# ---- #
unique(comp2$Vessel)
unique(loc$id) %in% unique(comp2$Vessel)

# MAY

x11(); par(mfrow = c(2, 2))
hist(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 5], freq = FALSE, breaks = 36)
lines(density(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 5], na.rm = T))

hist(comp2$wind_dir_0_360[month(comp2$Date) == 5], freq = FALSE, breaks = 36)
lines(density(comp2$wind_dir_0_360[month(comp2$Date) == 5], na.rm = T))


hist(loc$dir_bird_deg0_360[month(loc$date) == 5], freq = FALSE, breaks = 36)
lines(density(loc$dir_bird_deg0_360[month(loc$date) == 5], na.rm = T))

hist(loc$wind_meteo_dir0_360_loc[month(loc$date) == 5], freq = FALSE, breaks = 36)
lines(density(loc$wind_meteo_dir0_360_loc[month(loc$date) == 5], na.rm = T))


# JUNE

x11(); par(mfrow = c(2, 2))
hist(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 6], freq = FALSE, breaks = 36)
lines(density(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 6], na.rm = T))

hist(comp2$wind_dir_0_360[month(comp2$Date) == 6], freq = FALSE, breaks = 36)
lines(density(comp2$wind_dir_0_360[month(comp2$Date) == 6], na.rm = T))


hist(loc$dir_bird_deg0_360[month(loc$date) == 6], freq = FALSE, breaks = 36)
lines(density(loc$dir_bird_deg0_360[month(loc$date) == 6], na.rm = T))

hist(loc$wind_meteo_dir0_360_loc[month(loc$date) == 6], freq = FALSE, breaks = 36)
lines(density(loc$wind_meteo_dir0_360_loc[month(loc$date) == 6], na.rm = T))

# JULY

x11(); par(mfrow = c(2, 2))
hist(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 7], freq = FALSE, breaks = 36)
lines(density(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 7], na.rm = T))

hist(comp2$wind_dir_0_360[month(comp2$Date) == 7], freq = FALSE, breaks = 36)
lines(density(comp2$wind_dir_0_360[month(comp2$Date) == 7], na.rm = T))


hist(loc$dir_bird_deg0_360[month(loc$date) == 7], freq = FALSE, breaks = 36)
lines(density(loc$dir_bird_deg0_360[month(loc$date) == 7], na.rm = T))

hist(loc$wind_meteo_dir0_360_loc[month(loc$date) == 7], freq = FALSE, breaks = 36)
lines(density(loc$wind_meteo_dir0_360_loc[month(loc$date) == 7], na.rm = T))

# AUGUST

x11(); par(mfrow = c(2, 2))
hist(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 8], freq = FALSE, breaks = 36)
lines(density(comp2$bird_0_360_METEO_TOWARD[month(comp2$Date) == 8], na.rm = T))

hist(comp2$wind_dir_0_360[month(comp2$Date) == 8], freq = FALSE, breaks = 36)
lines(density(comp2$wind_dir_0_360[month(comp2$Date) == 8], na.rm = T))


hist(loc$dir_bird_deg0_360[month(loc$date) == 8], freq = FALSE, breaks = 36)
lines(density(loc$dir_bird_deg0_360[month(loc$date) == 8], na.rm = T))

hist(loc$wind_meteo_dir0_360_loc[month(loc$date) == 8], freq = FALSE, breaks = 36)
lines(density(loc$wind_meteo_dir0_360_loc[month(loc$date) == 8], na.rm = T))


# utilisation de adehabitat pour calculer abs.angle par burst comme pour la premiere methode

ar <- loc
# hist(table(ar$point.group))

#### Retrieve the UTM coordinates from sp object
ar_sp <- SpatialPoints(ar[, c("lon", "lat")],
                          proj4string = CRS("+proj=longlat"))
# obtention UTM coord
ar_UTM <- spTransform(ar_sp,
                         CRS("+init=epsg:32743"))

ar_UTM_sf <- st_as_sf(ar_UTM)

coords <- st_coordinates(ar_UTM_sf)
coords.DF <- as.data.frame(coords)
names(coords.DF) <- c('X', 'Y')

#### Complete coordinates in dataframe
ar <- cbind(as.data.frame(ar)[, - length(names(ar))], coords.DF)

#### Specific to the work on a single device ####
# Deletion of unused factor level
ar <- droplevels(ar)


#### Conversion to ltraj object ####
ar.traj <- as.ltraj(xy = ar[, c('X', 'Y')], # UTM coordinates for meter unitin distance computation
               date = ar$date,
               id = ar$id) # Trajectory split based on the burst of points

ar.traj
names(ar.traj[[1]])
loc_ade <- do.call("rbind", ar.traj)

loc_ade$dir_bird_deg <- loc_ade$abs.angle*180/pi
summary(loc_ade$dir_bird_deg)

loc_ade$dir_bird_deg0_360 <- ifelse(loc_ade$dir_bird_deg >= 0,
                                loc_ade$dir_bird_deg,
                                360 + loc_ade$dir_bird_deg)
summary(loc_ade$dir_bird_deg0_360)

x11(); par(mfrow = c(1, 2))
hist(loc_ade$dir_bird_deg0_360)
hist(loc$dir_bird_deg0_360)
