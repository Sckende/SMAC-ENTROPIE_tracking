# Production de carte synthétiques aux 3 mois #
# Pour la direction & la force des vents #
# Pour la chlo-a #
# Avec trajet des juveniles (RAGOS) et des adultes (GLS) #

# ---- Load packages ------ #
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

##### ----- Juvenile data - Argos from Pinet 2017 & 2018 ----- ####
# ----- Load data ----- #
juv_argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")

summary(juv_argos)
class(juv_argos$Date)

juv_argos$year_period[month(juv_argos$Date) %in% 1:3] <- "JFM"
juv_argos$year_period[month(juv_argos$Date) %in% 4:6] <- "AMJ"
juv_argos$year_period[month(juv_argos$Date) %in% 7:9] <- "JAS"
juv_argos$year_period[month(juv_argos$Date) %in% 10:12] <- "OND"

table(juv_argos$year_period)

juv_argos_sp <- SpatialPointsDataFrame(coords = juv_argos[, c("Longitude", "Latitude")],
                                  data = juv_argos,
                                  proj4string = CRS("+init=epsg:4326"))
##### ----- Adulte data - GLS from Pinet 2008 & 2009 ----- ####
# ----- Load data ----- #
ad_gls <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_RUN_tripsplit.txt",
                     h = T,
                     sep = "\t")
head(ad_gls)
summary(ad_gls)
dim(ad_gls)

# ----- Year division in trimestre ----- #
ad_gls$Date_GMT <- as.POSIXlt(ad_gls$Date_GMT,
                              format = "%d/%m/%y %H:%M")
ad_gls$year_period[month(ad_gls$Date_GMT) %in% 1:3] <- "JFM"
ad_gls$year_period[month(ad_gls$Date_GMT) %in% 4:6] <- "AMJ"
ad_gls$year_period[month(ad_gls$Date_GMT) %in% 7:9] <- "JAS"
ad_gls$year_period[month(ad_gls$Date_GMT) %in% 10:12] <- "OND"

table(ad_gls$year_period)

# par(mfrow = c(2, 2))
 for(i in unique(ad_gls$year_period)) {
     plot(x = ad_gls$LON[ad_gls$year_period == i],
          y = ad_gls$LAT[ad_gls$year_period == i],
          main = i,
          xlim = c(min(ad_gls$LON), max(ad_gls$LON)),
          ylim = c(min(ad_gls$LAT), max(ad_gls$LAT)))
 }
 
 ad_gls_sp <- SpatialPointsDataFrame(coords = ad_gls[, c("LON", "LAT")],
                                  data = ad_gls,
                                  proj4string = CRS("+init=epsg:4326"))

#### ----- CHLO-A data ----- ####
# ----------------------------- #

# chlo2018 <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/",
#                    pattern = "OCEANCOLOUR_GLO_CHL_L4_REP_OBSERVATIONS_009_082-TDS__CHLO-2018",
#                    full.names = TRUE)

# # ----- Period JANFEVMAR
# chlo_2018_1 <- terra::rast(chlo2018[1:3])
# # Spatial crop
# extend <- extent(30, 120, -50, 10) # xmin, xmax, ymin, ymax
# chlo_2018_1 <- crop(chlo_2018_1,
#                     extend)


# names(chlo_2018_1) <- substr(as.character(terra::time(chlo_2018_1)),
#                              1,
#                              10)
# mean_chlo_2018_1 <- mean(chlo_2018_1)
# plot(mean_chlo_2018_1,
#      main = "JFM 2018")
# points(x = ad_gls$LON[ad_gls$year_period == "JFM"],
#      y = ad_gls$LAT[ad_gls$year_period == "JFM"],
#      pch = 19,
#      col = "red")
# points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JFM"],
#        y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JFM"],
#        pch = 19,
#        col = "blue")
# # ----- Period AVRMAIJUI
# chlo_2018_2 <- terra::rast(chlo2018[4:6])

# # Spatial crop
# chlo_2018_2 <- crop(chlo_2018_2,
#                     extend)

# names(chlo_2018_2) <- substr(as.character(terra::time(chlo_2018_2)),
#                              1,
#                              10)
# mean_chlo_2018_2 <- mean(chlo_2018_2)
# plot(mean_chlo_2018_2,
#      main = "AMJ 2018")
# points(x = ad_gls$LON[ad_gls$year_period == "AMJ"],
#      y = ad_gls$LAT[ad_gls$year_period == "AMJ"],
#      pch = 19,
#      col = "red")
# points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "AMJ"],
#        y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "AMJ"],
#        pch = 19,
#        col = "blue")
# # ----- Period JUIAIUSEP
# chlo_2018_3 <- terra::rast(chlo2018[7:9])
# # SPatial crop
# chlo_2018_3 <- crop(chlo_2018_3,
#                     extend)

# names(chlo_2018_3) <- substr(as.character(terra::time(chlo_2018_3)),
#                              1,
#                              10)
# mean_chlo_2018_3 <- mean(chlo_2018_3)
# plot(mean_chlo_2018_3,
#      main = "JAS 2018")
# points(x = ad_gls$LON[ad_gls$year_period == "JAS"],
#      y = ad_gls$LAT[ad_gls$year_period == "JAS"],
#      pch = 19,
#      col = "red")
# points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JAS"],
#        y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JAS"],
#        pch = 19,
#        col = "blue")

# # ----- Period OCTNOVDEC
# chlo_2018_4 <- terra::rast(chlo2018[10:12])
# # Spatial crop
# chlo_2018_4 <- crop(chlo_2018_4,
#                     extend)

# names(chlo_2018_4) <- substr(as.character(terra::time(chlo_2018_4)),
#                              1,
#                              10)
# mean_chlo_2018_4 <- mean(chlo_2018_4)
# plot(mean_chlo_2018_4,
#      main = "OND 2018")
# points(x = ad_gls$LON[ad_gls$year_period == "OND"],
#      y = ad_gls$LAT[ad_gls$year_period == "OND"],
#      pch = 19,
#      col = "red")
# points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "OND"],
#        y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "OND"],
#        pch = 19,
#        col = "blue")

# #### ----- Graphical output - CHLO-A ----- ####
# # ------------------------------------------- #

# png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/PTEBAR_JUV_Carto/Chlo-a_n_birds_2018.png",
#     res = 300,
#     width = 50,
#     height = 40,
#     pointsize = 20,
#     unit = "cm",
#     # bg = "transparent",
#     bg = "white")

# par(mfrow = c(2, 2))

# # ----- AMJ ----- #
# plot(mean_chlo_2018_2,
#      main = "AMJ 2018")
# points(x = ad_gls$LON[ad_gls$year_period == "AMJ"],
#      y = ad_gls$LAT[ad_gls$year_period == "AMJ"],
#      pch = 19,
#      col = "red")
# points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "AMJ"],
#        y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "AMJ"],
#        pch = 19,
#        col = "blue")

# # ----- JAS ----- #
# plot(mean_chlo_2018_3,
#      main = "JAS 2018")
# points(x = ad_gls$LON[ad_gls$year_period == "JAS"],
#      y = ad_gls$LAT[ad_gls$year_period == "JAS"],
#      pch = 19,
#      col = "red")
# points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JAS"],
#        y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JAS"],
#        pch = 19,
#        col = "blue")

# # ----- OND ----- #
# plot(mean_chlo_2018_4,
#      main = "OND 2018")
# points(x = ad_gls$LON[ad_gls$year_period == "OND"],
#      y = ad_gls$LAT[ad_gls$year_period == "OND"],
#      pch = 19,
#      col = "red")
# points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "OND"],
#        y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "OND"],
#        pch = 19,
#        col = "blue")

# # ----- JFM ----- #
# plot(mean_chlo_2018_1,
#      main = "JFM 2018")
# points(x = ad_gls$LON[ad_gls$year_period == "JFM"],
#      y = ad_gls$LAT[ad_gls$year_period == "JFM"],
#      pch = 19,
#      col = "red")
# points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JFM"],
#        y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JFM"],
#        pch = 19,
#        col = "blue")

# dev.off()

#### ----- WIND data ----- ####
# ----------------------------- #

## YEAR 1 - 2017 ####
speed1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652088629_YEAR1_SPEED.nc')

zon1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637651769964_YEAR1_ZONAL.nc')
mer1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637651880863_YEAR1_MERIDIONAL.nc')

## YEAR 2 - 2018 ####
speed2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637653002349_YEAR2_SPEED.nc')

zon2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652399617_YEAR2_ZONAL.nc')
mer2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652499961_YEAR2_MERIDIONAL.nc')

# -----> 2017
#############

date1 <- names(mer1)
date1 <- substr(date1,
                2,
                20)
date1 <- as.POSIXlt(date1,
                    format = "%Y.%m.%d.%H.%M.%S")
# ----- #
year_period <- vector()

for(i in 1:length(date1)) {
     if(month(date1[i]) %in% 1:3) {
          year_period[i] <- "JFM"
     } else if(month(date1[i]) %in% 4:6) {
          year_period[i] <- "AMJ"
     } else if(month(date1[i]) %in% 7:9) {
          year_period[i] <- "JAS"
     } else {
          year_period[i] <- "OND"
     }
}
# ----- #
date11 <- paste(names(mer1),
                "-",
                year_period,
                sep = "")

names(mer1) <- date11
names(zon1) <- date11
names(speed1) <- date11

# -----> 2018
#############

date2 <- names(mer2)
date2 <- substr(date2,
                2,
                20)
date2 <- as.POSIXlt(date2,
                    format = "%Y.%m.%d.%H.%M.%S")
# ----- # 
year_period2 <- vector()

for(i in 1:length(date2)) {
     if(month(date2[i]) %in% 1:3) {
          year_period2[i] <- "JFM"
     } else if(month(date2[i]) %in% 4:6) {
          year_period2[i] <- "AMJ"
     } else if(month(date2[i]) %in% 7:9) {
          year_period2[i] <- "JAS"
     } else {
          year_period2[i] <- "OND"
     }
}
# ----- #
date22 <- paste(names(mer2),
                "-",
                year_period2,
                sep = "")

names(mer2) <- date22
names(zon2) <- date22
names(speed2) <- date22

#### ----- Graphical output - WIND ----- ####
# ----------------------------------------- #

# Map of Indian Ocean
ext <- as.vector(extent(zon2[[1]]))
library(maps)
library(maptools)
IndOcean <- map("world",
             fill = T,
             xlim = ext[1:2],
             ylim = ext[3:4],
             col = "grey")

IndOcean_sp <- maptools::map2SpatialPolygons(IndOcean,
                                          IDs = IndOcean$names,
                                proj4string = CRS(projection(zon2[[1]])))
# color option for wind scale
#############################

nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)

# ----------------------------------------- #
# ----- AMJ 2018----- #
# ------------------- #
zon2_AMJ <- zon2[[names(zon2)[which(str_detect(names(zon2), "AMJ"))]]]
mean_zon2_AMJ <- mean(zon2_AMJ)

mer2_AMJ <- mer2[[names(mer2)[which(str_detect(names(mer2), "AMJ"))]]]
mean_mer2_AMJ <- mean(mer2_AMJ)

speed2_AMJ <- speed2[[names(speed2)[which(str_detect(names(speed2), "AMJ"))]]]
mean_speed2_AMJ <- mean(speed2_AMJ)

png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/PTEBAR_JUV_Carto/Wind_n_birds_2018_AMJ.png",
    res = 300,
    width = 50,
    height = 40,
    pointsize = 20,
    unit = "cm",
    # bg = "transparent",
    bg = "white")

print(
     rasterVis::vectorplot(raster::stack(mean_zon2_AMJ, mean_mer2_AMJ),
               isField = 'dXY',
               narrows = 800,
               lwd.arrows = 1,
               aspX = 0.4,
               region = mean_speed2_AMJ,
               at = my_at,
               col.regions = my_cols,
               # scales = list(cex = 1.5),
               colorkey = list(labels = list(cex = 2)),
               main = list("AMJ 2018",
                           cex = 2.5),
               xlab = list("Longitude", 
                           cex = 2.5),
               ylab = list("Latitude",
                           cex = 2.5))     +
    layer(c(sp.points(ad_gls_sp[ad_gls_sp$year_period == "AMJ", ],
                      col = rgb(0, 0, 1, alpha = 0.9),
                      lwd = 2),
            sp.points(juv_argos_sp[juv_argos_sp$year_period == "AMJ" & year(juv_argos_sp$deploy) == 2018, ],
                      col = "white",
                      lwd = 2),
            sp.polygons(IndOcean_sp,
                        col = "grey",
                        fill = "white"))))
    
dev.off()

# ----- JAS 2018----- #
# ------------------- #
zon2_JAS <- zon2[[names(zon2)[which(str_detect(names(zon2), "JAS"))]]]
mean_zon2_JAS <- mean(zon2_JAS)

mer2_JAS <- mer2[[names(mer2)[which(str_detect(names(mer2), "JAS"))]]]
mean_mer2_JAS <- mean(mer2_JAS)

speed2_JAS <- speed2[[names(speed2)[which(str_detect(names(speed2), "JAS"))]]]
mean_speed2_JAS <- mean(speed2_JAS)

png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/PTEBAR_JUV_Carto/Wind_n_birds_2018_JAS.png",
    res = 300,
    width = 50,
    height = 40,
    pointsize = 20,
    unit = "cm",
    # bg = "transparent",
    bg = "white")

print(
     rasterVis::vectorplot(raster::stack(mean_zon2_JAS, mean_mer2_JAS),
               isField = 'dXY',
               narrows = 800,
               lwd.arrows = 1,
               aspX = 0.4,
               region = mean_speed2_JAS,
               at = my_at,
               col.regions = my_cols,
               # scales = list(cex = 1.5),
               colorkey = list(labels = list(cex = 2)),
               main = list("JAS 2018",
                           cex = 2.5),
               xlab = list("Longitude", 
                           cex = 2.5),
               ylab = list("Latitude",
                           cex = 2.5))     +
    layer(c(sp.points(ad_gls_sp[ad_gls_sp$year_period == "JAS", ],
                      col = rgb(0, 0, 1, alpha = 0.9),
                      lwd = 2),
            sp.points(juv_argos_sp[juv_argos_sp$year_period == "JAS" & year(juv_argos_sp$deploy) == 2018, ],
                      col = "white",
                      lwd = 2)),
          sp.polygons(IndOcean_sp,
                      col = "grey",
                      fill = "white")))
    
dev.off()

# ----- OND 2018----- #
# ------------------- #
zon2_OND <- zon2[[names(zon2)[which(str_detect(names(zon2), "OND"))]]]
mean_zon2_OND <- mean(zon2_OND)

mer2_OND <- mer2[[names(mer2)[which(str_detect(names(mer2), "OND"))]]]
mean_mer2_OND <- mean(mer2_OND)

speed2_OND <- speed2[[names(speed2)[which(str_detect(names(speed2), "OND"))]]]
mean_speed2_OND <- mean(speed2_OND)

png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/PTEBAR_JUV_Carto/Wind_n_birds_2018_OND.png",
    res = 300,
    width = 50,
    height = 40,
    pointsize = 20,
    unit = "cm",
    # bg = "transparent",
    bg = "white")

print(
     rasterVis::vectorplot(raster::stack(mean_zon2_OND, mean_mer2_OND),
               isField = 'dXY',
               narrows = 800,
               lwd.arrows = 1,
               aspX = 0.4,
               region = mean_speed2_OND,
               at = my_at,
               col.regions = my_cols,
               # scales = list(cex = 1.5),
               colorkey = list(labels = list(cex = 2)),
               main = list("OND 2018",
                           cex = 2.5),
               xlab = list("Longitude", 
                           cex = 2.5),
               ylab = list("Latitude",
                           cex = 2.5))     +
    layer(c(sp.points(ad_gls_sp[ad_gls_sp$year_period == "OND", ],
                      col = rgb(0, 0, 1, alpha = 0.9),
                      lwd = 2),
            sp.points(juv_argos_sp[juv_argos_sp$year_period == "OND" & year(juv_argos_sp$deploy) == 2018, ],
                      col = "white",
                      lwd = 2)),
          sp.polygons(IndOcean_sp,
                      col = "grey",
                      fill = "white")))
    
dev.off()

# ----- JFM 2019----- #
# ------------------- #
zon2_JFM <- zon2[[names(zon2)[which(str_detect(names(zon2), "JFM"))]]]
mean_zon2_JFM <- mean(zon2_JFM)

mer2_JFM <- mer2[[names(mer2)[which(str_detect(names(mer2), "JFM"))]]]
mean_mer2_JFM <- mean(mer2_JFM)

speed2_JFM <- speed2[[names(speed2)[which(str_detect(names(speed2), "JFM"))]]]
mean_speed2_JFM <- mean(speed2_JFM)

png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/PTEBAR_JUV_Carto/Wind_n_birds_2019_JFM.png",
    res = 300,
    width = 50,
    height = 40,
    pointsize = 20,
    unit = "cm",
    # bg = "transparent",
    bg = "white")

print(
     rasterVis::vectorplot(raster::stack(mean_zon2_JFM, mean_mer2_JFM),
               isField = 'dXY',
               narrows = 800,
               lwd.arrows = 1,
               aspX = 0.4,
               region = mean_speed2_JFM,
               at = my_at,
               col.regions = my_cols,
               # scales = list(cex = 1.5),
               colorkey = list(labels = list(cex = 2)),
               main = list("JFM 2019",
                           cex = 2.5),
               xlab = list("Longitude", 
                           cex = 2.5),
               ylab = list("Latitude",
                           cex = 2.5))     +
    layer(c(sp.points(ad_gls_sp[ad_gls_sp$year_period == "JFM", ],
                      col = rgb(0, 0, 1, alpha = 0.9),
                      lwd = 2),
            sp.points(juv_argos_sp[juv_argos_sp$year_period == "JFM" & year(juv_argos_sp$deploy) == 2018, ],
                      col = "white",
                      lwd = 2)),
          sp.polygons(IndOcean_sp,
                      col = "grey",
                      fill = "white")))
    
dev.off()

#### ---------------------- Soustraction direction bird vs wind & histo --------------------- ####
unique(juv_argos$Vessel)
names(juv_argos)
# bird_0_360_METEO_TOWARD = direction que prenne les oiseaux sur la projection météo (N = 0, E = 90, ...) avec angle compris entre 0 et 359 degrés
# wind_dir_0_360" = direction de l'origine des vents avec projection météo (N = 0, E = 90, ...) et angle compris entre 0 et 359

juv_argos$diff_dir <- (juv_argos$bird_0_360_METEO_TOWARD - juv_argos$wind_dir_0_360) %% 360
summary(juv_argos$diff_dir)
x11(); hist(juv_argos$diff_dir,
     breaks = 36)


PTT <- c("166569",
         "166572",
         "166564",
         "166565",
         "162070",
         "162072",
         "162073",
         "166561",
         "166563")

juv_argos2 <- juv_argos[juv_argos$Vessel %in% PTT, ]
juv_argos2 <- droplevels(juv_argos2)

# juv_argos2$year_period <- factor(juv_argos2$year_period, levels = c("AMJ", "JAS", "OND", "JFM"))


ju_list <- split(juv_argos2, juv_argos2$Vessel)

# ----- Global histo per track ----- #
x11(); par(mfrow = c(3, 3))
lapply(ju_list, function(x) {
     
     
     hist(x$diff_dir,
          breaks = 36,
          main = paste(unique(year(x$deploy)),
                       unique(x$Vessel),
                       sep = "-"))

})

# ----- Trimestrial histo per track ----- #
juv_argos3 <- juv_argos2[juv_argos2$year_period %in%c("AMJ", "JAS", "OND"), ]
juv_argos3 <- droplevels(juv_argos3)
ju_list2 <- split(juv_argos3, juv_argos3$Vessel)

lapply(ju_list2, function(x) {
     x_list <- split(x, x$year_period)
     
     png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/HISTO_diff_directions/DIFF_DIR_Trimestrial-",
               unique(year(x$deploy)),
               "-",
               unique(x$Vessel),
               ".png",
               sep = ""),
    res = 300,
    width = 50,
    height = 50,
    pointsize = 20,
    unit = "cm",
    bg = "white")
     
     # x11()
     par(mfrow = c(2, 2))
     lapply(x_list, function(y) {
          hist(y$diff_dir,
               breaks = 36
               ,
               main = paste(unique(year(y$deploy)),
                            unique(y$Vessel),
                            unique(y$year_period),
               sep = "-")
               )
     })
     dev.off()
})

# ----- Monthly histo per track ----- #
juv_argos2$month_numb <- month(juv_argos2$Date)
ju_list3 <- split(juv_argos2, juv_argos2$Vessel)

lapply(ju_list3, function(x) {
     x_list <- split(x, x$month_numb)
     
     print(length(x_list))
     
     png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/HISTO_diff_directions/DIFF_DIR_Monthly-",
               unique(year(x$deploy)),
               "-",
               unique(x$Vessel),
               ".png",
               sep = ""),
    res = 300,
    width = 70,
    height = 50,
    pointsize = 20,
    unit = "cm",
    bg = "white")
     
     # x11()
     par(mfrow = c(3, 4))
     lapply(x_list, function(y) {
          hist(y$diff_dir,
               breaks = 36,
               main = paste(unique(year(y$deploy)),
                            unique(y$month_numb),
                            unique(y$Vessel),
                            sep = "-")
               )
     })
     dev.off()
})

#### ----- DIFF DIR histo - 2 ind 2017 ----- ####
juv_2017 <- juv_argos[juv_argos$Vessel %in% c("166569",
                                              "166572"), ]
juv_2017 <- droplevels(juv_2017)

# ----- Global histo ----- #
x11()
hist(juv_2017$diff_dir,
     breaks = 36,
     main = paste(unique(year(juv_2017$deploy)),
                       "166569 & 166572",
                       sep = "-"))


# ----- Trimestrial histo per track ----- #
juv_2017_list <- split(juv_2017, juv_2017$year_period)

png("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/HISTO_diff_directions/DIFF_DIR_2017_group_Trimestrial.png",
    res = 300,
    width = 50,
    height = 50,
    pointsize = 20,
    unit = "cm",
    bg = "white")

par(mfrow = c(2, 2))

lapply(juv_2017_list, function(x) {
     hist(x$diff_dir,
          breaks = 36,
          main = paste(unique(year(x$deploy)),
                       "166569 & 166572",
                       unique(x$year_period),
                       sep = "-")
               )
     })
dev.off()

# ----- Monthly histo per track ----- #
juv_2017$month_numb <- month(juv_2017$Date)
juv_2017_list2 <- split(juv_2017, juv_2017$month_numb)
table(juv_2017$month_numb)
png("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/HISTO_diff_directions/DIFF_DIR_2017_group_Monthly.png",
    res = 300,
    width = 70,
    height = 50,
    pointsize = 20,
    unit = "cm",
    bg = "white")
par(mfrow = c(2, 3))
lapply(juv_2017_list2, function(x) {
     hist(x$diff_dir,
          breaks = 36,
          main = paste(unique(year(x$deploy)),
                       unique(x$month_numb),
                       "166569 & 166572",
                       sep = "-")
               )
     })

dev.off()

#### ----- DIFF DIR histo - 2 ind 2018 - SUD ----- ####
juv_2018_sud <- juv_argos[juv_argos$Vessel %in% c("166564",
                                              "166565"), ]
juv_2018_sud <- droplevels(juv_2018_sud)

# ----- Global histo ----- #
x11()
hist(juv_2018_sud$diff_dir,
     breaks = 36,
     main = paste(unique(year(juv_2018_sud$deploy)),
                       "166564 & 166565",
                       sep = "-"))


# ----- Trimestrial histo per track ----- #
juv_2018_sud_list <- split(juv_2018_sud, juv_2018_sud$year_period)

png("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/HISTO_diff_directions/DIFF_DIR_2018_group_SUD_Trimestrial.png",
    res = 300,
    width = 50,
    height = 50,
    pointsize = 20,
    unit = "cm",
    bg = "white")

par(mfrow = c(2, 2))

lapply(juv_2018_sud_list, function(x) {
     hist(x$diff_dir,
          breaks = 36,
          main = paste(unique(year(x$deploy)),
                       "166564 & 166565",
                       unique(x$year_period),
                       sep = "-")
               )
     })
dev.off()

# ----- Monthly histo per track ----- #
juv_2018_sud$month_numb <- month(juv_2018_sud$Date)
juv_2018_sud_list2 <- split(juv_2018_sud, juv_2018_sud$month_numb)
table(juv_2018_sud$month_numb)
png("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/HISTO_diff_directions/DIFF_DIR_2018_group_SUD_Monthly.png",
    res = 300,
    width = 70,
    height = 50,
    pointsize = 20,
    unit = "cm",
    bg = "white")
par(mfrow = c(3, 3))
lapply(juv_2018_sud_list2, function(x) {
     hist(x$diff_dir,
          breaks = 36,
          main = paste(unique(year(x$deploy)),
                       unique(x$month_numb),
                       "166564 & 166565",
                       sep = "-")
               )
     })

dev.off()


#### ----- DIFF DIR histo - 5 ind 2018 - NORD ----- ####
juv_2018_nord <- juv_argos[juv_argos$Vessel %in% c("162070",
                                              "162072",
                                              "162073",
                                              "166561",
                                              "166563"), ]
juv_2018_nord <- droplevels(juv_2018_nord)

# ----- Global histo ----- #
x11()
hist(juv_2018_nord$diff_dir,
     breaks = 36,
     main = paste(unique(year(juv_2018_nord$deploy)),
                       "NORD",
                       sep = "-"))


# ----- Trimestrial histo per track ----- #
juv_2018_nord_list <- split(juv_2018_nord, juv_2018_nord$year_period)

png("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/HISTO_diff_directions/DIFF_DIR_2018_group_NORD_Trimestrial.png",
    res = 300,
    width = 50,
    height = 50,
    pointsize = 20,
    unit = "cm",
    bg = "white")

par(mfrow = c(2, 2))

lapply(juv_2018_nord_list, function(x) {
     hist(x$diff_dir,
          breaks = 36,
          main = paste(unique(year(x$deploy)),
                       "NORD",
                       unique(x$year_period),
                       sep = "-")
               )
     })
dev.off()

# ----- Monthly histo per track ----- #
juv_2018_nord$month_numb <- month(juv_2018_nord$Date)
juv_2018_nord_list2 <- split(juv_2018_nord, juv_2018_nord$month_numb)
table(juv_2018_nord$month_numb)
png("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/HISTO_diff_directions/DIFF_DIR_2018_group_NORD_Monthly.png",
    res = 300,
    width = 70,
    height = 50,
    pointsize = 20,
    unit = "cm",
    bg = "white")
par(mfrow = c(3, 4))
lapply(juv_2018_nord_list2, function(x) {
     hist(x$diff_dir,
          breaks = 36,
          main = paste(unique(year(x$deploy)),
                       unique(x$month_numb),
                       "NORD",
                       sep = "-")
               )
     })

dev.off()