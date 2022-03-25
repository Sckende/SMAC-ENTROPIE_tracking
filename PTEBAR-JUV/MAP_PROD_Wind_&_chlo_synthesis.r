# Production de carte synthétiques aux 3 mois #
# Pour la direction & la force des vents #
# Pour la chlo-a #
# Avec trajet des juveniles (RAGOS) et des adultes (GLS) #

# ---- Load packages ------ #
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

##### ----- Juvenile data - Argos from Pinet 2017 & 2018 ----- ####
# ----- Load data ----- #
juv_argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
juv_argos_sp <- SpatialPointsDataFrame(coords = juv_argos[, c("Longitude", "Latitude")],
                                  data = juv_argos,
                                  proj4string = CRS("+init=epsg:4326"))
summary(juv_argos)
class(juv_argos$Date)

juv_argos$year_period[month(juv_argos$Date) %in% 1:3] <- "JFM"
juv_argos$year_period[month(juv_argos$Date) %in% 4:6] <- "AMJ"
juv_argos$year_period[month(juv_argos$Date) %in% 7:9] <- "JAS"
juv_argos$year_period[month(juv_argos$Date) %in% 10:12] <- "OND"

table(juv_argos$year_period)
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

par(mfrow = c(2, 2))
 for(i in unique(ad_gls$year_period)) {
     plot(x = ad_gls$LON[ad_gls$year_period == i],
          y = ad_gls$LAT[ad_gls$year_period == i],
          main = i,
          xlim = c(min(ad_gls$LON), max(ad_gls$LON)),
          ylim = c(min(ad_gls$LAT), max(ad_gls$LAT)))
 }

#### ----- CHLO-A data ----- ####
# ----------------------------- #

chlo2018 <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/",
                   pattern = "OCEANCOLOUR_GLO_CHL_L4_REP_OBSERVATIONS_009_082-TDS__CHLO-2018",
                   full.names = TRUE)

# ----- Period JANFEVMAR
chlo_2018_1 <- terra::rast(chlo2018[1:3])
# Spatial crop
extend <- extent(30, 120, -50, 10) # xmin, xmax, ymin, ymax
chlo_2018_1 <- crop(chlo_2018_1,
                    extend)


names(chlo_2018_1) <- substr(as.character(terra::time(chlo_2018_1)),
                             1,
                             10)
mean_chlo_2018_1 <- mean(chlo_2018_1)
plot(mean_chlo_2018_1,
     main = "JFM 2018")
points(x = ad_gls$LON[ad_gls$year_period == "JFM"],
     y = ad_gls$LAT[ad_gls$year_period == "JFM"],
     pch = 19,
     col = "red")
points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JFM"],
       y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JFM"],
       pch = 19,
       col = "blue")
# ----- Period AVRMAIJUI
chlo_2018_2 <- terra::rast(chlo2018[4:6])

# Spatial crop
chlo_2018_2 <- crop(chlo_2018_2,
                    extend)

names(chlo_2018_2) <- substr(as.character(terra::time(chlo_2018_2)),
                             1,
                             10)
mean_chlo_2018_2 <- mean(chlo_2018_2)
plot(mean_chlo_2018_2,
     main = "AMJ 2018")
points(x = ad_gls$LON[ad_gls$year_period == "AMJ"],
     y = ad_gls$LAT[ad_gls$year_period == "AMJ"],
     pch = 19,
     col = "red")
points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "AMJ"],
       y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "AMJ"],
       pch = 19,
       col = "blue")
# ----- Period JUIAIUSEP
chlo_2018_3 <- terra::rast(chlo2018[7:9])
# SPatial crop
chlo_2018_3 <- crop(chlo_2018_3,
                    extend)

names(chlo_2018_3) <- substr(as.character(terra::time(chlo_2018_3)),
                             1,
                             10)
mean_chlo_2018_3 <- mean(chlo_2018_3)
plot(mean_chlo_2018_3,
     main = "JAS 2018")
points(x = ad_gls$LON[ad_gls$year_period == "JAS"],
     y = ad_gls$LAT[ad_gls$year_period == "JAS"],
     pch = 19,
     col = "red")
points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JAS"],
       y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JAS"],
       pch = 19,
       col = "blue")

# ----- Period OCTNOVDEC
chlo_2018_4 <- terra::rast(chlo2018[10:12])
# Spatial crop
chlo_2018_4 <- crop(chlo_2018_4,
                    extend)

names(chlo_2018_4) <- substr(as.character(terra::time(chlo_2018_4)),
                             1,
                             10)
mean_chlo_2018_4 <- mean(chlo_2018_4)
plot(mean_chlo_2018_4,
     main = "OND 2018")
points(x = ad_gls$LON[ad_gls$year_period == "OND"],
     y = ad_gls$LAT[ad_gls$year_period == "OND"],
     pch = 19,
     col = "red")
points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "OND"],
       y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "OND"],
       pch = 19,
       col = "blue")

#### ----- Graphical output - CHLO-A ----- ####

png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/PTEBAR_JUV_Carto/Chlo-a_n_birds_2018.png",
    res = 300,
    width = 50,
    height = 40,
    pointsize = 20,
    unit = "cm",
    # bg = "transparent",
    bg = "white")

par(mfrow = c(2, 2))

# ----- AMJ ----- #
plot(mean_chlo_2018_2,
     main = "AMJ 2018")
points(x = ad_gls$LON[ad_gls$year_period == "AMJ"],
     y = ad_gls$LAT[ad_gls$year_period == "AMJ"],
     pch = 19,
     col = "red")
points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "AMJ"],
       y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "AMJ"],
       pch = 19,
       col = "blue")

# ----- JAS ----- #
plot(mean_chlo_2018_3,
     main = "JAS 2018")
points(x = ad_gls$LON[ad_gls$year_period == "JAS"],
     y = ad_gls$LAT[ad_gls$year_period == "JAS"],
     pch = 19,
     col = "red")
points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JAS"],
       y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JAS"],
       pch = 19,
       col = "blue")

# ----- OND ----- #
plot(mean_chlo_2018_4,
     main = "OND 2018")
points(x = ad_gls$LON[ad_gls$year_period == "OND"],
     y = ad_gls$LAT[ad_gls$year_period == "OND"],
     pch = 19,
     col = "red")
points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "OND"],
       y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "OND"],
       pch = 19,
       col = "blue")

# ----- JFM ----- #
plot(mean_chlo_2018_1,
     main = "JFM 2018")
points(x = ad_gls$LON[ad_gls$year_period == "JFM"],
     y = ad_gls$LAT[ad_gls$year_period == "JFM"],
     pch = 19,
     col = "red")
points(x = juv_argos$Longitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JFM"],
       y = juv_argos$Latitude[year(juv_argos$deploy) == 2018 & juv_argos$year_period == "JFM"],
       pch = 19,
       col = "blue")

dev.off()