# ------------------------------------------------------------------------------------------------- #
# OBJECTIFS - Obtenir la moyenne mensuelle des 3 paramètres de vent entre 2008 & 2018
#           - Produire des cartes à partir de ces paramètres en periode FAV/DEFAV (ou REPRO/HIVERN)    
# -------------------------------------------------------------------------------------------------- #

rm(list = ls())
# -------------------- #
# Packages loading ####
# -------------------- #

source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r')

# ----- #
# Loading data #### 
# ----- #

east <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2019/",
                   pattern = "WIND_GLO_WIND_L4_REP_OBSERVATIONS_012_006-TDS__eastward",
                   full.names = TRUE)
east <- terra::rast(east)
east
names(east) <- as.character(time(east))
names(east)[duplicated(names(east))]
east
east2 <- east[[names(east)[!duplicated(names(east))]]] # retrait des couches dupliquées
east3 <- east2[[-nlyr(east2)]] # retrait de la dernière date : 01-01-2019 00:00
tail(time(east3))
east3

# ----- #

north <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2019/",
                   pattern = "WIND_GLO_WIND_L4_REP_OBSERVATIONS_012_006-TDS__northward",
                   full.names = TRUE)
north <- terra::rast(north)
north
names(north) <- as.character(time(north))
names(north)[duplicated(names(north))]
north
north2 <- north[[names(north)[!duplicated(names(north))]]] # retrait des couches dupliquées
north3 <- north2[[-nlyr(north2)]] # retrait de la dernière date : 01-01-2019 00:00
tail(time(north3))
north3

# ----- #

speed <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2019/",
                   pattern = "WIND_GLO_WIND_L4_REP_OBSERVATIONS_012_006-TDS__wind_speed",
                   full.names = TRUE)
speed <- terra::rast(speed)
speed
names(speed) <- as.character(time(speed))
names(speed)[duplicated(names(speed))]
speed
speed2 <- speed[[names(speed)[!duplicated(names(speed))]]] # retrait des couches dupliquées
speed3 <- speed2[[-nlyr(speed2)]] # retrait de la dernière date : 01-01-2019 00:00
tail(time(speed3))
speed3

names(speed3)[year(time(speed3)) == 2008]
names(speed3[[names(speed3)[month(time(speed3)) == 8]]])

# ----- #
# Mean values for speed ####
# ----- #
 
for(i in 1:12) {
    
    mean_rast <- mean(speed3[[names(speed3)[month(time(speed3)) == i]]])
    
    terra::writeRaster(mean_rast,
                       paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_speed_month_",
                             i,
                             ".tif",
                             sep = ""),
                       overwrite = T)
    print(paste(i, " is done", sep = ""))
}

# ----- #
# Mean values for east ####
# ----- #

for(i in 1:12) {
    
    mean_rast <- mean(east3[[names(east3)[month(time(east3)) == i]]])
    
    terra::writeRaster(mean_rast,
                       paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_east_month_",
                             i,
                             ".tif",
                             sep = ""),
                       overwrite = T)
    print(paste(i, " is done", sep = ""))
}

# ----- #
# Mean values for north ####
# ----- #

for(i in 1:12) {
    
    mean_rast <- mean(north3[[names(north3)[month(time(north3)) == i]]])
    
    terra::writeRaster(mean_rast,
                       paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_north_month_",
                             i,
                             ".tif",
                             sep = ""),
                       overwrite = T)
    print(paste(i, " is done", sep = ""))
}
# ----- #
# Stack des valeurs mensuelles par parametres ####
# ----- #

m_speed <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/",
                   pattern = "mean_wind_speed_month",
                   full.names = TRUE)
mean_speed <- terra::rast(m_speed) # SpatRaster
ms <- raster::stack(m_speed) # RasterStack

x11();plot(mean_speed)
x11();plot(ms)

# ----- #

m_east <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/",
                   pattern = "mean_wind_east_month",
                   full.names = TRUE)
mean_east <- terra::rast(m_east) # SpatRaster
me <- raster::stack(m_east) # RasterStack

x11();plot(mean_east)
x11(); plot(me)

# ----- #

m_north <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/",
                   pattern = "mean_wind_north_month",
                   full.names = TRUE)
mean_north <- terra::rast(m_north) # SpatRaster
mn <- raster::stack(m_north) # RasterStack

# ----- #
# Visualisation ####
# ----- #

x11();plot(mean_north)
x11(); plot(mn)

# zonal = x = east component
# meridional = y = north component
nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev + 1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)
for (i in 1:12) {
    
#    x11()
png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/PTEBAR_JUV_Carto/WIND_2008-2018/",
          i,
          "-WIND_mean_2008-2018_",
          month.name[i],
          ".png",
          sep = ""),
    res = 300,
    width = 50,
    height = 40,
    pointsize = 20,
    unit = "cm",
    # bg = "transparent",
    bg = "white")

   print(vectorplot(raster::stack(me[[i]], mn[[i]]),
                 narrows = 800,
                 aspX = 0.4,
                 isField = 'dXY',
                 region = ms[[i]],
                 at = my_at,
                 col.regions = my_cols,
                 lwd.arrows = 1,
                 colorkey = list(labels = list(cex = 2)),
                 main = list(paste(month.name[i], " - Mean value", sep = ""),
                             cex = 2.5),
                 xlab = list("Longitude",
                             cex = 2.5),
                 ylab = list("Latitude",
                             cex = 2.5))) 
   dev.off()
}

graphics.off()

mean_east[[1]]
levelplot(mean_east)
vectorplot(mean_east)


# ----- #
# Mean values for the MOBILE period (APRIL - MAY) - SPEED ####
# ----- #

mean_rast_speed <- mean(speed3[[names(speed3)[month(time(speed3)) %in% 4:5]]])
terra::writeRaster(mean_rast_speed,
                   "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_speed_mobile_period_2008-2018.tif",
                   overwrite = T)

# ----- #
# Mean values for the MOBILE period (APRIL - MAY) - NORTH ####
# ----- #

mean_rast_north <- mean(north3[[names(north3)[month(time(north3)) %in% 4:5]]])
terra::writeRaster(mean_rast_north,
                   "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_north_mobile_period_2008-2018.tif",
                   overwrite = T)

# ----- #
# Mean values for the MOBILE period (APRIL - MAY) - EAST ####
# ----- #

mean_rast_east <- mean(east3[[names(east3)[month(time(east3)) %in% 4:5]]])
terra::writeRaster(mean_rast_east,
                   "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_east_mobile_period_2008-2018.tif",
                   overwrite = T)

# ---- #
# Visualisation ####
# ----- #
sp_mob <- raster::raster("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_speed_mobile_period_2008-2018.tif")
no_mob <- raster::raster("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_north_mobile_period_2008-2018.tif")
ea_mob <- raster::raster("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_east_mobile_period_2008-2018.tif")

# zonal = x = east component
# meridional = y = north component
nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev + 1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)
x11()
png("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/PTEBAR_JUV_Carto/WIND_2008-2018/Mobile_periode_April-May_2008-2018.png",
    res = 300,
    width = 50,
    height = 40,
    pointsize = 20,
    unit = "cm",
    bg = "white")

   print(vectorplot(raster::stack(ea_mob, no_mob),
                 narrows = 800,
                 aspX = 0.4,
                 isField = 'dXY',
                 region = sp_mob,
                 at = my_at,
                 col.regions = my_cols,
                 lwd.arrows = 1,
                 colorkey = list(labels = list(cex = 2)),
                 main = list("period of mobility April-May 2008-2018",
                             cex = 2.5),
                 xlab = list("Longitude",
                             cex = 2.5),
                 ylab = list("Latitude",
                             cex = 2.5)))
   
   dev.off()


objet <- vectorplot(raster::stack(ea_mob, no_mob),
                 narrows = 800,
                 aspX = 0.4,
                 isField = 'dXY',
                 region = sp_mob,
                 at = my_at,
                 col.regions = my_cols,
                 lwd.arrows = 1,
                 colorkey = list(labels = list(cex = 2))
                 )
terra::writeRaster(objet,
                   "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/TEST.tif",
                   overwrite = T)



# nlev <- 100
# my_at <- seq(from = 0,
#              to = 20,
#              length.out = nlev + 1)
# my_cols <- viridis_pal(begin = 1,
#                        end = 0,
#                        option = "A")(nlev)


# rasterVis::vectorplot(raster::stack(mean_zon2_JAS, mean_mer2_JAS),
#                isField = 'dXY',
#                narrows = 800,
#                lwd.arrows = 1,
#                aspX = 0.4,
#                region = mean_speed2_JAS,
#                at = my_at,
#                col.regions = my_cols,
#                # scales = list(cex = 1.5),
#                colorkey = list(labels = list(cex = 2)),
#                main = list("JAS 2018",
#                            cex = 2.5),
#                xlab = list("Longitude", 
#                            cex = 2.5),
#                ylab = list("Latitude",
#                            cex = 2.5))     +
#     layer(c(sp.points(ad_gls_sp[ad_gls_sp$year_period == "JAS", ],
#                       col = rgb(0, 0, 1, alpha = 0.9),
#                       lwd = 2),
#             sp.points(juv_argos_sp[juv_argos_sp$year_period == "JAS" & year(juv_argos_sp$deploy) == 2018, ],
#                       col = "white",
#                       lwd = 2)),
#           sp.polygons(IndOcean_sp,
#                       col = "grey",
#                       fill = "white"))




x1 <- as.Date("2008-01-01")
x2 <- as.Date("2018-12-31")

x2-x1
