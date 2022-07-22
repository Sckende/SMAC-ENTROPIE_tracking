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
# Loading data 
# ----- #

east <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2019/",
                   pattern = "WIND_GLO_WIND_L4_REP_OBSERVATIONS_012_006-TDS__eastward",
                   full.names = TRUE)
east <- terra::rast(east)
east
names(east) <- as.character(time(east))
names(east)[duplicated(names(east))]
east
east2 <- east[[names(east)[!duplicated(names(east))]]]
east2
# ----- #
north <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2019/",
                   pattern = "WIND_GLO_WIND_L4_REP_OBSERVATIONS_012_006-TDS__northward",
                   full.names = TRUE)
north <- terra::rast(north)
north
names(north) <- as.character(time(north))
names(north)[duplicated(names(north))]
north
north2 <- north[[names(north)[!duplicated(names(north))]]]
north2
# ----- #
speed <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2019/",
                   pattern = "WIND_GLO_WIND_L4_REP_OBSERVATIONS_012_006-TDS__wind_speed",
                   full.names = TRUE)
speed <- terra::rast(speed)
speed
names(speed) <- as.character(time(speed))
names(speed)[duplicated(names(speed))]
speed
speed2 <- speed[[names(speed)[!duplicated(names(speed))]]]
speed2

names(speed2)[year(time(speed2)) == 2008]
names(speed2[[names(speed2)[month(time(speed2)) == 8]]])

# ----- #
# Mean values for speed
# ----- #
 
# for(i in 1:12) {
    
#     mean_rast <- mean(speed2[[names(speed2)[month(time(speed2)) == i]]])
    
#     terra::writeRaster(mean_rast,
#                        paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_speed_month_",
#                              i,
#                              ".tif",
#                              sep = ""),
#                        overwrite = T)
#     print(paste(i, " is done", sep = ""))
# }

# ----- #
# Mean values for east
# ----- #

# for(i in 1:12) {
    
#     mean_rast <- mean(east2[[names(east2)[month(time(east2)) == i]]])
    
#     terra::writeRaster(mean_rast,
#                        paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_east_month_",
#                              i,
#                              ".tif",
#                              sep = ""),
#                        overwrite = T)
#     print(paste(i, " is done", sep = ""))
# }

# ----- #
# Mean values for north
# ----- #

# for(i in 1:12) {
    
#     mean_rast <- mean(north2[[names(north2)[month(time(north2)) == i]]])
    
#     terra::writeRaster(mean_rast,
#                        paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_north_month_",
#                              i,
#                              ".tif",
#                              sep = ""),
#                        overwrite = T)
#     print(paste(i, " is done", sep = ""))
# }
# ----- #
# Stack des valeurs mensuelles par parametres 
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