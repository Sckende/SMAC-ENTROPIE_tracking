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
 
for(i in 1:12) {
    
    mean_rast <- mean(speed2[[names(speed2)[month(time(speed2)) == i]]])
    
    terra::writeRaster(mean_rast,
                       paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_speed_month_",
                             i,
                             ".tif",
                             sep = ""),
                       overwrite = T)
    print(paste(i, " is done", sep = ""))
}

# ----- #
# Mean values for east
# ----- #

for(i in 1:12) {
    
    mean_rast <- mean(east2[[names(east2)[month(time(east2)) == i]]])
    
    terra::writeRaster(mean_rast,
                       paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_east_month_",
                             i,
                             ".tif",
                             sep = ""),
                       overwrite = T)
    print(paste(i, " is done", sep = ""))
}

# ----- #
# Mean values for north
# ----- #

for(i in 1:12) {
    
    mean_rast <- mean(north2[[names(north2)[month(time(north2)) == i]]])
    
    terra::writeRaster(mean_rast,
                       paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/mean_wind_north_month_",
                             i,
                             ".tif",
                             sep = ""),
                       overwrite = T)
    print(paste(i, " is done", sep = ""))
}
# ----- #
# Stack des valeurs mensuelles par parametres 
# ----- #

m_speed <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/",
                   pattern = "mean_wind_speed_month",
                   full.names = TRUE)
mean_speed <- terra::rast(m_speed)

x11();plot(mean_speed)

# ----- #

m_east <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/",
                   pattern = "mean_wind_east_month",
                   full.names = TRUE)
mean_east <- terra::rast(m_east)

x11();plot(mean_east)

# ----- #

m_north <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/Monthly_wind_2008-2018/",
                   pattern = "mean_wind_north_month",
                   full.names = TRUE)
mean_north <- terra::rast(m_north)

x11();plot(mean_north)
