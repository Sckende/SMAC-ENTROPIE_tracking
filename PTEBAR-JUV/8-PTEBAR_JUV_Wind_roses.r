rm(list = ls())
library(sf)
library(rasterVis)
library(viridis)
library(stringr)
library(terra)
library(raster)
library(viridis)
library(openair)
library(circular)

###################
# Loading data ####
###################

dirs_2017 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_dirs_2017.rds")
dirs_2017 # From -180 to 180

abs_wind_sp_2017 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/abs_wind_speed_2017.rds")

############################################
# Split layers for bimonthly wind roses ####
############################################

# -----> dataframe creation
###########################

head(names(dirs_2017)) # First date = 2017.04.01 00:00
tail(names(dirs_2017)) # Last date = 2017.10.01 00:00

# Dropping the last layer
dirs_2017 <- dropLayer(dirs_2017,
                       nlayers(dirs_2017))
abs_wind_sp_2017 <- dropLayer(abs_wind_sp_2017,
                       nlayers(abs_wind_sp_2017))

wind_rose_2017 <- data.frame()

for(i in 1:nlayers(dirs_2017)){
    
    name <- names(dirs_2017[[i]])
    dirs <- values(dirs_2017[[i]])
    abs_ws <- values(abs_wind_sp_2017[[i]])
    
    df <- data.frame(name = name,
                     dirs = dirs,
                     abs_wd = abs_ws)
    
    wind_rose_2017 <- rbind(wind_rose_2017, df)
    print(i)
}

summary(wind_rose_2017)
write.table(wind_rose_2017,
            "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_roses_2017.txt")

# Réfléchir à une autre stratégie !