# --------------------------- #
# OBJECTIVE - Trajectometry analysis
# INITIAL DATA - "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED_speed.rds"
# PREVIOUS SCRIPT - 1-PTEBAR_JUV_Vitesse.r
# LITTERATURE - 2016_de Grissac_et_al_Contrasting movement strategies among juvenile albatrosses and petrels
#             - 2006_The package “adehabitat” for the R software: A tool for the analysis of space and habitat use by animals
#             - 2009_Calenge_et_al_The concept of animals' trajectories from a data analysis perspective
# Orientation , daily distance travelled, sinuosity and range
#            - 2015_Calenge_Analysis of animal movement in R: the adehabitatLT package 

# --------------------------- #
rm(list = ls())

# ---- PACKAGES ----

library(mapview)
library(dplyr)
library(sf)
library(lubridate)
library(adehabitatHR)
library(adehabitatLT)
library(rgdal)
library(sp)

# ---- DATA ----
argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED_speed.rds")
str(argos)

# ---- WORKING on two devices ----
# ar <- do.call('rbind', argos[1:2])
ar <- argos[[1]]
# hist(table(ar$point.group))

#### Retrieve the lat/long coordinates from sf object
coords <- st_coordinates(ar)
coords.DF <- as.data.frame(coords)
names(coords.DF) <- c('lon', 'lat')

#### Convert lat/long coordinates to UTM
coords <- SpatialPoints(coords,
                        proj4string = CRS('+proj=longlat')) # conversion in sp object
coord.UTM <- spTransform(coords,
                         CRS('+init=epsg:32742')) # Transforming coordinate to UTM using EPSG=32742 for WGS=84, UTM Zone=42S
coord.UTM.DF <- as.data.frame(coord.UTM)

#### Complete coordinates in dataframe
ar <- cbind(ar, coords.DF, coord.UTM.DF)

#### Deletion of the sf geometry column
ar <- as.data.frame(ar)[, - length(names(ar))]

#### Specific to the work on a single device ####
# Deletion of unused factor level
ar <- droplevels(ar)

# Burst number as factor to split the trajectory in a next step
ar$burst <- as.factor(as.character(ar$point.group))
ar$burst <- factor(as.character(ar$burst),
                   levels = 1:max(ar$point.group))
summary(ar)

#### Conversion to ltraj object ####
ar.traj <- as.ltraj(xy = ar[, c('X', 'Y')], # UTM coordinates for meter unitin distance computation
               date = ar$Date,
               id = ar$burst) # Trajectory split based on the burst of points

ar.traj # Note that dx,dy,dist are expressed in the units of the coordinates x,y (here, Degrees, Minutes and Seconds), abs.angle,rel.angle are expressed in radians and dt is in seconds

#### Trajectory parameters ####
 ar.traj[[1]]
 ar.traj[[2]]
 ar.traj[[3]]
 ar.traj[[4]]
 plot(ar.traj[1:12])
 plot(ar.traj[13:24]) 
 plot(ar.traj[25:36]) 
 plot(ar.traj[37:48]) 
 plot(ar.traj[49:60]) 
 plot(ar.traj[61:69]) 

 t <- do.call('rbind', ar.traj) 
head(t) 

#### Speed conversion ####
t$speed <- t$dist / t$dt # Speed in m/s
t$speed.km.h <- t$speed * 0.001 / (1/3600)
par(mfrow = c(1, 2)); barplot(t$speed); barplot(speed2)
summary(t$speed); summary(t$speed.km.h)

#### Check for speed outliers ####

out <- t[t$speed.km.h > 40,]
out <- out[!is.na(out$speed.km.h),]
