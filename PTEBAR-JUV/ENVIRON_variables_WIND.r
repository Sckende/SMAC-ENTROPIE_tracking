
# Objectifs - Obtenir des cartes de force et de direction des vents dans la zone de trajet des juveniles PTEBAR au départ de la colonie avec superposition des trajet

rm(list = ls())
# packages ####
source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r')

# Polygon for bird relocations ####
# Creation d'un polygone avec 100% des localisations + une zone tampon de 3000 m (le double de l'erreur max de localisation)

argos <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.rds')
class(argos)
mapview(argos)

# STEP 1 
step1 <- concaveman(argos) # Contour des points les plus externes
plot(step1)

# STEP 2
step2 <- st_buffer(step1, 200000) # Définition d'un buffer
plot(step2, add = T)

# STEP 3
step3 <- st_as_sfc(st_bbox(step2, crs = st_crs(step2))) # Quadrilatère qui contient le contour de points avec buffer
# objet de class sfc
plot(step3, add = T)
step3 <- st_as_sf(step3) # Conversion du sfc en sf

mapview(step3)

# netCDF files ####

library(ncdf4)
# Speed wind pour la premiere annee de deploiement - 2017
# From 2017-04-06 00.00.00 to 2017-09-16 00.00.00
year1 <- 'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652088629_YEAR1_SPEED.nc'
stack1 <- stack(year1) # preferer la fonction stack() a raster() quand un objet a plusieurs couches comme c'est le cas ici
stack1 # 653 layers
plot(stack1[[c(1, 653)]])
plot(range(stack1))
plot(mean(stack1))

# Speed wind pour la deuxieme annee de deploiement - 2018
# From 2018-04-03 00.00.00 to 2019-01-10 00.00.00
year2 <- 'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637653002349_YEAR2_SPEED.nc'
stack2 <- stack(year2)
stack2 # 1129 layers
plot(stack2[[c(1, 1129)]])
plot(range(stack2))
plot(mean(stack2))

# Ne considérer que la zone des pétrels - Voir si c'est utile ?
st_crs(s) == st_crs(step3) # verification des crs qui doivent être identique

step3 <- st_transform(step3, 
                    4326) # code correspondant au crs du raster & plus simple de convertir le polygone que le raster, auquel cas, des erreurs peuvent apparaître dans les données

plot(stack1[[1]])
plot(st_geometry(step3), add = T)

p0 <- raster::mask(stack1, step3) # masque tout ce qui n'est pas contenu dans le polygon

plot(p0[[3]])

# FAKIR periode
names(stack2[[1]])
layers2 <- names(stack2)

which(layers2 == 'X2018.04.20.12.00.00') # 71
which(layers2 == 'X2018.04.26.18.00.00') # 96

fakir <- stack2[[71:96]]

# FAKIR - Maximal de puissance entre 23 av 18h et 24 av 12h

which(layers2 == 'X2018.04.23.18.00.00') # 84
which(layers2 == 'X2018.04.24.12.00.00') # 87
 
fak.max <- stack2[[84:87]]
plot(fak.max) 

animate(fakir, 0.1)

# Envol des juveniles - 2017 
layers1 <- names(stack1)
which(layers1 == 'X2017.04.06.00.00.00') # 1
which(layers1 == 'X2017.04.30.00.00.00') # 97

windEnvol2017 <- stack1[[1:97]]
animate(windEnvol2017, 0.05)

# Envol des juveniles - 2018 

which(layers2 == 'X2018.04.03.00.00.00') # 1
which(layers2 == 'X2018.04.30.00.00.00') # 109

windEnvol2018 <- stack2[[1:109]]
animate(windEnvol2018, 0.05)

# ----------------------------------- #
# Periode de depart des juveniles ####
# --------------------------------- #

class(argos$Date)

# STEP 1 - Ne conserver que le mois d'avril
dep_list <- split(argos, year(argos$deploy))

lapply(dep_list, summary)

dep_list2 <- lapply(dep_list, function(x){
  
  x <- x[month(x$Date) == 4,]
  x
})

lapply(dep_list2, summary)

mapview(dep_list2, zcol = 'Vessel', burst = T)

# STEP 2 - PTEBAR - Diviser le mois d'avril en 5 periodes pour chaque année
# Period   Range
#   1    1 -> 7
#   2    8 -> 14
#   3    15 -> 21
#   4    21 -> 28
#   5    29 -> 30

# Rapide visualisation
dep <- do.call('rbind', dep_list2)
dep$deployYear <- year(dep$deploy)
dep$deployYear <- as.factor(dep$deployYear)
dep$Period <- NA
dep$period[day(dep$Date) < 8] <- 1
dep$period[day(dep$Date) %in% 8:14] <- 2
dep$period[day(dep$Date) %in% 15:21] <- 3
dep$period[day(dep$Date) %in% 22:28] <- 4
dep$period[day(dep$Date) > 28] <- 5
table(dep$period)

mapview(dep[dep$period == 1,], zcol = 'deployYear')
mapview(dep[dep$period == 2,], zcol = 'deployYear')
mapview(dep[dep$period == 3,], zcol = 'deployYear')
mapview(dep[dep$period == 4,], zcol = 'deployYear')
mapview(dep[dep$period == 5,], zcol = 'deployYear')

mapview(stack2[[1]]) + mapview(dep[dep$period == 1,], zcol = 'deployYear')

# Application sur tout le jeux de donnees

dep_list3 <- split(dep, dep$deployYear)

dep_list4_pts <- lapply(dep_list3, function(x){
  x <- split(x, x$period)
  x
})

# Conversion en tracks
dep_list4_tracks <- lapply(dep_list4_pts, function(y){
  
  y <- lapply(y, function(x){
    
    x <- x %>% group_by(Vessel) %>% 
      summarize(do_union = F) %>% 
      st_cast('LINESTRING')
  })
})

# Visualisation 2017
mapview(dep_list4_tracks[[1]][[1]], zcol = 'Vessel', burst = T)
mapview(dep_list4_tracks[[1]][[2]], zcol = 'Vessel', burst = T)
mapview(dep_list4_tracks[[1]][[3]], zcol = 'Vessel', burst = T)
mapview(dep_list4_tracks[[1]][[4]], zcol = 'Vessel', burst = T)
mapview(dep_list4_tracks[[1]][[5]], zcol = 'Vessel', burst = T)

# Visualisation 2018
mapview(dep_list4_tracks[[2]][[1]], zcol = 'Vessel', burst = T)
mapview(dep_list4_tracks[[2]][[2]], zcol = 'Vessel', burst = T)
mapview(dep_list4_tracks[[2]][[3]], zcol = 'Vessel', burst = T)
mapview(dep_list4_tracks[[2]][[4]], zcol = 'Vessel', burst = T)
# mapview(dep_list4_tracks[[2]][[5]], zcol = 'Vessel', burst = T)

# STEP 3 - WIND - Diviser le mois d'avril en 5 periodes pour chaque année avec une moyenne de la vitesse des vents pour chaque periode
# Period   Range
#   1    1 -> 7
#   2    8 -> 14
#   3    15 -> 21
#   4    21 -> 28
#   5    29 -> 30

# 2017
  # periode 1
which(layers1 == 'X2017.04.07.00.00.00') #5
wind1p1 <- mean(stack1[[1:5]])
# periode 2
which(layers1 == 'X2017.04.14.00.00.00') #33
wind1p2 <- mean(stack1[[6:33]])
# periode 3
which(layers1 == 'X2017.04.21.00.00.00') #61
wind1p3 <- mean(stack1[[34:61]])
# periode 4
which(layers1 == 'X2017.04.28.00.00.00') #89
wind1p4 <- mean(stack1[[62:89]])
# periode 3
which(layers1 == 'X2017.04.30.00.00.00') #97
wind1p5 <- mean(stack1[[90:97]])

# 2018
# periode 1
which(layers2 == 'X2018.04.07.00.00.00') #17
wind2p1 <- mean(stack2[[1:17]])
# periode 2
which(layers2 == 'X2018.04.14.00.00.00') #45
wind2p2 <- mean(stack2[[18:45]])
# periode 3
which(layers2 == 'X2018.04.21.00.00.00') #73
wind2p3 <- mean(stack2[[46:73]])
# periode 4
which(layers2 == 'X2018.04.28.00.00.00') #101
wind2p4 <- mean(stack2[[74:101]])
# periode 5
which(layers2 == 'X2018.04.30.00.00.00') #109
wind2p5 <- mean(stack2[[102:109]])

mapview(wind1p1) + mapview(dep_list4_tracks[[1]][[1]])
mapview(wind1p2) + mapview(dep_list4_tracks[[1]][[2]])
mapview(wind1p3) + mapview(dep_list4_tracks[[1]][[3]])
mapview(wind1p4) + mapview(dep_list4_tracks[[1]][[4]])
mapview(wind1p5) + mapview(dep_list4_tracks[[1]][[5]])

mapview(wind2p1) + mapview(dep_list4_tracks[[2]][[1]])
mapview(wind2p2) + mapview(dep_list4_tracks[[2]][[2]])
mapview(wind2p3) + mapview(dep_list4_tracks[[2]][[3]])
mapview(wind2p4) + mapview(dep_list4_tracks[[2]][[4]])
mapview(wind2p5) + mapview(dep_list4_tracks[[2]][[5]])
