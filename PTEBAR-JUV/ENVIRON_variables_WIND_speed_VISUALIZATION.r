# ----------------------------------------------------------------------- #
# OBJECTIFS - Superposition des données de vitesse et de direction (basée sur les composantes zonales & méridionales) sur des cartes divisé par périodes de dispersion des juvéniles #
# --------------------------------------------------------------------- #

rm(list = ls())

# Packages loading ####
source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r')

# Data loading ####
argos <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.rds')

## YEAR 1 - 2017 ####
speed1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652088629_YEAR1_SPEED.nc')

zon1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637651769964_YEAR1_ZONAL.nc')
mer1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637651880863_YEAR1_MERIDIONAL.nc')

vec1 <- 180/pi * atan2(zon1, mer1) + 180
hist(values(vec1))

argos1 <- argos[year(argos$deploy) == 2017,]


## YEAR 2 - 2018 ####

speed2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637653002349_YEAR2_SPEED.nc')

zon2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652399617_YEAR2_ZONAL.nc')
mer2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652499961_YEAR2_MERIDIONAL.nc')

vec2 <- 180/pi * atan2(zon2, mer2) + 180
hist(values(vec2))

argos2 <- argos[year(argos$deploy) == 2018,]

# AVRIL 2017 & 2018 ####

## Locations points ####
argos.list <- split(argos, year(argos$deploy))

arg.envol <- lapply(argos.list, function(x){
  
  x <- x[month(x$Date) == 4,]
})

arg.env.period <- lapply(arg.envol, function(y){
  
  y$period[day(y$Date) < 8] <- 1
  y$period[day(y$Date) %in% 8:14] <- 2
  y$period[day(y$Date) %in% 15:21] <- 3
  y$period[day(y$Date) %in% 22:28] <- 4
  y$period[day(y$Date) > 28] <- 5
  y
})

lapply(arg.env.period, function(x){
  table(x$period)
  # sum(table(x$period))
  
})
arg.env.period <- lapply(arg.env.period, function(x){
  x <- split(x, x$period)
  x
})
## WIND speed & trajectory per period ####
layersSpeed1 <- names(speed1) 
layersVec1 <- names(vec1)

layersSpeed2 <- names(speed2)
layersVec2 <- names(vec2)

# period 1 - from 1 to 7 APRIL
all(layersSpeed1 == layersVec1)
which(layersSpeed1 == 'X2017.04.07.00.00.00')#5

sp1Per1 <- mean(speed1[[1:5]])
vec1Per1 <- mean(vec1[[1:5]])

y1period1 <- vectorplot(vec1Per1, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = sp1Per1)

# ---- #

all(layersSpeed2 == layersVec2)
which(layersSpeed2 == 'X2018.04.07.00.00.00')#17

sp2Per1 <- mean(speed2[[1:17]])
vec2Per1 <- mean(vec2[[1:17]])

y2period1 <- vectorplot(vec2Per1, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = sp2Per1)

# period 2 - from 8 to 14 APRIL

which(layersSpeed1 == 'X2017.04.14.00.00.00')#33

sp1Per2 <- mean(speed1[[18:33]])
vec1Per2 <- mean(vec1[[18:33]])

y1period2 <- vectorplot(vec1Per2, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = sp1Per2)

# ---- #

which(layersSpeed2 == 'X2018.04.14.00.00.00')#45

sp2Per2 <- mean(speed2[[18:45]])
vec2Per2 <- mean(vec2[[18:45]])

y2period2 <- vectorplot(vec2Per2, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = sp2Per2)

# period 3 - from 15 to 21 APRIL

which(layersSpeed1 == 'X2017.04.21.00.00.00')#61

sp1Per3 <- mean(speed1[[34:61]])
vec1Per3 <- mean(vec1[[34:61]])

y1period3 <- vectorplot(vec1Per3, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = sp1Per3)

# ---- #

which(layersSpeed2 == 'X2018.04.21.00.00.00')#73

sp2Per3 <- mean(speed2[[46:73]])
vec2Per3 <- mean(vec2[[46:73]])

y2period3 <- vectorplot(vec2Per3, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = sp2Per3)

# period 4 - from 22 to 28 APRIL

which(layersSpeed1 == 'X2017.04.28.00.00.00')#89

sp1Per4 <- mean(speed1[[62:89]])
vec1Per4 <- mean(vec1[[62:89]])

y1period4 <- vectorplot(vec1Per4, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = sp1Per4)

# ---- #

which(layersSpeed2 == 'X2018.04.28.00.00.00')#101

sp2Per4 <- mean(speed2[[74:101]])
vec2Per4 <- mean(vec2[[74:101]])

y2period4 <- vectorplot(vec2Per4, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = sp2Per4)

# period 5 - from 29 to 30 APRIL

which(layersSpeed1 == 'X2017.04.30.00.00.00')#97

sp1Per5 <- mean(speed1[[90:97]])
vec1Per5 <- mean(vec1[[90:97]])

y1period5 <- vectorplot(vec1Per5, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = sp1Per5)

# ---- #

which(layersSpeed2 == 'X2018.04.30.00.00.00')#109

sp2Per5 <- mean(speed2[[102:109]])
vec2Per5 <- mean(vec2[[102:109]])

y2period5 <- vectorplot(vec2Per5, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = sp2Per5)

par(mfrow = c(1,2
              ))
y1period1; y1period2

boby.sp <- as(st_transform(arg.env.period[[1]][[1]], 4326), 'Spatial')
class(boby.sp)

proj4string(boby.sp)
proj4string(vec1Per1)
 # Dispersion avril 2017
y1period1 +
  layer(sp.points(boby.sp))

y1period2 +
  layer(sp.points(as(st_transform(arg.env.period[[1]][[2]], 4326), 'Spatial')))

y1period3 +
  layer(sp.points(as(st_transform(arg.env.period[[1]][[3]], 4326), 'Spatial')))

y1period4 +
  layer(sp.points(as(st_transform(arg.env.period[[1]][[4]], 4326), 'Spatial')))

y1period5 +
  layer(sp.points(as(st_transform(arg.env.period[[1]][[5]], 4326), 'Spatial')))


# Dispersion avril 2018
y2period1 +
  layer(sp.points(as(st_transform(arg.env.period[[2]][[1]], 4326), 'Spatial')))

y2period2 +
  layer(sp.points(as(st_transform(arg.env.period[[2]][[2]], 4326), 'Spatial')))

y2period3 +
  layer(sp.points(as(st_transform(arg.env.period[[2]][[3]], 4326), 'Spatial')))

y2period4 +
  layer(sp.points(as(st_transform(arg.env.period[[2]][[4]], 4326), 'Spatial')))

# y2period5 +
#   layer(sp.points(as(st_transform(arg.env.period[[2]][[5]], 4326), 'Spatial')))
