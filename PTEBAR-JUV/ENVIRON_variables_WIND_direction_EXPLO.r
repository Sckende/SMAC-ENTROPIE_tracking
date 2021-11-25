# ---------------------------------------------------------------- #
# OBJECTIFS = explorer les données méridionales et zonales du vent à l'échelle de l'océan Indien #
# -------------------------------------------------------------- #

# EXAMPLE ####
rm(list = ls())
library('rasterVis')

# Example of application 
proj <- CRS('+proj=longlat +datum=WGS84')
df <- expand.grid(x = seq(-2, 2, .01), y = seq(-2, 2, .01))

df$z <- with(df, (3*x^2 + y)*exp(-x^2-y^2))
r <- rasterFromXYZ(df, crs=proj)

vectorplot(r, par.settings=RdBuTheme())

streamplot(r, par.settings=RdBuTheme())

atan2(pi/2, 0) * 180/pi

## packages ####
source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r')

## Meridional dataset ####
mer1 <- 'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637651880863_YEAR1_MERIDIONAL.nc'
mer1 <- stack(mer1)
mer1
# class      : RasterStack 
# dimensions : 261, 421, 109881, 653  (nrow, ncol, ncell, nlayers)
# resolution : 0.25, 0.25  (x, y)
# extent     : 24.875, 130.125, -40.125, 25.125  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# names      : X2017.04.06.00.00.00, X2017.04.06.06.00.00, X2017.04.06.12.00.00,...

mer2 <- 'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652499961_YEAR2_MERIDIONAL.nc'
mer2 <- stack(mer2)
mer2
# class      : RasterStack 
# dimensions : 261, 421, 109881, 1129  (nrow, ncol, ncell, nlayers)
# resolution : 0.25, 0.25  (x, y)
# extent     : 24.875, 130.125, -40.125, 25.125  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# names      : X2018.04.03.00.00.00, X2018.04.03.06.00.00, X2018.04.03.12.00.00,...

## Zonal dataset ####
zon1 <- 'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637651769964_YEAR1_ZONAL.nc'
zon1 <- stack(zon1)
zon1
# class      : RasterStack 
# dimensions : 261, 421, 109881, 653  (nrow, ncol, ncell, nlayers)
# resolution : 0.25, 0.25  (x, y)
# extent     : 24.875, 130.125, -40.125, 25.125  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# names      : X2017.04.06.00.00.00, X2017.04.06.06.00.00, X2017.04.06.12.00.00,...

zon2 <- 'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652399617_YEAR2_ZONAL.nc'
zon2 <- stack(zon2)
zon2
# class      : RasterStack 
# dimensions : 261, 421, 109881, 1129  (nrow, ncol, ncell, nlayers)
# resolution : 0.25, 0.25  (x, y)
# extent     : 24.875, 130.125, -40.125, 25.125  (xmin, xmax, ymin, ymax)
# crs        : +proj=longlat +datum=WGS84 +no_defs 
# names      : X2018.04.03.00.00.00, X2018.04.03.06.00.00, X2018.04.03.12.00.00,...

all(names(mer1) == names(zon1))

plot(mer1[[1]])
plot(zon1[[1]])

par(mfrow = c(1, 2))
hist(values(zon1[[1:100]]))
hist(values(mer1[[1:100]]))

## zone de tests ####
u <- zon1[[1]]
v <- mer1[[1]]

vec <- ((180/pi) * atan2(u, v)) + 180
plot(vec)
vectorplot(vec, unit = 'degrees', col.regions = viridis(150), cuts = 149)

# --- #

u <- zon1[[10]]
v <- mer1[[10]]

# vec <- ((180/pi) * atan2(u, v)) + 180
vec <- atan2(u, v) + 180 # pas de conversion en degré -180/pi- car u & v sont déjà en degrées
plot(vec)
vectorplot(vec)

# --- #

u1 <- zon1
v1 <- mer1

u2 <- zon2
v2 <- mer2

vec1 <- atan2(u1, v1) + 180
vec2 <- atan2(u2, v2) + 180

par(mfrow = c(1, 2))
hist(values(vec1)); hist(values(vec2))

mean1 <- mean(vec1)
mean2 <- mean(vec2)

vectorplot(mean1, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = T)
speed1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652088629_YEAR1_SPEED.nc')
speed1 <- mean(speed1)
plot(speed1)
vectorplot(mean1, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = F)
vectorplot(mean1, unit = 'degrees', col.regions = viridis(150), cuts = 149, region = speed1)

vectorplot(mean2, unit = 'degrees', col.regions = viridis(150), cuts = 149)


## FAKIR periode ####
layers2 <- names(vec2)

which(layers2 == 'X2018.04.20.12.00.00') # 71
which(layers2 == 'X2018.04.26.18.00.00') # 96

fakir <- vec[[71:96]]

## FAKIR - Maximal de puissance entre 23 av 18h et 24 av 12h ####

which(layers2 == 'X2018.04.23.18.00.00') # 84
which(layers2 == 'X2018.04.24.12.00.00') # 87

fak.max <- vec2[[84:87]]
plot(fak.max) 

animate(fakir, 0.1)
