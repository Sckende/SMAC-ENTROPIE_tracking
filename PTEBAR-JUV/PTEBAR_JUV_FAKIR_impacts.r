rm(list = ls())
library(sf)
library(mapview)
library(lubridate)
library(leafpop)
library(leaflet)

# ------------------------------------------------------------------ #
#### Loading and treatment of ARGOS data  - RAW cleaned localisations ####
# ---------------------------------------------------------------- #
argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.txt",
                    h = T,
                    sep = '\t',
                    dec = '.')
head(argos)
names(argos)[1] <- 'PTT'


# ---- Date class
argos$Date <- as.POSIXct(argos$Date,
                         format = "%Y-%m-%d %H:%M") # Date format
argos$deploy <- as.POSIXct(argos$deploy,
                           format = "%Y-%m-%d %H:%M") # Date format

# Conversion in sf Spatial Object
projLatLon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# UTM 43S corresponding to the center of the Indian ocean
# UTM 43 => 32743 
projUTM <- '+init=epsg:32743'

# Non projected spatial object
argos.sf <- st_as_sf(argos,
                     coords = c('Longitude', 'Latitude'),
                     crs = projLatLon)
class(argos.sf)

# Projected spatial object
argos.sf.UTM <- st_transform(argos.sf,
                             crs = 32743)

# ------------------------------------------------------------------ #
#### Loading and treatment of FAKIR data - RAW cleaned localisations ####
# ---------------------------------------------------------------- #

fakir <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/CYCLONE_Fakir_traj.txt",
                    h = T,
                    sep = '\t',
                    dec = '.')
# ---- Date class
fakir$date <- as.POSIXct(fakir$date,
                         format = "%Y-%m-%d") # Date format
fakir$start.date <- as.POSIXct(fakir$start.date,
                           format = "%Y-%m-%d") # Date format
fakir$end.date <- as.POSIXct(fakir$end.date,
                               format = "%Y-%m-%d") # Date format
summary(fakir)

# Non projected spatial object
fakir.sf <- st_as_sf(fakir,
                     coords = c('lon', 'lat'),
                     crs = projLatLon)

# ------------- #

# REPRENDRE ICI #
# ------------ #
birds_vs_fakir <- argos.sf[argos.sf$Date >= unique(fakir$start.date) & argos$Date >= unique(fakir$end.date),]

birds_vs_fakir <- birds_vs_fakir[year(birds_vs_fakir$deploy) == 2018,]

birds_vs_fakir$PTT <- as.factor(birds_vs_fakir$PTT)

mapview(birds_vs_fakir,
        zcol = 'PTT',
        burst = T)
