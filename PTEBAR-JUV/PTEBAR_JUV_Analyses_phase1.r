# ------------------------------------------------------------------------ #
#### First steps for the trajecory characterization of juvenile PTEBAR ####
# ---------------------------------------------------------------------- #

rm(list = ls())

library('mapview')
library('leaflet')
library('leafpop')
library('sf')
library('lubridate')
library('dplyr')
library('adehabitatHR')

#### Loading and treatment of data #### 
argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_TRACK_argos_CLEANED.txt",
                 h = T,
                 sep = '\t',
                 dec = '.')[, c(1:4, 9, 17)]
row.names(argos) <- NULL
head(argos)
summary(argos)

argos$Vessel <- as.factor(argos$Vessel)


class(argos$Date)
head(argos$Date)

argos$Date <- as.POSIXct(argos$Date,
                         format = '%Y-%m-%d %H:%M:%S')
argos$deploy <- as.POSIXct(argos$deploy,
                           format = '%Y-%m-%d %H:%M:%S') # Date format

#### Map production per individuals - Overlap btw points and tracklines ####
argos.lines.sp <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_track_lines_data.rds')
class(argos.lines.sp)

argos.ls <- split(argos, argos$Vessel)
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

lapply(argos.ls, function(x){
  x.sp <- st_as_sf(x,
                   coords = c('Longitude', 'Latitude'),
                   crs = projcrs)
  mapview(x.sp, popup = popupTable(x.sp, zcol = x.sp$Date)) 
  # + mapview(argos.lines.sp[argos.lines.sp$Vessel == unique(x$Vessel),])
})

mapview(x.sp,
        popup = popupTable(x.sp[, c('Vessel', 'Date')],
                           # zcol = c('Vessel', 'Date'),
                           row.numbers = F,
                           feature.id = F))
