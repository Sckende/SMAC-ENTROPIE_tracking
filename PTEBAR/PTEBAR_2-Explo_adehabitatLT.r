rm(list = ls())
source('C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR/PTEBAR_0-Functions_for_scripts.R')
require(adehabitatLT)
require(lubridate)

gps <- read.table("C:/Users/Etudiant/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_GPS_DB_V2_adehabLT.txt", h = T, sep = '\t', dec = ".") # Non duplicated data with NAs in lat/Long
summary(gps)
names(gps)

# Conversion of dates
# gps$time <- as.POSIXct(strptime(as.character(gps$time),
#                                 '%Y-%m-%d %H:%M:%S'),
#                        tz = 'Indian/Mauritius')
# head(gps$time)
# class(gps$time)

gps$time <- strptime(paste(gps$time), "%Y-%m-%d %H:%M:%S")
gps$time <- as.character(gps$time)

gps$time <- as.POSIXct(gps$time, tz="Indian/Mauritius") 

# gps$Logger_ID <- as.factor(gps$Logger_ID)
gps1 <- gps[gps$Logger_ID == 'PAC10',]
gps2 <- gps[gps$Logger_ID == 'PAC12',]
# Check for duplicated date
splitdata <- split(gps, gps$Logger_ID)
splitdupz = lapply(splitdata, function(birdup)
  which(duplicated(birdup$time)))
str(splitdupz)

# creation of an object of class ltraj to store movements
ptebar <- as.ltraj(xy = gps[, c('Longitude', 'Latitude')],
                  date = gps$time,
                  id = gps$Logger_ID)

plot(ptebar)
x11(); plotltr(ptebar, 'dt')
