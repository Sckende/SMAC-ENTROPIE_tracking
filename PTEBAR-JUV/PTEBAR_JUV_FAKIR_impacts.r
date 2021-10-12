rm(list = ls())
library(sf)
library(mapview)
library(lubridate)
library(leafpop)
library(leaflet)
library(dplyr)

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
fakir$date2 <- paste(fakir$date, fakir$hour, sep = ' ')
fakir$date2 <- as.POSIXct(fakir$date2,
                         format = "%Y-%m-%d %H:%M") # Date format
fakir$start.date <- as.POSIXct(fakir$start.date,
                           format = "%Y-%m-%d") # Date format
fakir$end.date <- as.POSIXct(fakir$end.date,
                               format = "%Y-%m-%d") # Date format
summary(fakir)

# Non projected spatial object
fakir.sf <- st_as_sf(fakir,
                     coords = c('lon', 'lat'),
                     crs = projLatLon)
# Projected Spatial object
# Projected spatial object
fakir.sf.UTM <- st_transform(fakir.sf,
                             crs = 32740)

fakir.sf$CI <- as.factor(fakir.sf$CI)
fakir.sf.UTM$CI <- as.factor(fakir.sf.UTM$CI)
mapview(fakir.sf.UTM,
        zcol = 'CI',
        col.regions = c('green', 'green', 'green', 'yellow', 'yellow', 'orange', 'orange', 'red', 'red'))

mapview(st_buffer(fakir.sf.UTM, dist = 150000, joinStyle = 'ROUND'),
        zcol = 'CI',
        col.regions = c('green', 'green', 'green', 'yellow', 'yellow', 'orange', 'orange', 'red', 'red'))

# ------------- #

start <- min(fakir$date2)
end <- max(fakir$date2)

argos2 <- argos[year(argos$deploy) == 2018,]
argos3 <- argos2[argos2$Date >= start & argos2$Date <= end,]

# Non projected spatial object
argos3.sf <- st_as_sf(argos3,
                     coords = c('Longitude', 'Latitude'),
                     crs = projLatLon)

argos3.sf$PTT <- as.factor(argos3.sf$PTT)
mapview(argos3.sf,
        zcol = 'PTT',
        burst = T) + mapview(st_buffer(fakir.sf.UTM, dist = 150000, joinStyle = 'ROUND'),
                             zcol = 'CI',
                             col.regions = c('green', 'green', 'green', 'yellow', 'yellow', 'orange', 'orange', 'red', 'red'), 
                             layer.name = 'Cyclone intensity')
# date max
for(i in unique(argos3$PTT)){
  print(max(argos3$Date[argos3$PTT == i]))
}

# ----- #

bird.Fak <- argos.sf[argos.sf$PTT %in% unique(argos3$PTT) & argos.sf$Date < '2018-05-06',]
bird.Fak$PTT <- as.factor(bird.Fak$PTT)

bird.Fak.Tracks <- bird.Fak %>%
  group_by(PTT) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING")


mapview(bird.Fak,
        burst = T,
        zcol = 'PTT',
        # col.regions = 'black',
        cex = 3,
        legend = F) + mapview(bird.Fak.Tracks,
                              burst = T) + mapview(st_buffer(fakir.sf.UTM, dist = 150000, joinStyle = 'ROUND'),
                                                         zcol = 'CI',
                                                         col.regions = c('green', 'green', 'green', 'yellow', 'yellow', 'orange', 'orange', 'red', 'red'),
                                                         layer.name = 'Cyclone intensity')

# ----- #

# category depending on the intensity of cyclone

for(i in 1:length(bird.Fak$PTT)){
  if(bird.Fak$Date[i] <= '2018-04-22 00:00' | bird.Fak$Date[i] > '2018-04-24 18:00'){
    
    bird.Fak$Cycl_cat[i] <- 1
  } else
  
  if(bird.Fak$Date[i] > '2018-04-22 00:00' & bird.Fak$Date[i] <= '2018-04-23 00:00'){
    
    bird.Fak$Cycl_cat[i] <- 2
  } else
    
    if(bird.Fak$Date[i] > '2018-04-23 00:00' & bird.Fak$Date[i] <= '2018-04-23 12:00'){
      
      bird.Fak$Cycl_cat[i] <- 3
    } else
      
      if(bird.Fak$Date[i] > '2018-04-23 12:00' & bird.Fak$Date[i] <= '2018-04-24 12:00'){
        
        bird.Fak$Cycl_cat[i] <- 4
      } else
        
        if(bird.Fak$Date[i] > '2018-04-24 12:00' & bird.Fak$Date[i] <= '2018-04-24 18:00'){
          
          bird.Fak$Cycl_cat[i] <- 3
        }  
}

bird.Fak$Cycl_cat <- as.factor(bird.Fak$Cycl_cat)

mapview(bird.Fak[bird.Fak$PTT == '162070',],
        zcol = 'Cycl_cat',
        col.regions = c('green', 'yellow', 'orange', 'red')
        ) + mapview(bird.Fak.Tracks[bird.Fak.Tracks$PTT == '162070',],
                    burst = T) + mapview(st_buffer(fakir.sf.UTM, dist = 150000, joinStyle = 'ROUND'),
                                         zcol = 'CI',
                                         col.regions = c('green', 'green', 'green', 'yellow', 'yellow', 'orange', 'orange', 'red', 'red'),
                                         layer.name = 'Cyclone intensity')
