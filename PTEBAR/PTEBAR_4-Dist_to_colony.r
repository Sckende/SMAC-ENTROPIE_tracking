# ---------------------------------------------------- #
####----- Distance to the coilony computation -----####
#### Track segmentation & back/forth qualification #### 
# -------------------------------------------------- #

rm(list = ls())
require(sf)
require(dplyr)
require(lubridate)
require(mapview)

setwd("C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR")
source('C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR/PTEBAR_0-Functions_for_scripts.R')

# Starting with no duplicated rows database
gps <- read.table('C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/data/PTEBAR_GPS_DB_V2_noDUPL.txt', h = T, dec = '.', sep = '\t') # database with correct date but with NA and duplicated rows
head(gps, 10)

# Deletion of fucked gps - PAC04, PAC05, PAC13 -
gps <- gps[!(gps$Logger_ID %in% c('PAC04', 'PAC05', 'PAC13')),]
unique(gps$Logger_ID)

# Time class
gps$time <- as.POSIXct(gps$time)
gps$date <- date(gps$time)

#### Division of tracks based on a gap more than 24h between records ####
# For all data 
results <- list()
for(k in 1:length(unique(gps$Logger_ID))){
  log <- unique(gps$Logger_ID)[k]
  data <- gps[gps$Logger_ID == log,]
  data <- data[!is.na(data$Latitude),]
  
  data$diff <- c(1, as.vector(diff(data$time)))
    
    j <- 1
    vec <- NULL
    data$attr <- NA
    
    for(i in 1:length(data$diff)){
      if(data$diff[i] <= 2880){
        vec <- c(vec, i)
      } else {
        data$attr[vec] <- paste(unique(data$Logger_ID), j, sep = '-')
        j <- j + 1
        vec <- i
      }
      if(i == length(data$attr)){
        data$attr[vec] <- paste(unique(data$Logger_ID), j, sep = '-')
      }
    }
    
    results[[k]] <- list(date.point = data[data$diff > 2880,], summary = table(data$attr, useNA = 'always'), data = data)
    names(results)[k] <- log
  }
  
# Visualisation on map
# Loading of Reunion Island spatial polygons
run <- st_read("C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm0.shp")
project <- st_crs(run)
maps <- list()
for (i in 1:length(results)){
  data <- results[[i]]$data
  DF_spat <- st_as_sf(data,
                      coords = c('Longitude', 'Latitude'),
                      crs = project)
  maps[[i]] <- mapview(DF_spat,
                       zcol = 'attr',
                       burst = T)
  names(maps)[i] <- unique(data$Logger_ID)
}


