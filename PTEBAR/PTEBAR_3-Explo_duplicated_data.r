# ------------------------------------- #
#### EXPLORATION OF DUPLICATED ROWS ####
# ----------------------------------- #

rm(list = ls())
require(sf)
require(dplyr)

# setwd("C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR")
source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR/PTEBAR_0-Functions_for_scripts.R')

# Starting from the initial database
source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR/PTEBAR_00-Corrections_initial_database.r') # database with correct date but with NA and duplicated rows

summary(gps1)

# Deletion of rows with NA in Longitude
gps1_noNA <- gps1[!is.na(gps1$Longitude),]
summary(gps1_noNA$Longitude)
summary(gps1_noNA$Latitude)

# Selection of the duplicated id ('LoggerTime') based on the 'LoggerTime' variable
gps1_noNA$LoggerTime <- paste(gps1_noNA$Logger_ID, gps1_noNA$time, sep = '')
gps1_noNA$LoggerTime[duplicated(gps1_noNA$LoggerTime)] # 31% of initial data is duplicated

gps_dupl_id <- unique(gps1_noNA$LoggerTime[duplicated(gps1_noNA$LoggerTime)])

# Extraction of the duplicated database & visualization
gps_dupl <- gps1_noNA[gps1_noNA$LoggerTime %in% gps_dupl_id,]

tot <- dplyr::count(gps_dupl, gps_dupl$LoggerTime)
barplot(table(table(gps_dupl$LoggerTime)))

gps_dupl_spat <- st_as_sf(gps_dupl,
                          coords = c('Longitude', 'Latitude'),
                          crs = 4326) # '+init=epsg:4326'
gps_dupl_spat_UTM <- st_transform(gps_dupl_spat,
                                  '+init=epsg:32743')

mapview::mapview(gps_dupl_spat,
                 zcol = 'LoggerTime')

LoggerTime <- vector(length = length(gps_dupl_id))
max_dist <- vector(length = length(gps_dupl_id))
max_dist_UTM <- vector(length = length(gps_dupl_id))
n_dupl <- vector(length = length(gps_dupl_id))

for(i in 1:length(gps_dupl_id)){
  
  dat <- gps_dupl_spat[gps_dupl_spat$LoggerTime == gps_dupl_id[i],]
  dat_UTM <- gps_dupl_spat_UTM[gps_dupl_spat_UTM$LoggerTime == gps_dupl_id[i],]
  
  LoggerTime[i] <- gps_dupl_id[i]
  n_dupl[i] <- nrow(dat)
  max_dist[i] <- max(st_distance(dat))
  max_dist_UTM[i] <- max(st_distance(dat_UTM))
}

info_dupl <- data.frame(LoggerTime, n_dupl, max_dist, max_dist_UTM)
info_dupl

# Dataframe with no NA and no duplicated data

gps2 <- gps1_noNA[!duplicated(gps1_noNA$LoggerTime),]

# write.table(gps2,
            # 'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_GPS_DB_V2_noDUPL.txt')

