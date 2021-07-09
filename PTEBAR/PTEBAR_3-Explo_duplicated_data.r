# ------------------------------------- #
#### EXPLORATION OF DUPLICATED ROWS ####
# ----------------------------------- #

rm(list = ls())
require(sf)

setwd("C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR")
source('C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR/PTEBAR_0-Functions_for_scripts.R')

# Starting from the initial database
source('C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR/PTEBAR_00-Corrections_initial_database.R') # database with correct date but with NA and duplicated rows

# Deletion of rows with NA in Longitude
gps1_noNA <- gps1[!is.na(gps1$Longitude),]
summary(gps1_noNA$Longitude)
summary(gps1_noNA$Latitude)

# Selection of the duplicated id ('LoggerTime') based on the 'LoggerTime' variable
gps1_noNA$LoggerTime <- paste(gps1_noNA$Logger_ID, gps1_noNA$time, sep = '')
gps1_noNA$LoggerTime[duplicated(gps1_noNA$LoggerTime)] # 31% of initial data is duplicated

gps_dupl_id <- unique(gps1_noNA$LoggerTime[duplicated(gps1_noNA$LoggerTime)])

# Extraction of the duplicated database
gps_dupl <- gps1_noNA[gps1_noNA$LoggerTime %in% gps_dupl_id,]

gps_dupl_spat <- st_as_sf(gps_dupl,
                          coords = c('Longitude', 'Latitude'),
                          crs = 4326) # '+init=epsg:4326'

mapview::mapview(gps_dupl_spat,
                 zcol = 'LoggerTime')


bobynco <- vector(length = length(gps_dupl_id))
for(i in 1:length(gps_dupl_id)){
  boby <- max(st_distance(gps_dupl_spat[gps_dupl_spat$LoggerTime == gps_dupl_id[i],]))
  
  bobynco[i] <- boby
}

table(bobynco)



