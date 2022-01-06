# ---------------------------------------------------- #
####----- Distance to the colony computation -----####
#### Track segmentation & back/forth qualification #### 
# -------------------------------------------------- #

rm(list = ls())
require(sf)
require(dplyr)
require(lubridate)
require(mapview)

source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR/PTEBAR_0-Functions_for_scripts.R')

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
  
# Visualization on map
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


#### Computation of distance from the colony ####
# Merge data in single DF
gps2 <- data.frame()
for (i in 1:length(results)){
  d <- results[[i]]$data
  
  gps2 <- rbind(gps2, d)
}
summary(gps2)

# Creation of spatial objects
run <- st_read("C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm0.shp")
project <- st_crs(run)
gps2.spat <- st_as_sf(gps2,
                      coords = c('Longitude', 'Latitude'),
                      crs = project)
# nest DF
nest <- gps[, c('nest', 'X_nest', 'Y_nest', 'Logger_ID')]
nest <- nest[!duplicated(nest),]
nest
nest_spat <- st_as_sf(nest,
                      coords = c('X_nest', 'Y_nest'),
                      crs = project)

gps2.spat.list <- split(gps2.spat, gps2.spat$Logger_ID)

for (i in 1:length(gps2.spat.list)){
  dist_colo <- NA
  behav <- NA
  data <- gps2.spat.list[[i]]
  log <- names(gps2.spat.list)[i]
  p.nest <- nest_spat$geometry[nest_spat$Logger_ID == log]
  
  for(j in 1:length(data$Logger_ID)){
    p1 <- data$geometry[j]
    dist_colo[j] <- max(st_distance(c(p.nest, p1)))
  }
  gps2.spat.list[[i]]$dist_colo <- dist_colo
  
  for(k in 2:length(data$Logger_ID)){
    if(dist_colo[k] >= dist_colo[k - 1]){
      behav[k] <- 'FORTH'
    } else {
      behav[k] <- 'BACK'    
    }}
  gps2.spat.list[[i]]$behav <- behav  
}

mapview(gps2.spat.list[[5]],
        zcol = 'behav')


# Write data for rmarkdown documents
  # Retrieve the gps points
# rmdoc <- do.call('rbind', gps2.spat.list)
# rmdoc$behav[is.na(rmdoc$behav)] <- 'FORTH'
# rmdoc1 <- cbind(rmdoc, recup_coord(rmdoc$geometry))
# 
# write.table(rmdoc1,
#             'C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/data/PTEBAR_GPS_behav.txt',
#             sep = '\t')

test <- read.table('C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/data/PTEBAR_GPS_behav.txt',
                   h = T,
                   sep = '\t')
head(test)
test.spat <- st_as_sf(test,
                      coords = c('Longitude', 'Latitude'),
                      crs = project)
mapview(test.spat[test.spat$Logger_ID == 'PAC10',],
        zcol = 'behav')
unique(test.spat$Logger_ID)

head(test.spat)

mapview(test.spat[test.spat$local == 'land',],
        zcol = 'behav')

test.spat[is.na(test.spat$behav),]
