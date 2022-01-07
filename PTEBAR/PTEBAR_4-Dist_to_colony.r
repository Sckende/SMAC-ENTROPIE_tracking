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
# ---------------------------------------- #
gps <- read.table('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_GPS_DB_V2_noDUPL.txt', h = T, dec = '.', sep = '\t')

head(gps, 10)

# Deletion of fucked gps - PAC04, PAC05, PAC13 -
# ---------------------------------------- #
gps <- gps[!(gps$Logger_ID %in% c('PAC04', 'PAC05', 'PAC13')),]
unique(gps$Logger_ID)

# Time class
# ---------------------------------------- #
gps$time <- as.POSIXct(gps$time)
gps$date <- date(gps$time)

#### Division of tracks based on a gap more than 24h between records ####
# ---------------------------------------- #
# For all data 
results <- list()
for(k in 1:length(unique(gps$Logger_ID))){
  log <- unique(gps$Logger_ID)[k]
  data <- gps[gps$Logger_ID == log,]
  data <- data[!is.na(data$Latitude),]
  
  # data$diff <- c(1, as.vector(diff(data$time))) # lag btw points in MINUTES
  data$diff <- difftime(c(data$time[-1], NA), # dates with a lag of 1
                        data$time,
                        units = 'mins') # difference btw (t+1) - t in minuts
  data$diff[is.na(data$diff)] <- 1
  
  j <- 1
  vec <- NULL
  data$burst <- NA
  
  # --------------------------------------------- #
  for(i in 1:length(data$diff)){
    
    data$burst[i] <- paste(unique(data$Logger_ID), j, sep = '-')
    
    if(data$diff[i] >= 1440){
      
      j <- j + 1
      
    }
  }
  
  # ---------------------------------------- #
  results[[k]] <- list(date.point = data[data$diff > 1440,],
                       summary = table(data$burst, useNA = 'always'),
                       data = data)
  names(results)[k] <- log
}
  
# Visualization on map
# ---------------------------------------- #
# Loading of Reunion Island spatial polygons
run <- st_read("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm0.shp")
project <- st_crs(run)
maps <- list()
for (i in 1:length(results)){
  data <- results[[i]]$data
  DF_spat <- st_as_sf(data,
                      coords = c('Longitude', 'Latitude'),
                      crs = project)
  maps[[i]] <- mapview(DF_spat,
                       zcol = 'burst',
                       burst = T)
  names(maps)[i] <- unique(data$Logger_ID)
}

maps[[1]]
maps[[2]]
maps[[3]]
maps[[4]]
maps[[5]]
maps[[6]]
maps[[7]]

#### Computation of distance from the colony ####
# ---------------------------------------- #
# Merge data in single DF
gps2 <- data.frame()
for (i in 1:length(results)){
  d <- results[[i]]$data
  
  gps2 <- rbind(gps2, d)
}
summary(gps2)
table(gps2$burst)

# Write table with bursts separated by 24h for each track
# ---------------------------------------- #
# write.table(gps2,
#             'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_GPS_bursts.txt',
#             sep = '\t')

# Creation of spatial objects
# ---------------------------------------- #
run <- st_read("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm0.shp")
project <- st_crs(run)
gps2.spat <- st_as_sf(gps2,
                      coords = c('Longitude', 'Latitude'),
                      crs = project)
# Creation of a nest dataframe and merging with gps data
# ---------------------------------------- #

nest <- data.frame(nest_ID = c(108,104,31,30,20,103,100,11,13,12, NA, NA),
                   X_nest = c(336447.0443, 336504.2641, 336502.2784, 336499.6882, 336484.4198, 336484.4198, 336488.4353, 336472.4654, 336472.4654, 336472.4654, NA, NA),
                   Y_nest = c(7663586.004, 7663608.288, 7663609.486, 7663608.796, 7663586.108, 7663586.108, 7663591.191, 7663571.436, 7663571.436, 7663571.436, NA, NA),
                   Logger_ID = c('PAC11', 'PAC20', 'PAC16', 'PAC15', 'PAC04', 'PAC03', 'PAC14', 'PAC10', 'PAC06', 'PAC12', 'PAC05', 'PAC13'))

nest <- nest[!is.na(nest$X_nest),]

# nest <- gps[, c('nest', 'X_nest', 'Y_nest', 'Logger_ID')]
# nest <- nest[!duplicated(nest),]
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
# ---------------------------------------- #
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
