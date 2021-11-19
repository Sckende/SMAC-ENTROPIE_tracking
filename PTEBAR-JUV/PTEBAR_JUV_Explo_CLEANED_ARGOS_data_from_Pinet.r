# Exploration des données Argos déployées sur des juvéniles pétrel de Barau en avril 2017 et 2018
# Data à l'origine du papier de Weimerskirch et al 2016 - Wettability of juvenile plumage as a major cause of mortality threatens endangered Barau’s petrel
# Utilisation des données récupérées auprès de P. Pinet

rm(list = ls())

library('mapview')
library('leaflet')
library('leafpop')
library('sf')
library('sp')
library('lubridate')
library('dplyr')
library('adehabitatHR')
library('viridis')
library('viridisLite')

# ------------------------------------------------- #
#### Loading and treatment of data 1 - METADATA #### 
# ----------------------------------------------- #

infos_argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Infos_deploiement.txt",
                          h = T,
                          sep = "\t")
names(infos_argos)

infos_argos$deploy <- as.POSIXct(infos_argos$deploy,
                                 format = "%d/%m/%Y %H:%M") # Date format - This is the most important date here since the devices started before the deployment
summary(infos_argos)

infos_argos2 <- infos_argos[, -9]
infos_argos2 <- infos_argos2[, c(2, 1, 3:8)]

# --------------- #
#### RMD file ####
# ------------- #
# saveRDS(infos_argos2,
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/infos_argos.rds')

# ------------------------------------------------------------------ #
#### Loading and treatment of data 2 - RAW cleaned localisations ####
# ---------------------------------------------------------------- #
argos <- do.call('rbind', readRDS('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.rds'))
head(argos)
names(argos)[1] <- 'PTT'
# ---- Date class
argos$Date <- as.POSIXct(argos$Date,
                         format = "%Y-%m-%d %H:%M") # Date format
argos$deploy <- as.POSIXct(argos$deploy,
                           format = "%Y-%m-%d %H:%M") # Date format

class(argos$Date)
summary(argos$Date)
barplot(table(argos$Date))
year(head(argos$Date))
table(year(argos$Date))

# ---- Retrieve the coords from sf object

coords1 <- st_coordinates(argos)
coords2 <- as.data.frame(coords1)
names(coords2) <- c('Longitude', 'Latitude')
argos <- cbind(argos, coords2)

# ---- Summary of device recordings

arg_bil <- argos %>% group_by(PTT) %>% 
  summarise(n_loc = n(),
            min_date = date(min(Date)),
            max_date = date(max(Date)),
            max_lat = max(Latitude),
            min_lat = min(Latitude),
            max_lon = max(Longitude),
            min_lon = min(Longitude))

# ---------------------------------------------- #
#### Extra information for each trajectories ####
# -------------------------------------------- #

infos_argos2$PTT <- as.factor(infos_argos2$PTT)
arg_bil2 <- left_join(arg_bil,
                      infos_argos2[, c(2, 3)],
                      by = 'PTT')

# ---- Tracking duration in days
# From the deployment date to the last date of recording

arg_bil2$duration_trip_day <- arg_bil2$max_date - date(arg_bil2$deploy)  

# ---- Max distance from Reunion island and total distance traveled 
# Conversion in sf Spatial Object
projLatLon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# UTM 43S corresponding to the center of the Indian ocean
# UTM 43 => 32743 
projUTM <- '+init=epsg:32743'

# Projected spatial object for computing distance matrice in meter
argos <- st_transform(argos,
                      crs = 32743)

coords3 <- as.data.frame(st_coordinates(argos))
names(coords3) <- c('X', 'Y')

argos <- cbind(argos, coords3)

# Distance matrice computation
argos_sf_list <- split(argos, argos$PTT)

argos_dist_mat <- lapply(argos_sf_list, st_distance) # Matrix distance for each device - *** WARNING *** Long process
# st_distance() computes the distance between each points based on the great circle distances method (take the curvature of the earth into account)

# saveRDS(argos_dist_mat, "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Distance_matrices.rds")


# argos_dist_mat <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Distance_matrices.rds") # import distance matrices for juvenile petrels

matrix_data <- data.frame()

for(i in 1:length(argos_dist_mat)){
  id <- names(argos_dist_mat[i]) # name of the device
  matr <- argos_dist_mat[[i]] # distance matrix for the device
  
  max_dist <- max(matr[1,]) # max of the first row of the matrix, i.e. distance between the first point (breeding colony at Reunion Island) and each following points
  loc_max_dist <- min(which(matr[1,] == max_dist)) # Location or point number where the maximal distance from the Reunion Island is obtained
  
  matr_diag_sec <- diag(matr[, -1]) # secondary diagonal of the matrix containing the distance between each consecutive points
  # dist_travel <- max(cumsum(matr_diag_sec))
  dist_travel <- sum(matr_diag_sec) # max of the cumulative sum for obtaining the total traveled distance
  
  matrix_data <- rbind(matrix_data, c(id, max_dist, loc_max_dist, dist_travel))
}
names(matrix_data) <- c('PTT', 'max_dist', 'loc_max_dist', 'dist_travel')
matrix_data$max_dist_km <- as.numeric(matrix_data$max_dist)/1000 # conversion in km
matrix_data$dist_travel_km <- as.numeric(matrix_data$dist_travel)/1000 # conversion in km
View(matrix_data)  

# ---- Timing to reach the maximal distance from the breeding colony

for (i in 1:nrow(matrix_data)){
  id <- matrix_data$PTT[i]
  loc <- as.numeric(matrix_data$loc_max_dist[i])
  
  matrix_data$date_loc[i] <- as.character(argos$Date[argos$PTT == id][loc]) # date corresponding to the nth loc where the maximal distance is reached
  matrix_data$date_deploy[i] <- unique(as.character(infos_argos2$deploy[infos_argos2$PTT == id])) # deployment date of the argos device
}

matrix_data$date_loc <- as.POSIXct(matrix_data$date_loc,
                                   format = "%Y-%m-%d %H:%M:%S") # Date format

matrix_data$date_deploy <- as.POSIXct(matrix_data$date_deploy,
                                      format = "%Y-%m-%d %H:%M:%S") # Date format

matrix_data$timing_for_max <- date(matrix_data$date_loc) - date(matrix_data$date_deploy)

# ---- Combine the both summary df 
matrix_data$PTT <- as.factor(matrix_data$PTT)
arg_bil3 <- left_join(arg_bil2,
                      matrix_data[, c(1, 5, 6, 9)],
                      by = 'PTT')
arg_bil3$dep_year <- year(arg_bil3$deploy)
arg_bil3 <- arg_bil3[order(arg_bil3$deploy),]
# -------------- #
#### RMD file ####
# -------------- #
# saveRDS(arg_bil3,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_Bilan_ARGOS_data.rds")

# ------------------------------------------------------ #
#### Production of Spatial Trackline object for maps ####
# ---------------------------------------------------- #
argos.sf.track <- argos %>%
  group_by(PTT) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") # Creation of SF LINESTRINGS

mapview(argos.sf.track,
        zcol = 'PTT',
        burst = T,
        legend = T)

# ---------------- #
#### RMD files ####
# -------------- #

# saveRDS(argos,
# "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_Spatial_points_ARGOS.rds")

# saveRDS(argos.sf.track,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_Spatial_tracks_ARGOS.rds")

# ---------------------------------------- #
#### Visualisation of relocation class ####
# -------------------------------------- #

table(argos$Class, useNA = 'always')
argos$Class <- as.factor(argos$Class)
argos$Class <- ordered(argos$Class, levels = c('B', 'A', '0', '1', '2', '3'))

mapview(argos,
        zcol = 'Class',
        # col.regions = c('darkred', 'red', 'orange', 'yellow', 'green', 'darkgreen'),
        # col.regions = viridis,
        burst = T)
#### A REPRENDRE ICI AVEC LES DONNEES COMPLETES ####
# ----------------------------- #
#### Minimum Convex Polygon ####
# --------------------------- #

# ---- Computation of the area of the MCP (Minimum Complex Polygon) for each individuals

# Spatial Points Data frame creation to use in mcp function
coords <- SpatialPoints(argos[, c('Longitude', 'Latitude')],
                        proj4string = CRS(projLatLon))
coords.UTM <- spTransform(coords,
                          CRS(projUTM))
argos.sp <- SpatialPointsDataFrame(coords = coords,
                                   data = argos)
argos.sp.UTM <- SpatialPointsDataFrame(coords = coords.UTM,
                                   data = argos)
class(argos.sp)

# Computation and plot of the Minimum Convex Polygon with adehabitatHR
# *** WARNING **** Have to use UTM coordinates to have the good units
PTT <- unique(argos$PTT)

cpUTM <- mcp(argos.sp.UTM[, 1],
          percent = 100,
          unin = 'm',
          unout = 'km2')
cpUTM
mapview(cpUTM,
        zcol = 'id',
        burst = T)
cpUTM95 <- mcp(argos.sp.UTM[, 1],
             percent = 95,
             unin = 'm',
             unout = 'km2')

cpUTM80 <- mcp(argos.sp.UTM[, 1],
             percent = 80,
             unin = 'm',
             unout = 'km2')

# saveRDS(cpUTM,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_Minimum_convex_polygons100.rds")
# saveRDS(cpUTM95,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_Minimum_convex_polygons95.rds")
# saveRDS(cpUTM80,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_Minimum_convex_polygons80.rds")

# Combine polygon and points for individual plot

mapview(cpUTM[cpUTM$id == '166572',], legend = F) + mapview(argos.sf[argos.sf$PTT == '166572',], legend = F)

mapview(list(cpUTM[cpUTM$id == '166572',], argos.sf[argos.sf$PTT == '166572',]), col.regions = viridis(length(PTT))[6],
        layer.name = c('Minimim Convex Polygon', 'Relocations'))

# ------------------------------------------ #
#### Home Range size for all individuals ####
# ---------------------------------------- #
kud <- kernelUD(argos.sp.UTM,
                h = 'href', 
                # h = 0.1,
                grid = 500)
kud@h
KUDvol <- getvolumeUD(kud)
ver90 <- getverticeshr(KUDvol, 90)
ver80 <- getverticeshr(KUDvol, 80)
ver70 <- getverticeshr(KUDvol, 70)
ver60 <- getverticeshr(KUDvol, 60)
ver50 <- getverticeshr(KUDvol, 50)

verHREF <- list()
v <- c(90, 80, 70, 60, 50)

for(i in 1:length(v)){
  verHREF[[i]] <- getverticeshr(KUDvol, v[i])
}

# saveRDS(verHREF,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_kernelHREF.rds")

# ----- #
kudLSCV <- kernelUD(argos.sp.UTM,
                    h = 'LSCV',
                    grid = 500)
kudLSCV@h
kudLSCVvol <- getvolumeUD(kudLSCV)
ver90LSCV <- getverticeshr(kudLSCVvol, 90)
ver80LSCV <- getverticeshr(kudLSCVvol, 80)
ver70LSCV <- getverticeshr(kudLSCVvol, 70)

verLSCV <- list()
v <- c(90, 80, 70, 60, 50)

for(i in 1:length(v)){
  verLSCV[[i]] <- getverticeshr(kudLSCVvol, v[i])
}

kud1 <- kernelUD(argos.sp,
                h = 1, # ici 1 degré (relié au type de projection, si lat/lon (non projeté) = degré, si UTM (projeté) = m) en rapport à la précision des GLS env. 180km (1deg = 111m) - pour ARGOS précision environ 1km, donc 1/100 degré
                grid = 500) # ici correspond 500x500 degrés (1deg = 111 km à l'équateur)
KUDvol1 <- getvolumeUD(kud1)
ver90.1 <- getverticeshr(KUDvol1, 90)
ver80.1 <- getverticeshr(KUDvol1, 80)
ver70.1 <- getverticeshr(KUDvol1, 70)

mapview(list(ver90.1, ver80.1, ver70.1),
        col.regions = viridis(n = 3, alpha = 0.5),
        layer.name = c('kern90', 'kern80', 'kern70'))

kud2 <- kernelUD(argos.sp,
                h = 2,
                grid = 500)
KUDvol2 <- getvolumeUD(kud2)
ver90.2 <- getverticeshr(KUDvol2, 90)
ver80.2 <- getverticeshr(KUDvol2, 80)
ver70.2 <- getverticeshr(KUDvol2, 70)

mapview(list(ver90.2, ver80.2, ver70.2),
        col.regions = viridis(n = 3, alpha = 0.5),
        layer.name = c('kern90', 'kern80', 'kern70'))


mapview(list(ver90, ver80, ver70, ver60, ver50),
        col.regions = viridis(n = 5, alpha = 0.5)) +
  mapview(argos.sp,
          cex = 1,
          col.regions = 'black')


mapview(list(ver90LSCV, ver80LSCV, ver70LSCV),
        col.regions = c('red', 'orange', 'green'))


# ------------------------------------- #
#### Home Range size per individual ####
# ----------------------------------- #
unique(argos.sp.UTM$PTT)

shortPTT <- c(162070,162072,162073,166561,166563,166564,166565,166568,166569,166572) # deletion of PTTs 162071, 166570, 166571, 166573, 166566

THElist <- list()

for(i in 1:length(shortPTT)){
  x <- argos.sp.UTM[argos.sp.UTM$PTT == shortPTT[i],]
  kud <- kernelUD(x,
                  h = 'href',
                  grid = 500)
  
  THElist[[i]] <- list()
  names(THElist)[i] <- shortPTT[i]
  v <- c(90, 80, 70, 60, 50)
  
  for(j in 1:length(v)){
    THElist[[i]][[j]] <- getverticeshr(kud, v[j])
  }
}

# saveRDS(THElist,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_LIST_ind_kernelHREF.rds")



mapview(THElist[[1]],
        col.regions = viridis(n = length(THElist[[1]]), alpha = 0.5)) +
  mapview(argos.sp[argos.sp$PTT == names(THElist)[1],],
          cex = 1,
          col.regions = 'black')


mapview(THElist[[2]],
        col.regions = viridis(n = length(THElist[[2]]), alpha = 0.5)) +
  mapview(argos.sp[argos.sp$PTT == names(THElist)[2],],
          cex = 1,
          col.regions = 'black')













































# ------------------------------------------------------------ #
#### Rapid visual exploration of extrapolated trajectories ####
# ---------------------------------------------------------- #
argos.raw.list <- lapply(arg.list, function(x){
  x <- x[!duplicated(x$Date),]
})

argos.raw <- do.call('rbind', argos.raw.list)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
argos.raw.sp <- sf::st_as_sf(argos.raw,
                             coords = c('Longitude', 'Latitude'),
                             crs = projcrs)
argos.raw.sp$Vessel <- as.factor(argos.raw.sp$Vessel)

mapview(argos.raw.sp,
        zcol = 'Vessel',
        burst = T,
        legend = F)

argos.raw.track <- argos.raw.sp %>%
  group_by(Vessel) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") # Creation of SF LINESTRINGS

mapview(argos.raw.track,
        zcol = 'Vessel',
        burst = T,
        legend = F)
# ------------------------------------------------------------ #
#### Rapid visual exploration of extrapolated trajectories ####
# ---------------------------------------------------------- #

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
argos_sp <- sf::st_as_sf(argos2.qual,
                         coords = c('Longitude', 'Latitude'),
                         crs = projcrs)

# argos_sp$Vessel <- as.factor(argos_sp$Vessel)

track_lines <- argos_sp %>%
  group_by(Vessel) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") # Creation of SF LINESTRINGS

track_lines <- left_join(track_lines, arg_bil2, by = 'Vessel')

track_lines$popup_info <- paste0("<b>PTT</b> ",
                                 track_lines$Vessel,
                                 "<br/>",
                                 "<b>Durée de l'enregistrement </b>",
                                 track_lines$max_date - track_lines$deploy)
# saveRDS(track_lines,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_track_lines_data.rds")

argos_sp$Vessel <- as.factor(argos_sp$Vessel)
track_lines$Vessel <- as.factor(track_lines$Vessel)

mapview(argos_sp,
        zcol = 'Vessel',
        burst = T,
        homebutton = F) 
# saveRDS(map_points,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_raw_map_points.rds")
# + 

mapview(track_lines,
        zcol = 'Vessel',
        # homebutton = F,
        popup = popupTable(track_lines,
                           zcol = 'popup_info',
                           feature.id = FALSE,
                           row.numbers = FALSE,
                           className = 'mapview-popup'),
        burst = TRUE
)
# saveRDS(map_lines,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_raw_map_lines.rds")

# ------------------------------------------------------- #
#### Rapid visual exploration of trajectories on land ####
# ----------------------------------------------------- #
run <- st_read("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm0.shp")

in_run <- st_intersection(argos_sp, run)

# saveRDS(in_run,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_in_Run_points_data.rds")

mapview(in_run,
        zcol = 'Vessel',
        burst = T,
        homebutton = F)
# mapview(track_lines[track_lines$Vessel == '166568',]) # Back and forth from Reunion Island before to go toward Tanzania


