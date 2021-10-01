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

#### Loading and treatment of data 1 - METADATA #### 
infos_argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Infos_deploiement.txt",
                        h = T,
                        sep = "\t")
names(infos_argos)

infos_argos$deploy <- as.POSIXct(infos_argos$deploy,
                                 format = "%d/%m/%Y %H:%M") # Date format - This is the most important date here since the devices started before the deployment
summary(infos_argos)


#### Loading and treatment of data 2 - RAW cleaned localisations ####
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

class(argos$Date)
summary(argos$Date)
barplot(table(argos$Date))
year(head(argos$Date))
table(year(argos$Date))

# ---- Summary of device recordings

arg_bil <- argos %>% group_by(PTT) %>% 
  summarise(n_loc = n(),
            min_date = date(min(Date)),
            max_date = date(max(Date)),
            max_lat = max(Latitude),
            min_lat = min(Latitude),
            max_lon = max(Longitude),
            min_lon = min(Longitude))

# ---- Addition of the area of the MCP (Minimum Complex Polygon) for each individuals

# Spatial Dataframe conversion
reun <- data.frame(Longitude = 55.42, Latitude = -21.12)
reun <- SpatialPoints(reun)
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

argos.sf <- st_as_sf(argos,
                     coords = c('Longitude', 'Latitude'),
                     crs = projcrs) # step 1
class(argos.sf)


coords <- SpatialPoints(argos[, c('Longitude', 'Latitude')])
argos.sp <- SpatialPointsDataFrame(coords, argos)

class(argos.sp)

# Computation of the Minimum Convex Polygon with adehabitatHR

cp <- mcp(argos.sp[, 1],
          percent = 95,
          unin = 'km',
          unout = 'km2')
plot(cp)
plot(argos.sp, add = TRUE)

plot(cp[cp$id == '162071',])
plot(argos.sp[argos$PTT == '162071',], add = TRUE, col = 'darkgrey')
plot(reun, col = 'red', add = T) # BUG HERE !




#### TO CONTINUE FROM HERE #####



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
  
# ---------------------------------------------- #
#### Extra information for each trajectories ####
# -------------------------------------------- #

# Tracking duration in hours & in days
# From the deployment date to the last date of recording
  
  arg_bil2$duration_trip_day <- arg_bil2$max_date - arg_bil2$deploy  
  
# Max distance from Reunion island and total distance traveled 
  argos_sp_list <- split(argos_sp, argos_sp$Vessel)
  
  # library(pbapply) # progress bar for apply functions
  # argos_dist_mat <- pblapply(argos_sp_list, st_distance) # Matrix distance for each device - *** WARNING *** Long process
  # saveRDS(argos_dist_mat, "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Distance_matrices.rds")
  # st_distance() computes the distance between each points based on the great circle distances method (take the curvature of the earth into account)
  
  argos_dist_mat <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Distance_matrices.rds") # import distance matrices for juvenile petrels
  matrix_data <- data.frame()
  
  for(i in 1:length(argos_dist_mat)){
    id <- names(argos_dist_mat[i]) # name of the device
    matr <- argos_dist_mat[[i]] # distance matrix for the device
    
    max_dist <- max(matr[1,]) # max of the first row of the matrix, i.e. distance between the first point (breeding colony at Reunion Island) and each following points
    loc_max_dist <- which(matr[1,] == max_dist) # Location or point number where the maximal distance from the Reunion Island is obtained
    
    matr_diag_sec <- diag(matr[, -1]) # secondary diagonal of the matrix containing the distance between each consecutive points
    dist_travel <- max(cumsum(matr_diag_sec)) # max of the cumulative sum for obtaining the total traveled distance
    
    matrix_data <- rbind(matrix_data, c(id, max_dist, loc_max_dist, dist_travel))
  }
names(matrix_data) <- c('Vessel', 'max_dist', 'loc_max_dist', 'dist_travel')
matrix_data$max_dist_km <- as.numeric(matrix_data$max_dist)/1000 # conversion in km
matrix_data$dist_travel_km <- as.numeric(matrix_data$dist_travel)/1000 # conversion in km
View(matrix_data)  
  
# Timing to reach the maximal distance from the breeding colony

for (i in 1:nrow(matrix_data)){
  id <- matrix_data$Vessel[i]
  loc <- as.numeric(matrix_data$loc_max_dist[i])
  
  matrix_data$date_loc[i] <- as.character(argos_sp$Date[argos_sp$Vessel == id][loc]) # date corresponding to the nth loc where the maximal distance is reached
  matrix_data$date_deploy[i] <- unique(as.character(argos2.qual$deploy[argos2.qual$Vessel == id])) # deployment date of the argos device
}

matrix_data$date_loc <- as.POSIXct(matrix_data$date_loc,
                         format = "%Y-%m-%d %H:%M:%S") # Date format

matrix_data$date_deploy <- as.POSIXct(matrix_data$date_deploy,
                           format = "%Y-%m-%d %H:%M:%S") # Date format

matrix_data$timing_for_max <- matrix_data$date_loc - matrix_data$date_deploy

# Combine the both summary df ... 
arg_bil2$Vessel <- as.character(arg_bil2$Vessel)
total <- left_join(arg_bil2, matrix_data, by = 'Vessel')

# ... and addition of the spatial lines
track_lines$Vessel <- as.character(track_lines$Vessel)

# total_sp <- st_as_sf(total,
#                      wkt = 'geometry')
total_sp <- left_join(track_lines[, c('Vessel', 'geometry')], total, by = 'Vessel' )
# Save the df
# saveRDS(total_sp,
# "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Infos_bilan.rds")
