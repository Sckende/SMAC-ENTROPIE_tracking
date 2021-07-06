# Exploration of the raw GPS data of PTEBAR
# Device deployment in 2018, December

rm(list = ls())
source('C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR/PTEBAR_0-Functions_for_scripts.R')
require(lubridate)

gps <- read.csv2("C:/Users/Etudiant/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_GPS_all.csv", dec = ".")
summary(gps)
names(gps)

#### Creation of 'time' variable ####
gps$time <- paste(paste(gps$Year, gps$Month, gps$Day, sep = "-"), paste(gps$Hour, gps$Minute, gps$Second, sep = ":"), sep = " ")
gps$time <- as.POSIXct(gps$time)

names(gps)
#### here keeping only date, ID, lat & long, speed, searching_time, Voltage ####
gps1 <- gps[, c(2:13, 29)]

#### Addition of a 'period' variable

gps1$period[month(gps1$time) %in% 1:4] <- 'rearing'
gps1$period[month(gps1$time) %in% 5:8] <- 'winter'
gps1$period[month(gps1$time) %in% 9:10] <- 'prosp'
gps1$period[month(gps1$time) %in% 11:12] <- 'incub'

table(gps1$period, useNA = 'always')

#### Line correction with winter period - Modification of DATE and PERIOD
gps1[gps1$period == "winter",]

gps1$time[gps1$period == "winter"] <- as.POSIXct('2018-12-18 11:33:00')
gps1$period[gps1$period == "winter"] <- 'incub'

table(gps1$period, useNA = 'always') # check point

#### For each logger, % of missing data, max/min speed, ... ####
#all(is.na(gps$Latitude) == is.na(gps$Longitude)) # check point

gps_list1 <- split(gps1, gps1$Logger_ID)

# NA summary
bilan <- data.frame()

for (i in 1:length(gps_list1)){
  log_ID <- unique(gps_list1[[i]]$Logger_ID)
  point_numb <- nrow(gps_list1[[i]])
  NA_numb <- length(gps_list1[[i]]$Latitude[is.na(gps_list1[[i]]$Latitude)])
  prop_NA <- round(NA_numb/point_numb*100, digits = 1)
  
  bilan <- rbind(bilan, c(log_ID, point_numb, NA_numb, prop_NA))
  
}
names(bilan) <- c('log_ID', 'point_numb', 'NA_numb', 'prop_NA')
bilan[order(as.numeric(bilan$point_numb)),]

#### Delete the duplicated rows for DATE/TIME based on the lower searching_time ####
bilan2 <- data.frame()
gps_list1.1 <- list()

for(i in 1:length(gps_list1)){
  g <- gps_list1[[i]][order(gps_list1[[i]]$time, gps_list1[[i]]$Searching_time, decreasing = F),]
  g <- g[!duplicated(g$time),]
  t <- g[!is.na(g$Latitude),]
  
  
  log_ID <- unique(t$Logger_ID)
  point_numb <- nrow(t)
  time_min <- as.character(min(t$time))
  time_max <- as.character(max(t$time))
  speed_min <- min(t$Speed)
  speed_max <- max(t$Speed)
  
  bilan2 <- rbind(bilan2, c(log_ID, point_numb, time_min, time_max, speed_min, speed_max))
  gps_list1.1[[i]] <- g
  names(gps_list1.1)[i] <- unique(t$Logger_ID)
}
names(bilan2) <- c('log_ID', 'point_numb', 'time_min', 'time_max', 'speed_min', 'speed_max')
bilan2[order(as.numeric(bilan2$point_numb)),]

str(gps_list1.1)
gps1.1 <- do.call('rbind', gps_list1.1)
#which(duplicated(paste(gps1.1$time, gps1.1$Logger_ID))) # check point

#### Addition of the nest coordinates ####

nest <- read.table('C:/Users/Etudiant/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_coord_nests_logger.txt', dec = '.', sep = '\t', h = T)

# Conversion of nest coordinates in from UTM to decimal
require(sf)
require(sp)
nest_utm <- SpatialPoints(cbind(nest$Lon_nest, nest$Lat_nest), 
                          proj4string=CRS("+init=epsg:32740")) #epsg correspnding to the UTM 40 S
#spTransform(utm, CRS("+proj=longlat +datum=WGS84"))
LonLat <- spTransform(nest_utm, CRS('+init=epsg:4326')) # epsg corresponding to the mondial decimal reference
class(LonLat)
nest1 <- cbind(nest, LonLat)
names(nest1)[5:6] <- c('X_nest', 'Y_nest')

gps1.2 <- merge(gps1.1, nest1[, c('Logger_ID', 'nest', 'X_nest', 'Y_nest')], all.x = T)
head(gps1.2)

#### Addition of the at sea/on land status ####

# Split the DF depending on NA in lon/lat data
DF_NA <- gps1.2[is.na(gps1.2$Longitude),]
DF <- gps1.2[!is.na(gps1.2$Longitude),]

# Loading of Reunion Island spatial polygons
run <- st_read("C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm0.shp")
project <- st_crs(run)

DF_spat <- st_as_sf(DF,
                    coords = c('Longitude', 'Latitude'),
                    crs = project)
on_run <- st_intersection(run, DF_spat)
#mapview(on_run)

DF_spat$local[DF_spat$geometry %in% on_run$geometry] <- 'land'
DF_spat$local[is.na(DF_spat$local)] <- 'sea'
table(DF_spat$local)
require(mapview)
mapview(DF_spat[DF_spat$local == 'land',])

#### Addition of the inside/outside the colony status ####

# Loading the colony shapefile and conversion of the crs 
protec_col <- st_read("C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/APB_PTEBAR/APB_PTEBAR.shp") # 1 = Petite Ile & 2 = GBN + PTN
protect_col_split <- st_cast(protec_col, 'POLYGON') # Coercion from multipolygon to polygon
protect_GBN <- protect_col_split[3,]
st_crs(protect_GBN) == project # UTM 
jo <- as(protect_GBN, 'Spatial') # coercion from sf to sp in order to ...
protect_GBN_dec <- spTransform(jo, # mofify the crs ...
                               CRS('+init=epsg:4326')) # epsg corresponding to the mondial decimal reference
protect_GBN_dec <- st_as_sf(protect_GBN_dec) # sf object

st_crs(protect_GBN_dec) == project

# inside vs outside the colony
in_col <- st_intersection(protect_GBN_dec, DF_spat)
# mapview(in_col) + mapview(protect_GBN_dec)

DF_spat$colony[DF_spat$geometry %in% in_col$geometry] <- 'in'
DF_spat$colony[is.na(DF_spat$colony)] <- 'out'
table(DF_spat$colony)

mapview(DF_spat[DF_spat$colony == 'in',]) + mapview(protect_GBN_dec)
mapview(DF_spat[DF_spat$Logger_ID == 'PAC03',],
        zcol = 'colony')

# **** MERGE WITH THE NA DF **** ####
# In order to obtain a complete df
DF_NA$local <- NA
DF_NA$colony <- NA

DF1 <- as.data.frame(DF_spat)
coord <- recup_coord(DF_spat$geometry)

DF2 <- cbind(DF1[, 1:7], Latitude = coord$Latitude, Longitude = coord$Longitude, DF1[, 8:15], DF1[, 17:18])

DF_fusion <- rbind(DF2, DF_NA)
DF_fin <- DF_fusion[order(DF_fusion$Logger_ID, DF_fusion$time, decreasing = F),]

# write.table(DF_fin, 'C:/Users/Etudiant/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_GPS_DB_V2_adehabLT.txt', sep = '\t')

#### DELETION of PAC04, PAC13 & PAC05 ####
# Due to low number of GPS fixes
k <- c('PAC04', 'PAC13', 'PAC05')
no <- setdiff(names(gps_list1), k)
gps_list2 <- gps_list[no] # keeping list levels with data of interest

#### Visual explo ####

require(mapview)

# data conversion in SF LINESTRING

#gps2 <- gps[!(gps$Logger_ID %in% c('PAC04', 'PAC13', 'PAC05')),]
# gps2 <- gps2[!is.na(gps2$Latitude),]

gps2 <- do.call('rbind', gps_list2)

gps2 <- gps2[!is.na(gps2$Latitude),]

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
gps2 <- sf::st_as_sf(gps2,
                     coords = c('Longitude', 'Latitude'),
                     crs = projcrs)
head(gps2)

# Creation of SF LINESTRINGS
require(tidyverse)
require(sf)
require(mapview)
track_lines <- gps2 %>% group_by(Logger_ID) %>% summarize(do_union = FALSE) %>% st_cast("LINESTRING")

mapview(gps2) + mapview(track_lines, zcol = 'Logger_ID', burst = T, homebutton = F)

# Loading of Reunion Island spatial polygons
run <- st_read("C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm0.shp")

mapview(track_lines, zcol = 'Logger_ID', burst = T, homebutton = F) + mapview(run)

#### Extract points inside of the Reunion Island ####

# Points inside the island only
in_run <- st_intersection(gps2, run)
in_run$set <- paste(in_run$Logger_ID, in_run$Year, in_run$Month, in_run$Day, sep="")
in_run$set2 <- paste(in_run$Logger_ID, in_run$time, sep="")
names(in_run)

mapview(in_run,
        zcol = 'set')

#### Merge the at-sea/on land information for gps points ####
gps2$set2 <- paste(gps2$Logger_ID, gps2$time, sep = "") 
gps2$run_loc[gps2$set2 %in% unique(in_run$set2)] <- 'in'
gps2$run_loc[is.na(gps2$run_loc)] <- 'out'


# track_lines_out <- gps2[gps2$run_loc == 'out',] %>% group_by(Logger_ID) %>% summarize(do_union = FALSE) %>% st_cast("LINESTRING")
# mapview(track_lines_out)

# Creation of an unique variable per ind per date
gps2$set1 <- paste(gps2$Logger_ID, date(gps2$time), sep = "")

mapview(gps2[gps2$run_loc == 'in',],
        zcol = 'set1',
        burst = T,
        homebutton = F)

#### Version 2.0 of the initial GPS database *** WITHOUT DUPLICATED DATA AND NA ***
#write.table(gps2.1, 'C:/Users/Etudiant/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_GPS_DB_V2.txt')

#### Number of fixes per day ####

# computation
list_fix_freq <- lapply(gps_list2, test)

# Visualization
bars <- lapply(list_fix_freq, barp_list)

# Summary of fixes frequencies for all the period

su <- do.call('rbind', list_fix_freq)
su_list <- tapply(su$n, su$Logger_ID, summary)
su_df <- do.call('rbind', su_list); su_df

# Summary of fixes frequencies for the rearing period only
rear_dates <- seq.Date(as.Date(strftime('2019-01-01', "%Y-%m-%d")),
                       as.Date(strftime('2019-04-30', "%Y-%m-%d")),
                       by = 1)

su_rear <- su[su$date %in% rear_dates,]
su_rear_list <- tapply(su_rear$n, su_rear$Logger_ID, summary)
su_rear_df <- do.call('rbind', su_rear_list); su_rear_df


#### Filtering based on the part of monitored breeding colony ####

mon_col <- st_read("C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/Lieu_dit_terrain/lieu_dit_terrain.shp")
protec_col <- st_read("C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/APB_PTEBAR/APB_PTEBAR.shp")
