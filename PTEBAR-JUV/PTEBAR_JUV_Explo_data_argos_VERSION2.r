# There, we will use the ARGOS data retrieved from the LIFE + PROJECT #
# Duty cycle : 10/24 #
# The extrapolated data (1loc/hour) were obtained by P. Pinet #
# *** The 'Date' variable corresponds to the moment where a location was recorded while 'DATE_1' corresponds to the extrapolated data #

rm(list = ls())

library('mapview')
library('leaflet')
library('leafpop')
library('sf')
library('lubridate')
library('dplyr')

#### Loading and treatment of extrapolated ARGOS data ####
argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_extrapolated_argos_Life.txt",
                    h = T,
                    sep = '\t',
                    dec = '.')

head(argos)

# ---- Date class ----
argos$Date <- as.POSIXct(argos$Date,
                         format = "%d/%m/%Y %H:%M") # Date format
argos$DATE_1 <- as.POSIXct(argos$DATE_1,
                           format = "%d/%m/%Y %H:%M") # Date format
summary(argos)

# ---- Check for the coordinates of positions ----
all(is.na(argos$Latitude)) # No NA for Latitude
all(is.na(argos$Longitude)) # No NA for Longitude

# ---- Only keep the location Class B, A, 0, 1, 2 & 3 (from the worst to the best) ----
argos <- argos[argos$Class %in% c('B', 'A', '0', '1', '2', '3'),] # From 46,299 locs to 37,506 locs

# ---- Check for min trajectory date vs deployment date ----
infos_argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Infos_deploiement.txt",
                          h = T,
                          sep = "\t")
infos_argos$deploy <- as.POSIXct(infos_argos$deploy,
                                 format = "%d/%m/%Y %H:%M") # Date format - This is the most important date here since the devices started before the deployment
infos_argos$start <- as.POSIXct(infos_argos$start,
                                format = "%d/%m/%Y %H:%M") # Date format


argos <- left_join(argos, infos_argos[, c('device', 'deploy')], by = c('Vessel' = 'device'))

arg.list <- split(argos, argos$Vessel) 

# ---- Deletion of locations recorded before the deployment ----

new.arg.list <- lapply(arg.list, function(x){
  x <- x[x$DATE_1 >= unique(x$deploy),]
})

argos2 <- do.call('rbind', new.arg.list)
# ---- Write a new dataframe with cleaned extrapolated data ----
# write.table(argos2,
#             "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_extrapolated_argos_Life_CLEANED.txt",
#             sep = '\t')

# ---- Retrieve the 'RAW' location ----
# --- Test zone
# ex <- new.arg.list[[15]]
# ex
# 
# table(ex$Date)
# table(ex$Date, ex$Class)
# # --
# ex.test <- ex[ex$Date == '2017-04-10 06:05:00',]
# ex.test.sp <- sf::st_as_sf(ex.test,
#                            coords = c('Longitude', 'Latitude'),
#                            crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# mapview(ex.test.sp)
# # --
# ex.test2 <- ex[ex$Date == '2017-04-11 12:31:00',]
# ex.test.sp2 <- sf::st_as_sf(ex.test2,
#                            coords = c('Longitude', 'Latitude'),
#                            crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# mapview(ex.test.sp2)
# # --
# ex.dup <- ex[!duplicated(ex$Date),]
# ex.dup.sp <- sf::st_as_sf(ex.dup,
#                           coords = c('Longitude', 'Latitude'),
#                           crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# mapview(ex.dup.sp)
# # --
# exx <- new.arg.list[[12]]
# table(exx$Date, exx$Class)
# # --
# exx.test <- exx[exx$Date == '2017-04-15 11:46:00',]
# exx.test.sp <- sf::st_as_sf(exx.test,
#                            coords = c('Longitude', 'Latitude'),
#                            crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# mapview(exx.test.sp)
# # -- 
# exx.dup <- exx[!duplicated(exx$Date),]
# exx.dup.sp <- sf::st_as_sf(exx.dup,
#                           coords = c('Longitude', 'Latitude'),
#                           crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# mapview(exx.dup.sp)

# --- Application zone
# The serie of the same date seems to a linear replication of the point -- WEIRDOOO
# Finally, I decided to only keep the first value of the variable 'Date' when it's replicated

argos2.list <- split(argos2, argos2$Vessel)

argos.raw.list <- lapply(argos2.list, function(x){
  x <- x[!duplicated(x$Date),]
})

RAW.argos <- do.call('rbind', argos.raw.list)

# ---- Write a new dataframe with retrieved RAW location data ----
# write.table(RAW.argos,
#             "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_retrieve_RAW_data.txt",
#             sep = '\t')

# ---- Summary of raw data ----
RAW.argos %>%
  group_by(Vessel) %>%
  count()

# ---- Visualization
RAW.argos$Vessel <- as.factor(RAW.argos$Vessel)
RAW.sp <- sf::st_as_sf(RAW.argos,
                       coords = c('Longitude', 'Latitude'),
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # Spatial Points

RAW.track <- RAW.sp %>%
  group_by(Vessel) %>% 
  summarize(do_union = FALSE) %>%
  st_cast("LINESTRING") # Creation of SF LINESTRINGS

mapview(RAW.sp,
        zcol = 'Vessel',
        burst = T,
        legend = F) + mapview(RAW.track,
          zcol = 'Vessel',
          burst = F,
          legend = F)
