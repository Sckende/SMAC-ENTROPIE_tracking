# ----------------------------------------------------------- #
# data --> ARGOS deployed on juvenile PTEBAR from 2017 to 2018
# source --> Patrick Pinet/Life +
# ---------------------------------------------------------- #

# ---- Clean ---- 
rm(list = ls())

# ---- Packages -----
library('mapview')
library('leaflet')
library('leafpop')
library('sf')
library('lubridate')
library('dplyr')

# ---- Data ----
argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_argos_data.txt",
                    sep = '\t',
                    h = T)
head(argos)
dim(argos)

table(argos$Vessel)
table(argos$Class)

argos$Date <- as.POSIXct(argos$Date,
                         format = '%d/%m/%Y %H:%M')
argos <- argos[order(argos$Vessel, argos$Date),]

#  ---- Deletion of location class in Z or U ----
table(argos$Class, useNA = 'always') # **** here we lost the PTT 166562 (2 locs with U class) ****

argos2 <- argos[!(argos$Class %in% c('U', 'Z')),]

# ---- Deletion of PTTs with almost no data ---- 
argos2 %>% group_by(Vessel) %>% count() # 2 PTTs are concerned 
# 162074 (2 locs)
# 166567 (5 locs)

argos3 <- argos2[!(argos2$Vessel %in% c('162074', '166567')), ]

# ---- Check for the first date of location for each devices ---- 
infos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_infos_deploiement.txt",
                    sep = '\t',
                    h = T)

setdiff(unique(argos3$Vessel), infos$device)

argos4 <- left_join(argos3,
                     infos[, c('device', 'deploy')],
                     by = c('Vessel' = 'device'))

argos4$deploy <- as.POSIXct(argos4$deploy,
                             format = '%d/%m/%Y %H:%M')
summary(argos4$deploy)

# ----
min.date <- argos4 %>%
  group_by(Vessel) %>% 
  summarise(min.date = min(Date)) # minimal date for each device

min.date.infos <- left_join(infos, min.date, by = c('device' = 'Vessel'))
min.date.infos$deploy <= min.date.infos$min.date

# ---- Quick visualization of data ----
argos.sp <- sf::st_as_sf(argos4,
                         coords = c('Longitude', 'Latitude'),
                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
argos.sp$Vessel <- as.factor(argos.sp$Vessel)

argos.track <- argos.sp %>% group_by(Vessel) %>%  summarize(do_union = F) %>% st_cast('LINESTRING')
mapview(argos.sp,
        zcol = 'Vessel',
        burst = T) + mapview(argos.track,
                             zcol = 'Vessel')

# ---- Delay btw records & visualization of loc groups ----

a.list <- split(argos4, argos4$Vessel)

a.list2 <- lapply(a.list, function(x){
  
  # del.hour <- as.numeric((x$Date - c(x$Date[-1], NA))/3660) # For computing delay btw loc recordings
  del.hour <- difftime(x$Date,
                       c(x$Date[-1], NA),
                       units = 'hours')
  
  x <- cbind(x, delay = c(0, del.hour[-length(del.hour)]))
  
  
  # ---- #
  
  val <- 1
  for(i in 1:length(x$Vessel)){
    
    if(x$delay[i] > -10){
      
      x$point.group[i] <- val
      
    } else {
      
      val <- val + 1
      x$point.group[i] <- val
      
    }
    
    x$odd[i] <- ifelse(x$point.group[i] %% 2, 'yes', 'no')
    
  }
  
    x
})

a.list2.sp <- lapply(a.list2, function(x){
  x <- sf::st_as_sf(x,
                    coords = c('Longitude', 'Latitude'),
                    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  x$Vessel <- as.factor(x$Vessel)
  x
})

mapview(a.list2.sp,
        zcol = 'odd',
        burst = T,
        col.regions = c('dodgerblue4', 'olivedrab'),
        popup = 'point.group',
        legend = F
) + mapview(argos.track,
            zcol = 'Vessel',
            burst = T,
            color = 'darkgrey',
            legend = F)

# ---- Save the cleaned data ---- #
# saveRDS(a.list2.sp,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.rds")

# write.table(do.call('rbind', a.list2),
#             "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.txt",
#             sep = '\t')
