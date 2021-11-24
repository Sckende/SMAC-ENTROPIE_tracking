# ----------------------------------------------------------- #
# data --> ARGOS deployed on juvenile PTEBAR from 2017 to 2018
# source --> Patrick Pinet/Life +
# ---------------------------------------------------------- #

# ---- Cleaning ---- 
rm(list = ls())

# ---- Packages -----
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

# ---- Data ----
argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_argos_data.txt",
                    sep = '\t',
                    h = T)

infos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_infos_deploiement.txt",
                    sep = '\t',
                    h = T)
head(argos)
dim(argos)

table(argos$Vessel)
table(argos$Class)

# Conversion of Class variable in numeric one

argos$Class.num <- argos$Class
argos$Class.num[argos$Class.num == 'B'] <- -2
argos$Class.num[argos$Class.num == 'A'] <- -1

table(argos$Class.num)
table(argos$Class)

# EXPLORATION of first points before deployment ####
# Necessary to keep at least one point one the breeding colony

# Date format
argos$Date <- as.POSIXct(argos$Date,
                         format = '%d/%m/%Y %H:%M')
argos <- argos[order(argos$Vessel, argos$Date),]

argos %>% group_by(Vessel) %>% summarize(min(Date))

infos$deploy <- as.POSIXct(infos$deploy,
                         format = '%d/%m/%Y %H:%M')

# List creation for keeping saved relocations before deployment for each device 
bef.deploy <- lapply(split(argos, argos$Vessel), function(x){
  
  y <- x[x$Date <= infos$deploy[infos$PTT == unique(x$Vessel)],]
  
  if(length(y$Vessel) >= 1){
    y  
  }
})

bef.deploy[5] <- NULL # deletion of devices with no date before the deployment date

# First visual exploration of these relocations

bef.sp <- lapply(bef.deploy, function(x){
  
  x <- sf::st_as_sf(x,
                    coords = c('Longitude', 'Latitude'),
                    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  x
  })

bef.sp <- do.call('rbind', bef.sp)
bef.sp$Vessel <- as.factor(bef.sp$Vessel)
mapview(bef.sp, zcol = 'Vessel', burst = T) # All device with at least one point one the colony

# Keep the more accurate relocs before the deployment date per device
pt.bef.deploy1 <- bef.deploy[1:9]
pt.bef.deploy2 <- bef.deploy[8:16]

pt.bef.deploy2 <- lapply(pt.bef.deploy2, function(x){

    x$Class.num <- as.numeric(x$Class.num)
    y <- x[which(x$Class.num == max(x$Class.num)),]
    z <-y[which(y$Date == max(y$Date)),]
    z
 
})

pt.bef.deploy <- rbind(do.call('rbind', pt.bef.deploy1),
                       do.call('rbind', pt.bef.deploy2))

pt.bef.deploy.sp <- sf::st_as_sf(pt.bef.deploy,
                                 coords = c('Longitude', 'Latitude'),
                                 crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
mapview(bef.sp, zcol = 'Vessel', burst = T) + mapview(pt.bef.deploy.sp, col.regions = 'black')

#  ---- DELETION of location class in Z or U ----
table(argos$Class, useNA = 'always') # **** here we lost the PTT 166562 (2 locs with U class) ****

argos2 <- argos[!(argos$Class %in% c('U', 'Z')),]

# ---- DELETION of PTTs with almost no data ---- 
argos2 %>% group_by(Vessel) %>% count() # 2 PTTs are concerned 
# 162074 (2 locs)
# 166567 (5 locs)

argos3 <- argos2[!(argos2$Vessel %in% c('162074', '166567')), ]

# ---- CHECK for the first date of location for each devices ---- 


setdiff(unique(argos3$Vessel), infos$PTT)

argos4 <- left_join(argos3,
                     infos[, c('PTT', 'deploy')],
                     by = c('Vessel' = 'PTT'))

argos4$deploy <- as.POSIXct(argos4$deploy,
                             format = '%d/%m/%Y %H:%M')
summary(argos4$deploy)

# ---- #
min.date <- argos4 %>%
  group_by(Vessel) %>% 
  summarise(min.date = min(Date)) # minimal date for each device

min.date.infos <- left_join(infos, min.date, by = c('PTT' = 'Vessel'))
min.date.infos$deploy <= min.date.infos$min.date

# ---- QUICK visualization of data ----
argos.sp <- sf::st_as_sf(argos4,
                         coords = c('Longitude', 'Latitude'),
                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
argos.sp$Vessel <- as.factor(argos.sp$Vessel)

argos.track <- argos.sp %>% group_by(Vessel) %>%  summarize(do_union = F) %>% st_cast('LINESTRING')
mapview(argos.sp,
        zcol = 'Vessel',
        burst = T) + mapview(argos.track,
                             zcol = 'Vessel')
# ---- RETRIEVE the first point on breeding colony if it's necessary ####
# 162070, 162071, 162072, 162073, 166561, 166563, 166564, 166565
rec <- pt.bef.deploy[pt.bef.deploy$Vessel %in% c(162070, 162071, 162072, 162073, 166561, 166563, 166564, 166565),]
rec <- left_join(rec,
                 infos[,c(1, 3)],
                 by = c('Vessel' = 'PTT'))
argos5 <- rbind(argos4,
                rec)
argos5$Vessel <- as.factor(argos5$Vessel)
argos5.sp <- sf::st_as_sf(argos5,
                         coords = c('Longitude', 'Latitude'),
                         crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

mapview(argos5.sp, zcol = 'Vessel', burst = T)

# RE-ORDER device per date #### 
argos5 <- argos5[order(argos5$Vessel, argos5$Date),]


# ---- DELAY btw records & visualization of loc groups ----

a.list <- split(argos5, argos5$Vessel)

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

# DEAL with the duplicated data ####
# Order data based on date, then descending Class.num for keeping the best data quality when date is duplicated

dup2 <- lapply(a.list2, function(x){
  x$Class.num[x$Class.num == 'U'] <- NA
  x$Class.num <- as.numeric(x$Class.num)
  x <- x[with(x, order(x$Date, -x$Class.num)),]
  x <- x[duplicated(x$Date),]
  x
}) # Which rows are duplicated

dim(do.call('rbind', dup2))

# Deletion of the duplicated rows

argos6.ls <- lapply(a.list2, function(x){
  x$Class.num[x$Class.num == 'U'] <- NA
  x$Class.num <- as.numeric(x$Class.num)
  x <- x[with(x, order(x$Date, -x$Class.num)),]
  x <- x[!duplicated(x$Date),]
  x
})

# SPATIAL object & extraction of projected and non projected coords ####
argos7 <- do.call('rbind', argos6.ls)

argos7.sp <- sf::st_as_sf(argos7,
                    coords = c('Longitude', 'Latitude'),
                    crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

coordLatLon <- st_coordinates(argos7.sp)
coordUTM <- st_coordinates(st_transform(argos7.sp,
                                        crs = 32743))

UTMDF <- as.data.frame(coordUTM)

argos8 <- cbind(as.data.frame(argos7), coordUTM)
head(argos8)

argos9 <- st_as_sf(argos8,
                   coords = c('X', 'Y'),
                   crs = 32743)
argos9 <- cbind(argos9, coordUTM)
# ---- Save the cleaned data ---- #
# saveRDS(argos9,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.rds")

# write.table(argos8,
#             "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.txt",
#             sep = '\t')
