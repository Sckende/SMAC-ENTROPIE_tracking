# Here the script is based on the data retrieved from the dryad associated with the 'wettability' paper of Henri Weimerskirch #
rm(list = ls())

#### Loading and treatment of raw data ####
wargos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_weimerskirch_argos_data.txt",
           sep = '\t',
           h = T)

summary(wargos)

head(wargos$Date)
wargos$Date <- as.POSIXct(wargos$Date,
                          format = '%d/%m/%Y %H:%M')
summary(wargos$Date)
head(wargos$Date)

# Deletion of location class in Z or U
table(wargos$Class, useNA = 'always') # **** here we lost the PTT 166562 (2 locs with U class) ****

wargos <- wargos[!(wargos$Class %in% c('U', 'Z')),]

# Deletion of PTTs ith almost no data
wargos %>% group_by(PTT) %>% count() # 2 PTTs are concerned 
# 162074 (2 locs)
# 166567 (5 locs)

wargos <- wargos[!(wargos$PTT %in% c('162074', '166567')), ]

# Check for the first date of location for each devices
infos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_infos_deploiement.txt",
                    sep = '\t',
                    h = T)
setdiff(unique(wargos$PTT), infos$device)
head(infos)

wargos2 <- left_join(wargos,
                     infos[, c('device', 'deploy')],
                     by = c('PTT' = 'device'))
wargos2$deploy <- as.POSIXct(wargos2$deploy,
                             format = '%d/%m/%Y %H:%M')
summary(wargos2$deploy)
# ----
min.date <- wargos2 %>%
  group_by(PTT) %>% 
  summarise(min.date = min(Date))

min.date.infos <- left_join(infos, min.date, by = c('device' = 'PTT'))
min.date.infos$deploy <= min.date.infos$min.date

# Check for the delay between records for each devices

wargos.list <- split(wargos2, wargos2$PTT)

t <- wargos.list[[1]]# **** ORDONNER LES DATES !!!! ****
t2 <- t[order(t$Date),]
del.hour <- as.numeric((t2$Date - c(t2$Date[-1], NA))/3660)
summary(del.hour)

t3 <- cbind(t2, c(NA, del.hour[-length(del.hour)]))
names(t3)[11] <- 'delay'

t3$test <- NA
val <- 1
for(i in 1:length(t3$test)){

  if(t3$delay[i] > -10 | is.na(t3$delay[i])){
    t3$test[i] <- val
  } else {
    val <- val + 1
    t3$test[i] <- val
  }
}
t3$test <- as.factor(t3$test)
# exploration in order to determine a threshold for grouping points based on the duty cycle
t3.sp <- sf::st_as_sf(t3,
                      coords = c('Longitude', 'Latitude'),
                      crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

t3.sp.track <- t3.sp %>% summarize(do_union = F) %>% st_cast('LINESTRING')
mapview(t3.sp,
        zcol = 'test',
        col.regions = rep(c('olivedrab', 'dodgerblue4'), length(unique(t3.sp$test))/2),
        legend = F) + mapview(t3.sp.track,
                              color = 'darkgrey')

# Treatment to do on the whole list ! 
