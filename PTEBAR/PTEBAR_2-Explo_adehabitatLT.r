rm(list = ls())
source('C:/Users/Etudiant/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR/PTEBAR_0-Functions_for_scripts.R')
require(adehabitatLT)
require(lubridate)

gps <- read.table("C:/Users/Etudiant/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_GPS_DB_V2_adehabLT.txt", h = T, sep = '\t', dec = ".") # Non duplicated data with NAs in lat/Long
summary(gps)
names(gps)

# Conversion of dates
# gps$time <- as.POSIXct(strptime(as.character(gps$time),
#                                 '%Y-%m-%d %H:%M:%S'),
#                        tz = 'Indian/Mauritius')
# head(gps$time)
# class(gps$time)

gps$time <- strptime(paste(gps$time), "%Y-%m-%d %H:%M:%S")
gps$time <- as.character(gps$time)

gps$time <- as.POSIXct(gps$time, tz="Indian/Mauritius") 


# Check for duplicated date
splitdata <- split(gps, gps$Logger_ID)

splitdupz <- lapply(splitdata, function(birdup){
  which(duplicated(birdup$time))})
str(splitdupz)

# Dates explorations
head(gps)
plot(gps$time)
table(gps$date)

time_details <- list()
j <- 0
for(i in unique(gps$Logger_ID)){
  j <- j + 1
  data <- gps[gps$Logger_ID == i,]
  
  t <- as.data.frame(table(data$date))
  names(t) <- c('date', 'fix')
  t_NA <- as.data.frame(table(data$date[is.na(data$Latitude)]))
  names(t_NA) <- c('date', 'NA_count') 
  ttot <- merge(t, t_NA, all.x = T) 
  ttot$NA_count[is.na(ttot$NA_count)] <- 0
    
  hh <- table(data$Hour)
  hh_NA <- table(data$Hour[is.na(data$Latitude)])
  
  time_details[[j]] <- list(date = ttot, hours = hh)
  names(time_details)[j] <- i
}


for(i in 1:length(time_details)){
  par(mfrow = c(2, 1))
  
  data <- time_details[[i]]
  
  barplot(data$date$fix,
          names.arg = data$date$date,
          ylim = c(0, max(data$date$fix)),
          las = 2,
          main = names(time_details)[i])
  barplot(data$date$NA_count,
          add = T,
          col = 'red',
          las = 2,
          ylim = c(0, max(data$date$fix)))
  
  barplot(data$hours)
}


x11(); par(mfrow = c(3, 4))
for(i in 1:length(time_details)){
  data <- time_details[[i]]
  
  barplot(data$hours,
          main = names(time_details)[i])
  # barplot(data$date$NA_count,
  #         add = T,
  #         col = 'red',
  #         las = 2,
  #         ylim = c(0, max(data$date$fix)))
}


# creation of an object of class ltraj to store movements
ptebar <- as.ltraj(xy = gps[, c('Longitude', 'Latitude')],
                  date = gps$time,
                  id = gps$Logger_ID,
                  infolocs = gps[, 14:19])

plot(ptebar)
plot(ptebar[1])
plot(ptebar[2])
plot(ptebar[3])
plot(ptebar[4])
plot(ptebar[5])
plot(ptebar[6])
plot(ptebar[7])
plot(ptebar[8])
plot(ptebar[9])
plot(ptebar[10])
# x11(); 
plotltr(ptebar, 'dt')
plotltr(ptebar, 'dist')

# Deletion of PAC04, PAC05 & PAC13 - 2, 3 & 8
ptebar1 <- ptebar[c(1, 4:7, 9, 10)]
plot(ptebar1)
plot(ptebar1[4])

# Subset of bursts
# Test with one individual - PAC06

# p <- ptebar1[2]
p <- ptebar1[1]
p
p[[1]]
plot(p)
p
info <- as.data.frame(infolocs(p))
# df_p <- ld(p)

table(info$colony == 'in')
colo_test <- function(colo_var){
  return(colo_var == 'in')
}

p.test <- cutltraj(p, 'colo_test(as.data.frame(infolocs(p))$colony)')
plot(p.test[1])
plot(p.test[2])


#### TEST ZONE ####
# Find the multiple max of lat and lon
require(ggpmisc)
require(ggplot2)
plot(df_ptest$y, type = 'l')
peak.y <- df_ptest$y[ggpmisc:::find_peaks(df_ptest$y)]
points(1:length(peak.y), peak.y) ### a continuer !

plot(df_ptest$x, type = 'l')

df_ptest$y[ggpmisc:::find_peaks(df_ptest$y)]
df_ptest$x[ggpmisc:::find_peaks(df_ptest$y)]

ggplot(data = df_ptest[!is.na(df_ptest),], aes(x = x, y = y)) + geom_line() + stat_peaks(col = "red")

# Others tricks .... ?
require(sf)
require(sp)
require(mapview)

protec_col <- st_read("C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/APB_PTEBAR/APB_PTEBAR.shp") # 1 = Petite Ile & 2 = GBN + PTN
protect_col_split <- st_cast(protec_col, 'POLYGON')
protect_GBN <- protect_col_split[3,]
st_crs(protect_GBN) # UTM 

jo <- as(protect_GBN, 'Spatial') # coercion from sf to sp in order to ...
protect_GBN_dec <- spTransform(jo, # mofify the crs ...
                               CRS('+init=epsg:4326')) # epsg corresponding to the mondial decimal reference
projcrs <- st_crs(protect_GBN_dec)
protect_GBN_dec <- st_as_sf(protect_GBN_dec) # sf object


ttt <- ld(p.test[2])
ttt_spa <- st_as_sf(ttt[!is.na(ttt$x),],
                    coords = c('x', 'y'),
                    crs = projcrs)
mapview(ttt_spa) + mapview(protect_GBN_dec)
plot(p.test[3])
plot(p.test[4])
plot(p.test[5])

df_ptest <- ld(p.test)
table(df_ptest$colony[df_ptest$burst == 'PAC06.02'])
df_ptest[df_ptest$colony == 'in',]


