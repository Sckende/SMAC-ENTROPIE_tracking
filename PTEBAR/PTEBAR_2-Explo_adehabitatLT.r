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

# creation of an object of class ltraj to store movements
ptebar <- as.ltraj(xy = gps[, c('Longitude', 'Latitude')],
                  date = gps$time,
                  id = gps$Logger_ID,
                  infolocs = gps[, 14:17])

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

p <- ptebar1[2]
p
p[[1]]
plot(p)
p
p.test <- cutltraj(p, 'is.na(p[[1]]$x)')
plot(p.test)
df_ptest <- ld(p.test)
plot(df_ptest$R2n)

tt <- 1:10
tt
cum(tt)
cumsum(tt)

cumsum(!is.na(p[[1]]$dist))

p.dy <- p[[1]]$dx[!is.na(p[[1]]$dx)]
cumsum(p.dy)       
plot(p.dy)
plot(cumsum(p.dy))



require(sf)
require(mapview)

protec_col <- st_read("C:/Users/Etudiant/Desktop/SMAC/SPATIAL_data_RUN/APB_PTEBAR/APB_PTEBAR.shp") # 1 = Petite Ile & 2 = GBN + PTN

mapview(protec_col, zcol = 'ARRETE_PRO', burst = T)

mapview(protec_col[protec_col$ARRETE_PRO == 2,])
