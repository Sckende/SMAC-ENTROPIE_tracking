#### Error correction of initial database & addition of useful variables ####
require(lubridate)

gps <- read.csv2("C:/Users/Etudiant/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_GPS_all.csv", dec = ".")

#### Creation of 'time' variable ####
gps$time <- paste(paste(gps$Year, gps$Month, gps$Day, sep = "-"), paste(gps$Hour, gps$Minute, gps$Second, sep = ":"), sep = " ")
gps$time <- as.POSIXct(gps$time)

#### here keeping only date, ID, lat & long, speed, searching_time, Voltage ####
gps1 <- gps[, c(2:13, 29)]

#### Addition of a 'period' variable

gps1$period[month(gps1$time) %in% 1:4] <- 'rearing'
gps1$period[month(gps1$time) %in% 5:8] <- 'winter'
gps1$period[month(gps1$time) %in% 9:10] <- 'prosp'
gps1$period[month(gps1$time) %in% 11:12] <- 'incub'

#### Line correction with winter period - Modification of DATE and PERIOD
gps1[gps1$period == "winter",]

gps1$Month[gps1$period == "winter"] <- 12
gps1$time[gps1$period == "winter"] <- as.POSIXct('2018-12-18 11:33:00')
gps1$period[gps1$period == "winter"] <- 'incub'

rm(gps)
