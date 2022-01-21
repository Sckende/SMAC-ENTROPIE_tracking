#####################################
##### Test for the TERRA PACKAGE ####
#####################################
rm(list = ls())
# Exploration
library(terra)
library(raster)
# help(rast)

# filename <- system.file("ex/logo.tif", package="terra")
# filename

# b <- rast(filename)
# b
# b[[1]]
# plot(b[[1]])
# plot(b[[2]])
# plot(b[[3]])
# plot(b)

raster.path <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R"
names.raster <- "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS__SST-2017-08.nc"
raster.path
names.raster
r <- stack(paste(raster.path, names.raster, sep = '/'))
r
names(r)
crs(r)
plot(r[[1]])
plot(r[[2]])
mean.r <- mean(r)
plot(mean.r)

#############################################################
#### Data extraction under several locations / TEST ZONE ####
#############################################################
argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.txt",
                    sep = '\t',
                    h = T)
head(argos)
summary(argos)
dim(argos)

argos.s <- argos[sample(nrow(argos), 1000), ] # data subset
argos.s

# help(extract)
extract(r[[1]],
        argos.s[, c("Longitude", "Latitude")])

env.folder <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R" 
list.names <- list.files(env.folder)

#####################################################
#### Fusion of all env. files for each variables ####
#####################################################
library(stringr)
library(lubridate)

# WIND
# wind_sp <- list.names[str_detect(list.names, 'SPEED')]
# wind_sp_list <- vector()
# for(i in 1:length(wind_sp)){
#     r <- stack(paste(env.folder, wind_sp[i], sep = '/'))
#     wind_sp_list <- c(wind_sp_list, r)
# }
# names(wind_sp_list[[1]])
# names(wind_sp_list[[2]])
# names(wind_sp_list[[8]])
# names(wind_sp_list[[9]])
# names(wind_sp_list[[11]])
# plot(wind_sp_list[[11]][[17:30]])
# wind_speed_stack <- stack(wind_sp_list)
# names(wind_speed_stack)

# Stack all variable rasters per group
var <- c( 'SST', 'CHLO', 'WIND-SPEED', 'WIND-NORTH', 'WIND-EAST')
stack.var.list <- list()

for(j in 1:length(var)){
    v <- list.names[str_detect(list.names, var[j])]
    v.list <- vector()

    for(i in 1:length(v)){
        r <- stack(paste(env.folder, v[i], sep = '/'))
        v.list <- c(v.list, r)
    }

stack.var.list[j] <- stack(v.list)
names(stack.var.list)[j] <- var[j]
}

warnings()

stack.var.list
names(stack.var.list)

SST_stack <- stack.var.list[[1]]
chlo_stack <- stack.var.list[[2]]
wind_speed_stack <- stack.var.list[[3]]
wind_north_stack <- stack.var.list[[4]]
wind_east_stack <- stack.var.list[[5]]


##############################################################################
#### Automated extraction of environmental informations under each relocs ####
##############################################################################
# argos.s <- argos[sample(nrow(argos), 100), ] # data subset
# argos.s

# argos.s$SST <- NA
# argos.s$CHLO <- NA

##################
# For SST & CHLO #
##################
# # ----- > First method : loop with extraction for each row

# for(i in 1:length(argos.s$Date)){
#     # lon <- argos.s$Longitude[i]
#     # lat <- argos.s$Latitude[i]
# # ----- #
#     date_loc_char <- as.character(date(argos.s$Date[i]))
#     date_loc_char2 <- str_replace_all(date_loc_char, '-', '.')
# # ----- #
#     SST_raster <- SST_stack[[str_which(names(SST_stack), date_loc_char2)]]
#     CHLO_raster <- chlo_stack[[str_which(names(chlo_stack), date_loc_char2)]]
# # ----- #
#     argos.s$SST[i] <- extract(SST_raster,
#                               argos.s[i, c('Longitude', 'Latitude')])
#     argos.s$CHLO[i] <- extract(CHLO_raster,
#                               argos.s[i, c('Longitude', 'Latitude')])
# }
# print("ayéééé")
# argos.s


# ----- > Second method : Split the dataframe by date and apply the exctraction by group of coordinates
length(unique(date(argos$Date)))
argos$dt <- str_replace_all(as.character(date(argos$Date)), '-', '.')
argos_list <- split(argos, argos$dt)
length(argos_list)

argos_list2 <- lapply(argos_list, function(x){

    SST_raster <- SST_stack[[str_which(names(SST_stack), unique(x$dt))]]
    CHLO_raster <- chlo_stack[[str_which(names(chlo_stack), unique(x$dt))]] 

    x$SST <- extract(SST_raster,
                     x[, c('Longitude', 'Latitude')])
    x$CHLO <- extract(CHLO_raster,
                      x[, c('Longitude', 'Latitude')])
    
    x
})
print("ayéééé")

argos2 <- do.call('rbind', argos_list2)
mean(argos2$SST, na.rm = T)
summary(argos2$SST)

mean(argos2$CHLO, na.rm = T)
summary(argos2$CHLO)

#################################
# For WIND speed & orientation #
################################
test <- argos[sample(nrow(argos), 1000), ]
test <- test[, c("Date", "Vessel", "Longitude", "Latitude")]
head(test)
test$minutes <- hour(test$Date)*60 + minute(test$Date)

# raster aux 6h 
# deb      fin       raster.hour     raster.day
# 21:01 (1261) -> 03:00 (180)  ==> 00:00           J+1 or J depending on bef/aft midnight
# 03:01 (181)  -> 09:00 (540)  ==> 06:00            J
# 09:01 (541)  -> 15:00 (900)  ==> 12:00            J
# 15:01 (901)  -> 21:00 (1260) ==> 18:00            J

test$raster_hour[test$minutes >= 1261 | test$minutes <= 180] <- "00.00"
test$raster_hour[test$minutes >= 181 & test$minutes <= 540] <- "06.00"
test$raster_hour[test$minutes >= 541 & test$minutes <= 900] <- "12.00"
test$raster_hour[test$minutes >= 901 & test$minutes <= 1260] <- "18.00"
# unique(hour(test$Date[test$raster_hour == "18.00"]))
# unique(hour(test$Date[test$raster_hour == "00.00"]))
# unique(hour(test$Date[test$raster_hour == "06.00"]))
# unique(hour(test$Date[test$raster_hour == "12.00"]))
# test[test$minutes == 1260,]

test$raster_date <- ifelse(test$minutes >= 1261, as.character(date(test$Date)+1), as.character(date(test$Date)))
# test[test$minutes >= 1261,]
# test[test$minutes < 1261,]

test$raster_layer <- paste(str_replace_all(test$raster_date, '-', '.'),
                          test$raster_hour,
                          sep = '.')
head(test)

test_list <- split(test, test$raster_layer)
length(test_list)

test_list2 <- lapply(test_list, function(x){
# ----- #
    speed_raster <- wind_speed_stack[[str_which(names(wind_speed_stack), unique(x$raster_layer))]]
    north_raster <- wind_north_stack[[str_which(names(wind_north_stack), unique(x$raster_layer))]]
    east_raster <- wind_east_stack[[str_which(names(wind_east_stack), unique(x$raster_layer))]]
# ----- #
    x$wind_speed <- extract(speed_raster,
                     x[, c('Longitude', 'Latitude')])
    x$wind_north <- extract(north_raster,
                      x[, c('Longitude', 'Latitude')])
    x$wind_east <- extract(east_raster,
                      x[, c('Longitude', 'Latitude')])
# ----- #
    print(unique(x$raster_layer))
    print(str_which(unique(test$raster_layer), unique(x$raster_layer)))
# ----- #
    x
})
print("ayééé")

ln1 <- str_sub(names(wind_speed_stack), 2, 17) 
ln2 <- str_sub(names(wind_north_stack), 2, 17)
ln3 <- str_sub(names(wind_east_stack), 2, 17)

all(ln1 == ln2)
all(ln1 == ln3)
all(ln2 == ln3)

lntest <- unique(test$raster_layer)
all(lntest %in% ln1)
all(ln1 %in% lntest)

# Recherche des couches manquantes dans les wind_xxxx_stack
extra <- lntest[!(lntest %in% ln1)]

# "2018.05.31.18.00"
# "2017.04.30.18.00"
# "2018.06.30.18.00"
# "2017.05.31.12.00"
# "2018.05.31.06.00"
# "2018.09.30.12.00"

for(i in str_sub(extra, 1, 10)){ # "2018.05.31" "2017.04.30" "2018.06.30" "2017.05.31" "2018.05.31" "2018.09.30"
    names(wind_speed_stack)[str_which(names(wind_speed_stack), "2018.09.30")]
}

