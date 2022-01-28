#####################################
##### Test for the TERRA PACKAGE ####
#####################################
rm(list = ls())
# Exploration
library(terra)
library(raster)
library(sf)
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
# argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.txt",
#                     sep = '\t',
#                     h = T)
argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED_speed.rds")
argos <- do.call('rbind', argos)
argos <- st_drop_geometry(argos)
names(argos)
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
length(list.names)

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
# saveRDS(SST_stack,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/SST_stack.rds")
chlo_stack <- stack.var.list[[2]]
# saveRDS(chlo_stack,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/chlo_stack.rds")
wind_speed_stack <- stack.var.list[[3]]
# saveRDS(wind_speed_stack,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_speed_stack.rds")
wind_north_stack <- stack.var.list[[4]]
# saveRDS(wind_north_stack,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_north_stack.rds")
wind_east_stack <- stack.var.list[[5]]
# saveRDS(wind_east_stack,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_east_stack.rds")


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
                     as.data.frame(x[, c('Longitude', 'Latitude')]))
    x$CHLO <- extract(CHLO_raster,
                      as.data.frame(x[, c('Longitude', 'Latitude')]))
    
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

# Deletion of duplicated layers in raster
# ---> wind_east_stack
str_length(names(wind_east_stack))
j <- names(wind_east_stack)[str_length(names(wind_east_stack)) > 20]
east_wind_deletion <- j[str_detect(j, '.00.2')]
dim(wind_east_stack)
wind_east_stack <- dropLayer(wind_east_stack,
                             match(east_wind_deletion,
                                   names(wind_east_stack)))
dim(wind_east_stack)   
names(wind_east_stack)

# ---> wind_north_stack
str_length(names(wind_north_stack))
k <- names(wind_north_stack)[str_length(names(wind_north_stack)) > 20]
north_wind_deletion <- k[str_detect(k, '.00.2')]
dim(wind_north_stack)
wind_north_stack <- dropLayer(wind_north_stack,
                              match(north_wind_deletion, 
                                    names(wind_north_stack)))
dim(wind_north_stack) 

# ---> wind_speed_stack
str_length(names(wind_speed_stack))
l <- names(wind_speed_stack)[str_length(names(wind_speed_stack)) > 20]
speed_wind_deletion <- l[str_detect(l, '.00.2')]
dim(wind_speed_stack)
wind_speed_stack <- dropLayer(wind_speed_stack,
                              match(speed_wind_deletion,
                                    names(wind_speed_stack)))
dim(wind_speed_stack)

# Test zone
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

test$raster_date <- ifelse(test$minutes >= 1261,
                           as.character(date(test$Date)+1),
                           as.character(date(test$Date)))
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
    speed_raster <- wind_speed_stack[[str_which(names(wind_speed_stack),
                                                unique(x$raster_layer))]]
    north_raster <- wind_north_stack[[str_which(names(wind_north_stack),
                                                unique(x$raster_layer))]]
    east_raster <- wind_east_stack[[str_which(names(wind_east_stack),
                                              unique(x$raster_layer))]]
# ----- #
    x$wind_speed <- extract(speed_raster,
                            as.data.frame(x[, c("Longitude", "Latitude")]))
    x$wind_north <- extract(north_raster,
                            as.data.frame(x[, c("Longitude", "Latitude")]))
    x$wind_east <- extract(east_raster,
                           as.data.frame(x[, c("Longitude", "Latitude")]))
# ----- #
    print(unique(x$raster_layer))
    print(str_which(unique(test$raster_layer), unique(x$raster_layer)))
# ----- #
    x
})
print("ayééé")
#####################
# On ALL Argos data #
#####################

head(argos2, 50)
argos2$minutes <- hour(argos2$Date)*60 + minute(argos2$Date)

# raster aux 6h 
# deb      fin       raster.hour     raster.day
# 21:01 (1261) -> 03:00 (180)  ==> 00:00           J+1 or J depending on bef/aft midnight
# 03:01 (181)  -> 09:00 (540)  ==> 06:00            J
# 09:01 (541)  -> 15:00 (900)  ==> 12:00            J
# 15:01 (901)  -> 21:00 (1260) ==> 18:00            J

argos2$raster_hour[argos2$minutes >= 1261 | argos2$minutes <= 180] <- "00.00"
argos2$raster_hour[argos2$minutes >= 181 & argos2$minutes <= 540] <- "06.00"
argos2$raster_hour[argos2$minutes >= 541 & argos2$minutes <= 900] <- "12.00"
argos2$raster_hour[argos2$minutes >= 901 & argos2$minutes <= 1260] <- "18.00"

head(argos2)
argos2$raster_date <- ifelse(argos2$minutes >= 1261,
                             as.character(date(argos2$Date)+1),
                             as.character(date(argos2$Date)))

argos2$raster_layer <- paste(str_replace_all(argos2$raster_date, '-', '.'),
                             argos2$raster_hour,
                             sep = '.')

argos2_list <- split(argos2, argos2$raster_layer)
length(argos2_list)

argos3_list <- lapply(argos2_list, function(x) {
# ----- #
    speed_raster <- wind_speed_stack[[str_which(names(wind_speed_stack),
                                                unique(x$raster_layer))]]
    north_raster <- wind_north_stack[[str_which(names(wind_north_stack),
                                                unique(x$raster_layer))]]
    east_raster <- wind_east_stack[[str_which(names(wind_east_stack),
                                              unique(x$raster_layer))]]
# ----- #
    x$wind_speed <- extract(speed_raster,
                            as.data.frame(x[, c("Longitude", "Latitude")]))
    x$wind_north <- extract(north_raster,
                            as.data.frame(x[, c("Longitude", "Latitude")]))
    x$wind_east <- extract(east_raster,
                           as.data.frame(x[, c("Longitude", "Latitude")]))
# ----- #
    print(unique(x$raster_layer))
    print(str_which(unique(test$raster_layer), unique(x$raster_layer)))
# ----- #
    x
})
print("ayééé")

argos3 <- do.call('rbind', argos3_list)
mean(argos3$wind_speed, na.rm = T)
summary(argos3$wind_speed)

mean(argos3$wind_north, na.rm = T)
summary(argos3$wind_north)

mean(argos3$wind_east, na.rm = T)
summary(argos3$wind_east)

argos3 <- argos3[order(argos3$Vessel, argos3$Date),]
head(argos3)

#########################################
# Write the output data with extraction #
#########################################
# saveRDS(argos3,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA.rds")

