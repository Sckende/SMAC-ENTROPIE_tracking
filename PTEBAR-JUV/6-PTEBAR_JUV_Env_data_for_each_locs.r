#####################################
##### Test for the TERRA PACKAGE ####
#####################################
# Exploration
library(terra)
help(rast)

filename <- system.file("ex/logo.tif", package="terra")
filename

b <- rast(filename)
b
b[[1]]
plot(b[[1]])
plot(b[[2]])
plot(b[[3]])
plot(b)

raster.path <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R"
names.raster <- "SST_GLO_SST_L4_REP_OBSERVATIONS_010_011-TDS__SST-2017-6.nc"
raster.path
names.raster
r <-rast(paste(raster.path, names.raster, sep = '/'))
r
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

help(extract)
extract(r[[1]],
        argos.s[, c("Longitude", "Latitude")])

env.folder <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R" 
list.names <- list.files(env.folder)

#####################################################
#### Fusion of all env. files for each variables ####
#####################################################
library(stringr)
library(lubridate)

# SST
SST <- list.names[str_detect(list.names, 'SST')]
SST.list <- vector()
for(i in 1:length(SST)){
    r <- stack(paste(env.folder, SST[i], sep = '/'))
    SST.list <- c(SST.list, r)
}
SST.stack <- stack(SST.list)
names(SST.stack)

# All variables
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
names(stack.var.list[j]) <- var[j]
}


# Selection of the .nc file for each loc

list.names[str_detect(list.names, 'WIND-2018-4')]
list.names[str_detect(list.names, '2018-4')]

getwd()
paste(env.folder, list.names[str_detect(list.names, 'WIND-2018-4')], sep = '/')
r <- rast(paste(env.folder, list.names[str_detect(list.names, 'SST-2018-5')], sep = '/'))
r
r@time
names(r[[1]])
head(r[[1]])
r[[1]]
summary(r[[1]])

argos.s$wind_speed <- NA
argos.s$wind_north <- NA
argos.s$wind_east <- NA
argos.s$chlo <- NA
argos.s$SST <- NA

for(i in 1:length(argos.s$Latitude)){
    year <- year(argos.s$Date[i])
    month <- month(argos.s$Date[i])
    day <- day(arhos.s$Date[i])
    lon <- argos.s$Longitude[i]
    lat <- argos.s$Latitude[i]

# ---- #

    SST <- 
    chlo
    wind_speed
    wind_north
    wind_east


}

library(raster)
rr <- stack(paste(env.folder, list.names[str_detect(list.names, 'SST-2018-5')], sep = '/'))
rr
rrr <- stack(paste(env.folder, list.names[str_detect(list.names, 'SST-2018-6')], sep = '/'))
names(rr)
names(rrr)

j <- c(rr, rrr)
t <- stack(rr, rrr)
names(t)
stack(j)
