rm(list = ls())
library(sf)
library(rasterVis)
library(viridis)
library(stringr)
library(terra)
library(raster)
library(viridis)
library(openair)
library(circular)
library(lubridate)
library(dplyr)

###################
# Loading data ####
###################

dirs_2017 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_dirs_2017.rds")
dirs_2017 # From -180 to 180

abs_wind_sp_2017 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/abs_wind_speed_2017.rds")

############################################
# Split layers for bimonthly wind roses ####
############################################

# 4 layers / 24h
# 1 --> 15 = 60 layers
# 16 --> 30 = 60 layers
# 16 --> 31 = 64 layers
names(dirs_2017)[1:60] # --> from 1 (00:00) to 15 (18:00) April 2017
names(dirs_2017)[61:120] # --> from 16 (00:00) to 30 (18:00) April 2017
names(dirs_2017)[121:180] # --> from 1 (00:00) to 15 (18:00) May 2017
names(dirs_2017)[181:244] # --> from 16 (00:00) to 31 (18:00) May 2017

apr_1 <- dirs_2017[[1:60]]
abs_wind_apr_1 <- abs_wind_sp_2017[[1:60]]

apr_2 <- dirs_2017[[61:120]]
abs_wind_apr_2 <- abs_wind_sp_2017[[61:120]]

may_1 <- dirs_2017[[121:180]]
abs_wind_may_1 <- abs_wind_sp_2017[[121:180]]

may_2 <- dirs_2017[[181:244]]
abs_wind_may_2 <- abs_wind_sp_2017[[181:244]]

jun_1 <- dirs_2017[[245:304]]
abs_wind_jun_1 <- abs_wind_sp_2017[[245:304]]

jun_2 <- dirs_2017[[305:364]]
abs_wind_jun_2 <- abs_wind_sp_2017[[245:364]]

jul_1 <- dirs_2017[[365:424]]
abs_wind_jul_1 <- abs_wind_sp_2017[[365:424]]

jul_2 <- dirs_2017[[425:488]]
abs_wind_jul_2 <- abs_wind_sp_2017[[425:488]]

aug_1 <- dirs_2017[[489:548]]
abs_wind_aug_1 <- abs_wind_sp_2017[[489:548]]

aug_2 <- dirs_2017[[549:612]]
abs_wind_aug_2 <- abs_wind_sp_2017[[549:612]]

list_dirs <- list(apr_1 = apr_1,
                  apr_2 = apr_2,
                  may_1 = may_1,
                  may_2 = may_2,
                  jun_1 = jun_1,
                  jun_2 = jun_2,
                  jul_1 = jul_1,
                  jul_2 = jul_2,
                  aug_1 = aug_1,
                  aug_2 = aug_2)
list_abs_wind <- list(abs_wind_apr_1 = abs_wind_apr_1,
                      abs_wind_apr_2 = abs_wind_apr_2,
                      abs_wind_may_1 = abs_wind_may_1,
                      abs_wind_may_2 = abs_wind_may_2,
                      abs_wind_jun_1 = abs_wind_jun_1,
                      abs_wind_jun_2 = abs_wind_jun_2,
                      abs_wind_jul_1 = abs_wind_jul_1,
                      abs_wind_jul_2 = abs_wind_jul_2,
                      abs_wind_aug_1 = abs_wind_aug_1,
                      abs_wind_aug_2 = abs_wind_aug_2)

# ----------------- #
list_wind_roses <- list()

for (i in 1:length(list_dirs)) {
    raster_dirs <- list_dirs[[i]]
    raster_abs_ws <- list_abs_wind[[i]]
    raster_name <- names(list_dirs[i])
    wind_df <- data.frame()
    
    for(j in 1:nlayers(raster)){
        
        print(paste("Avancement: ",
                        i,
                        ".",
                        j,
                        sep = ""))
         wind <- data.frame(name = raster_name,
                            wd1 = values(raster_dirs[[j]]),
                            ws = values(raster_abs_ws[[i]]))
         
         wind$wd0_360 <- ifelse(wind$wd1 >= 0,
                                wind$wd1,
                                360 + wind$wd1) # handmade conversion from -180/180 to 0/360 for windRose() plot
         
         wind_df <- rbind(wind_df,
                          wind)
    }

     list_wind_roses[[i]] <- wind_df
     names(list_wind_roses[i]) <- unique(wind_df$name)
}

head(list_wind_roses[[1]])


lapply(list_wind_roses, function(x){
    x11()
windRose(mydata = x, # meteorological wind projection with N = 0 = 360, E = 90, S = 180, W = 270 - Necessity to have angle from 0 to 360
         wd = "wd0_360",
         ws = "ws",
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
         col = viridis(5, option = "D"))
})

##################################################################
# Wind roses for wind direction & abs speed under bird relocs ####
##################################################################

# -----> Loading bird data #
############################

argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed.rds")

names(argos)
year(argos$Date)


# -----> Variable creation for bi-monthly indices #
###################################################

#-----> 2017 #
############## 

argos_2017 <- argos[year(argos$Date) == 2017,]
dim(argos_2017)

argos_2017$bimonth <- ifelse(day(argos_2017$Date) %in% 1:15,
                              paste(months(argos_2017$Date), 1, sep = "-"),
                              paste(months(argos_2017$Date), 2, sep = "-"))
 
head(argos_2017)

argos_2017$wind_dir_0_360 <- ifelse(argos_2017$wind_dir >= 0,
                                    argos_2017$wind_dir,
                                    360 + argos_2017$wind_dir) # handmade conversion from-180/180 to 0/360 for windRose() plot
     
x11()
windRose(mydata = argos_2017,
         wd = "wind_dir_0_360",
         ws = "abs_ws",
         type = "bimonth",
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))


argos_2017_list <- split(argos_2017,
                         argos_2017$bimonth)

lapply(argos_2017_list, function(x){
    # x11()
windRose(mydata = x,
         wd = "wind_dir_0_360",
         ws = "abs_ws",
         type = "bimonth",
         max.freq = 90,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))

png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/Wind_Roses/Wind_under_birds_relocs/2017_bimonthly/2017_",
          unique(x$bimonth),
          ".png",
          sep = ""),
    res=300,
    width=30,
    height= 30,
    pointsize=12,
    unit="cm",
    bg="white")
windRose(mydata = x,
         wd = "wind_dir_0_360",
         ws = "abs_ws",
         type = "bimonth",
         max.freq = 90,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))
dev.off()
})
# ?windRose

# -----> 2018 #
###############

argos_2018 <- argos[year(argos$Date) != 2017,]
dim(argos_2018)
table(year(argos_2018$Date))

argos_2018$bimonth <- ifelse(day(argos_2018$Date) %in% 1:15,
                              paste(months(argos_2018$Date), 1, sep = "-"),
                              paste(months(argos_2018$Date), 2, sep = "-"))
 
head(argos_2018)

argos_2018$wind_dir_0_360 <- ifelse(argos_2018$wind_dir >= 0,
                                    argos_2018$wind_dir,
                                    360 + argos_2018$wind_dir) 
# handmade conversion from-180/180 to 0/360 for windRose() plot
     
x11()
windRose(mydata = argos_2018,
         wd = "wind_dir_0_360",
         ws = "abs_ws",
         type = "bimonth",
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))


argos_2018_list <- split(argos_2018,
                         argos_2018$bimonth)

lapply(argos_2018_list, function(x){
    x11()
windRose(mydata = x,
         wd = "wind_dir_0_360",
         ws = "abs_ws",
         type = "bimonth",
         max.freq = 90,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))

png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/Wind_Roses/Wind_under_birds_relocs/2018_bimonthly/",
          unique(year(x$Date)),
          "_",
          unique(x$bimonth),
          ".png",
          sep = ""),
    res=300,
    width=30,
    height= 30,
    pointsize=12,
    unit="cm",
    bg="white")
windRose(mydata = x,
         wd = "wind_dir_0_360",
         ws = "abs_ws",
         type = "bimonth",
         max.freq = 90,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))
dev.off()
})
graphics.off()

##############################################
# Wind roses for bird direction bimonthly ####
##############################################
g <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_argos_&_speed_ltraj.rds")

str(g)
g2 <- do.call("rbind", g)
summary(g2$abs.angle) # angles in radians
summary(g2$rel.angle)

# -----> Working on the absolute angle alpha
# Which corresponds to 180 * atan2(dx, dy) / pi

# -----> Conversion from radian to degree
# angles from -180 to +180 in mathematical convention
g2$abs.angle_deg <- g2$abs.angle * 180/pi
summary(g2$abs.angle_deg)

# ----- > Conversion from -180/180 to 0/360 system
# handmade conversion from -180/180 to 0/360 for windRose() plot
g2$bird_dir_0_360 <- ifelse(g2$abs.angle_deg >= 0,
                            g2$abs.angle_deg,
                            360 + g2$abs.angle_deg)

# -----> Conversion from math angle convention to METEOROLOGICAL angle convention
# https://math.stackexchange.com/questions/1589793/a-formula-to-convert-a-counter-clockwise-angle-to-clockwise-angle-with-an-offset

# ----------------> TOWARD DIRECTION 
g2$bird_0_360_METEO_TOWARD <- (-1 * g2$bird_dir_0_360 + 90) %% 360 # Direction que prennent les oiseaux

# ----------------> FROM WHICH DIRECTION

g2$bird_0_360_METEO_FROM <- ifelse(g2$bird_0_360_METEO_TOWARD <= 180,
                                   g2$bird_0_360_METEO_TOWARD + 180,
                                   g2$bird_0_360_METEO_TOWARD - 180) # Direction d'où partent les oiseaux
summary(g2$bird_0_360_METEO_FROM)

# -----> Calculating the absolute bird speed from dy & dx 
g2$abs_bird_speed <- sqrt(g2$dx^2 + g2$dy^2)
summary(g2$abs_bird_speed)

# -----> Merge with argos df
# -----> 2017
g2_2017 <- g2[year(g2$date) == 2017,]
dim(g2_2017)

argos_2017_2 <- left_join(argos_2017,
                          g2_2017[, -c(1, 2, 14)],
                          by = c("Vessel" = "Vessel",
                                 "Date" = "date"))

argos_2017_2_list <- split(argos_2017_2,
                           argos_2017_2$bimonth)
# -----> 2018
g2_2018 <- g2[year(g2$date) != 2017,]
dim(g2_2018)

argos_2018_2 <- left_join(argos_2018,
                          g2_2018[, -c(1, 2, 14)],
                          by = c("Vessel" = "Vessel",
                                 "Date" = "date"))

argos_2018_2_list <- split(argos_2018_2,
                           argos_2018_2$bimonth)

# -----> 2018 ####
##################
lapply(argos_2017_2_list, function(x){
    x11()
windRose(mydata = x,
         wd = "bird_0_360_METEO_TOWARD",
         ws = "speed.m.s",
         type = "bimonth",
         max.freq = 35,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))

png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/Wind_Roses/Bird_directions/2017_bimonthly/BIRD_DIR_TOW_",
          unique(year(x$Date)),
          "_",
          unique(x$bimonth),
          ".png",
          sep = ""),
    res = 300,
    width = 30,
    height = 30,
    pointsize = 12,
    unit = "cm",
    bg = "white")

windRose(mydata = x,
         wd = "bird_0_360_METEO_TOWARD",
         ws = "speed.m.s",
         type = "bimonth",
         max.freq = 35,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))
dev.off()
})

# -----> 2018 ####
##################
lapply(argos_2018_2_list, function(x){
    x11()
windRose(mydata = x,
         wd = "bird_0_360_METEO_TOWARD",
         ws = "speed.m.s",
         type = "bimonth",
         max.freq = 35,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))

png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/Wind_Roses/Bird_directions/2018_bimonthly/BIRD_DIR_TOW_",
          unique(year(x$Date)),
          "_",
          unique(x$bimonth),
          ".png",
          sep = ""),
    res = 300,
    width = 30,
    height = 30,
    pointsize = 12,
    unit = "cm",
    bg = "white")

windRose(mydata = x,
         wd = "bird_0_360_METEO_TOWARD",
         ws = "speed.m.s",
         type = "bimonth",
         max.freq = 35,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))
dev.off()
})
