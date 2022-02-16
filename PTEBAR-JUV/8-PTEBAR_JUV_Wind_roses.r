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

# png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/Wind_Roses/Wind_under_birds_relocs/2017_bimonthly/2017_",
#           unique(x$bimonth),
#           ".png",
#           sep = ""),
#     res=300,
#     width=30,
#     height= 30,
#     pointsize=12,
#     unit="cm",
#     bg="white")
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
# dev.off()
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

# png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/Wind_Roses/Wind_under_birds_relocs/2018_bimonthly/",
#           unique(year(x$Date)),
#           "_",
#           unique(x$bimonth),
#           ".png",
#           sep = ""),
#     res=300,
#     width=30,
#     height= 30,
#     pointsize=12,
#     unit="cm",
#     bg="white")
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
          col = viridis(5, option = "D", direction = -1))
# dev.off()
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

# png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/Wind_Roses/Bird_directions/2017_bimonthly/BIRD_DIR_TOW_",
#           unique(year(x$Date)),
#           "_",
#           unique(x$bimonth),
#           ".png",
#           sep = ""),
#     res = 300,
#     width = 30,
#     height = 30,
#     pointsize = 12,
#     unit = "cm",
#     bg = "white")

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
# dev.off()
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

# png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/Wind_Roses/Bird_directions/2018_bimonthly/BIRD_DIR_TOW_",
#           unique(year(x$Date)),
#           "_",
#           unique(x$bimonth),
#           ".png",
#           sep = ""),
#     res = 300,
#     width = 30,
#     height = 30,
#     pointsize = 12,
#     unit = "cm",
#     bg = "white")

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
# dev.off()
})

###########################################
# Wind roses per bursts for each birds ####
###########################################

argos_2018_2_lnames <- split(argos_2018_2,
                             argos_2018_2$Vessel)
names(argos_2018_2_lnames)

fak_1 <- argos_2018_2_lnames[['166564']]
dim(fak_1)
fak_2 <- argos_2018_2_lnames[['166565']]
dim(fak_2)

names(fak_1)
class(fak_1$point.group)
table(fak_1$point.group)
fak_1$point.group <- as.factor(fak_1$point.group)
fak_1_l <- split(fak_1, 
                 fak_1$point.group)
x <- fak_1_l[[2]]
x

lapply(fak_1_l,
       function(x){
         x11()
         windRose(mydata = x,
         wd = "bird_0_360_METEO_TOWARD",
         ws = "speed.m.s",
        #  type = "point.group",
         max.freq = 35,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))})

### UGLY !!!!

#############################################
# Wind roses of bird directions per week ####
#############################################
# Focus sur les 6 premières semaines

argos_2017_2 <- droplevels(argos_2017_2)
argos_2018_2 <- droplevels(argos_2018_2)

argos_2017_2$week_numb <- week(argos_2017_2$Date)
table(argos_2017_2$week_numb)
table(year(argos_2017_2$Date))
table(argos_2017_2$Vessel)
argos_2017_2_lnames <- split(argos_2017_2,
                             argos_2017_2$Vessel)
names(argos_2017_2_lnames)
# ----- #
argos_2018_2$week_numb <- week(argos_2018_2$Date)
table(argos_2018_2$week_numb)
table(year(argos_2018_2$Date))
table(argos_2017_2$Vessel)

argos_2018_2_lnames <- split(argos_2018_2,
                             argos_2018_2$Vessel)
names(argos_2018_2_lnames)
# -----> 2017 ####
##################

lapply(argos_2017_2_lnames,
       function(x){
    vessel <- unique(x$Vessel)
    data <- x[x$week_numb %in% 15:20,]
    data$week_numb <- as.factor(data$week_numb)
    
    x11()
windRose(mydata = data,
         wd = "bird_0_360_METEO_TOWARD",
         ws = "speed.m.s",
         type = "week_numb",
         max.freq = 35,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D", direction = -1),
          main = paste("bird direction - ",
                       vessel,
                       sep = ""))

x11()
windRose(mydata = data,
         wd = "wind_dir_0_360",
         ws = "abs_ws",
         type = "week_numb",
         max.freq = 80,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 15,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D", direction = -1),
          main = paste("wind direction - ",
                       vessel,
                       sep = ""))
})
graphics.off()

# -----> 2018 ####
##################
# Comparaison sudistes vs. nordistes sur les 6 premières semaines

# les sudistes
##############

fak_1 <- argos_2018_2_lnames[['166564']]
dim(fak_1)
fak_2 <- argos_2018_2_lnames[['166565']]
dim(fak_2)

table(fak_1$week_numb)
table(fak_2$week_numb)

fak_1$week_numb <- as.factor(fak_1$week_numb)
fak_2$week_numb <- as.factor(fak_2$week_numb)

x11()
windRose(mydata = fak_1,
         wd = "bird_0_360_METEO_TOWARD",
         ws = "speed.m.s",
         type = "week_numb",
         max.freq = 35,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D", direction = -1))
x11()
windRose(mydata = fak_1[fak_1$week_numb %in% 15:20,],
         wd = "wind_dir_0_360",
         ws = "abs_ws",
         type = "week_numb",
         max.freq = 80,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 15,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D", direction = -1))

x11()
windRose(mydata = fak_2[fak_2$week_numb %in% 15:20,],
         wd = "bird_0_360_METEO_TOWARD",
         ws = "speed.m.s",
         type = "week_numb",
         max.freq = 35,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D", direction = -1))

x11()
windRose(mydata = fak_2[fak_2$week_numb %in% 15:20,],
         wd = "wind_dir_0_360",
         ws = "abs_ws",
         type = "week_numb",
         max.freq = 80,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 15,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D", direction = -1))

# les nordistes

names_Vessel <- names(argos_2018_2_lnames)[names(argos_2018_2_lnames) != c("166564", "166565")]

for (i in names_Vessel){
    data <- argos_2018_2_lnames[[i]]
    data$week_numb <- as.factor(data$week_numb)
    
    x11()
windRose(mydata = data[data$week_numb %in% 15:20,],
         wd = "bird_0_360_METEO_TOWARD",
         ws = "speed.m.s",
         type = "week_numb",
         max.freq = 35,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D", direction = -1),
          main = paste("bird direction - ",
                       i,
                       sep = ""))

x11()
windRose(mydata = data[data$week_numb %in% 15:20,],
         wd = "wind_dir_0_360",
         ws = "abs_ws",
         type = "week_numb",
         max.freq = 80,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 15,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D", direction = -1),
          main = paste("wind direction - ",
                       i,
                       sep = ""))
}
graphics.off()

# -----> Merge and save new dataframe ####
##########################################

class(argos_2018_2)
class(argos_2017_2)

argos3 <- rbind(argos_2017_2,
                argos_2018_2)
head(argos3)
names(argos3)
# saveRDS(argos3,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
