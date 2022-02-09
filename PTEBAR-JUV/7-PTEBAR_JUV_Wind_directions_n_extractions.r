rm(list = ls())
# library(languageserver)
library(sf)
library(rasterVis)
library(viridis)
library(stringr)
library(terra)
library(raster)
library(viridis)
library(openair)
library(circular)

######################
#### Data loading ####
######################


argos_sf <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA.rds")
argos_df <- st_drop_geometry(argos_sf)


list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/")

head(argos_df)
class(argos_sf)
class(argos_df)

summary(argos_df)

####################################
#### Wind direction exploration ####
####################################

######################################################
# -----> Map of wind direction with vectorplot() #####
######################################################

# u = Zonal velocity = x = east
# v = Meridional velocity = y = north

zon_stack <-readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_east_stack.rds")
zon_stack <- zon_stack[[order(names(zon_stack))]]
names(zon_stack)
zon_stack[[1]]
zon_stack[[5]]
zon_stack[[60]]

mer_stack <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_north_stack.rds")
mer_stack <- mer_stack[[order(names(mer_stack))]]
names(mer_stack)
mer_stack[[1]]
mer_stack[[5]]
mer_stack[[60]]

all(names(zon_stack) == names(mer_stack))


# YEAR 2017 ####
################

period_1 <- names(zon_stack)[str_which(names(zon_stack), "X2017.")]

u_2017 <- zon_stack[[which(names(zon_stack) %in% period_1)]]
names(u_2017)
hist(values(u_2017[[1]]))

mean_u_2017 <- mean(u_2017)

# ----- #
v_2017 <- mer_stack[[which(names(mer_stack) %in% period_1)]]
names(v_2017)
hist(values(v_2017[[1]]))

mean_v_2017 <- mean(v_2017)
mean_v_2017

# ----- #
x11()
vectorplot(raster::stack(mean_u_2017, mean_v_2017),
           isField = 'dXY',
           aspX = 0.4,
           region = T,
           narrows = 500,
           lwd.arrows = 1)
dev.off()

# YEAR 2018 ####
################

u_2018 <- dropLayer(zon_stack,
                    match(period_1,
                          names(zon_stack)))
names(u_2018)

mean_u_2018 <- mean(u_2018)
mean_u_2018

# ----- #
v_2018 <- dropLayer(mer_stack,
                    match(period_1,
                          names(mer_stack)))
names(v_2018)

mean_v_2018 <- mean(v_2018)
mean_v_2018

# ----- #
x11()
vectorplot(raster::stack(mean_u_2018, mean_v_2018),
           isField = 'dXY',
           aspX = 0.4,
           region = T,
           narrows = 500,
           lwd.arrows = 1)
dev.off()


############################################
# ---- > Calculating the wind direction ####
############################################

# LET'S GET TRYING #
# https://gis.stackexchange.com/questions/306138/get-wind-direction-from-raster-stack-u-and-v-in-r

# YEAR 2017 ####
################

# dirs_2017 <- 180 * atan2(v_2017, u_2017) / pi # atan2 gives direction in radian, then *180/pi allows the conversion in degree from -180 to 180
# In addition, atan2 gives the angle with METEOROLOGICAL convention
# N = 0 = 360, E = 90, S = 180, W = 270
# dirs_2017

# saveRDS(dirs_2017,
     #    "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_dirs_2017.rds")

dirs_2017 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_dirs_2017.rds")
dirs_2017 # From -180 to 180

# -----> absolute wind speed from u & v components
##################################################

# abs_wind_sp_2017 <- sqrt(u_2017^2 + v_2017^2)
# abs_wind_sp_2017
# names(abs_wind_sp_2017) <- names(u_2017)
# saveRDS(abs_wind_sp_2017,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/abs_wind_speed_2017.rds")


abs_wind_sp_2017 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/abs_wind_speed_2017.rds")

# YEAR 2018 ####
################

# dirs_2018 <- 180 * atan2(v_2018, u_2018) / pi # atan2 gives direction in radian, then *180/pi allows the conversion in degree from -180 to 180
# In addition, HERE the atan2 gives the angle with METEOROLOGICAL convention
# N = 0 = 360, E = 90, S = 180, W = 270
# dirs_2018
# names(dirs_2018) <- names(u_2018)
# saveRDS(dirs_2018,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_dirs_2018.rds")

dirs_2018 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_dirs_2018.rds")
dirs_2018 # From -180 to 180

# -----> absolute wind speed fromm u & v components
###################################################

# abs_wind_sp_2018 <- sqrt(u_2018^2 + v_2018^2)
# names(abs_wind_sp_2018) <- names(u_2018)
# saveRDS(abs_wind_sp_2018,
#         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/abs_wind_speed_2018.rds")

abs_wind_sp_2018 <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/abs_wind_speed_2018.rds")


###########################################
# ---- > Wind roses of wind directions ####
###########################################

t_2017 <- sample(1:744, 15) # random sampling of 15 layers
t_2018 <- sample(1:744, 15) # random sampling of 15 layers

for(i in t_2017){
     wind <- data.frame(wd1 = values(dirs_2017[[i]]),
                        ws = values(abs_wind_sp_2017[[i]]))
     
     wind$wd0_360 <- ifelse(wind$wd1 >= 0,
                       wind$wd1,
                       360 + wind$wd1) # handmade conversion from -180/180 to 0/360 for windRose() plot
     x11()
     windRose(mydata = wind, # meteorological wind projection with N = 0 = 360, E = 90, S = 180, W = 270 - Necessity to have angle from 0 to 360
         wd = "wd0_360",
         ws = "ws",
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
     #     key = list(labels = c(">0 - 2",
     #                           ">2 - 5",
     #                           ">5 - 8",
     #                           ">8 - 11",
     #                           "> 11")),
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))

}

# --- #
for(i in t_2018){
     wind <- data.frame(wd1 = values(dirs_2018[[i]]),
                        ws = values(abs_wind_sp_2018[[i]]))
     
     wind$wd0_360 <- ifelse(wind$wd1 >= 0,
                       wind$wd1,
                       360 + wind$wd1) # handmade conversion from -180/180 to 0/360 for windRose() plot
     x11()
     windRose(mydata = wind, # meteorological wind projection with N = 0 = 360, E = 90, S = 180, W = 270 - Necessity to have angle from 0 to 360
         wd = "wd0_360",
         ws = "ws",
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
     #     key = list(labels = c(">0 - 2",
     #                           ">2 - 5",
     #                           ">5 - 8",
     #                           ">8 - 11",
     #                           "> 11")),
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5,
                        option = "A",
                        begin = 1,
                        end = 0))

}


# Next step - Work with the type argument of the function

###########################################
# ---- > Wind roses of bird directions ####
###########################################

g <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_argos_&_speed_ltraj.rds")

str(g)
g2 <- do.call("rbind", g)
summary(g2$abs.angle) # angles in radians
summary(g2$rel.angle)

tt <- g[[3]] 
dim(tt)
names(tt)
class(tt)

plot(tt$x,
     tt$y,
     type = "b")

test <- tt
plot(test$x,
     test$y,
     type = "b")

# Working on the absolute angle alpha
#####################################
# Which corresponds to 180 * atan2(dx, dy) / pi

# -----> Conversion from radian to degree
test$abs.angle_deg <- test$abs.angle * 180/pi # angles from -180 to +180 in mathematical convention
summary(test$abs.angle_deg)

# ----- > Conversion from -180/180 to 0/360 system
test$wd0_360 <- ifelse(test$abs.angle_deg >= 0,
                       test$abs.angle_deg,
                       360 + test$abs.angle_deg) # handmade conversion from -180/180 to 0/360 for windRose() plot

hist(test$wd0_360[test$wd0_360 > 180])
hist(test$abs.angle_deg[test$abs.angle_deg < 0])

# -----> Conversion from math angle convention to METEOROLOGICAL angle convention
# https://math.stackexchange.com/questions/1589793/a-formula-to-convert-a-counter-clockwise-angle-to-clockwise-angle-with-an-offset

# ----------------> TOWARD DIRECTION 

test$wd0_360_METEO_TOWARD <- (-1 * test$wd0_360 + 90) %% 360 # Direction que prennent les oiseaux

# ----------------> FROM WHICH DIRECTION

test$wd0_360_METEO_FROM <- ifelse(test$wd0_360_METEO <= 180,
                               test$wd0_360_METEO + 180,
                               test$wd0_360_METEO - 180) # Direction d'où partent les oiseaux
summary(test$wd0_360_METEO_FROM)

# -----> Calculating the absolute wind speed from dy & dx 
test$abs.speed <- sqrt(test$dx^2 + test$dy^2)

# -----> Wind roses
x11()
windRose(mydata = test, # meteorological wind projection with N = 0 = 360, E = 90, S = 180, W = 270 - Necessity to have angle from 0 to 360
         wd = "wd0_360_METEO_TOWARD",
         ws = "speed.m.s",
         breaks = c(0, 2, 5, 8, 11, 17),
         angle = 20,
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
          col = viridis(5, option = "D"))
x11()
plot(test$x,
     test$y,
     type = "b")

test$burst <- substring(row.names(test), 7, 10)

# NEXT STEP, split in range of time
# Extract wind direction for each loc of birds
# Extract bathymetrie for each loc of birds

head(argos3)
str(argos3)
vessel <- unique(argos3$Vessel)

plot(argos3$Date[argos3$Vessel == vessel[1]],
     argos3$bathy[argos3$Vessel == vessel[1]])
