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

mer_stack <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_north_stack.rds")

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

# ----- #
x11()
vectorplot(raster::stack(mean_u_2017, mean_v_2017),
           isField = 'dXY',
           aspX = 0.4,
           region = T,
           narrows = 500,
           lwd.arrows = 1)


# YEAR 2018 ####
################

u_2018 <- dropLayer(zon_stack,
                    match(period_1,
                          names(zon_stack)))
names(u_2018)

mean_u_2018 <- mean(u_2018)

# ----- #
v_2018 <- dropLayer(mer_stack,
                    match(period_1,
                          names(mer_stack)))
names(v_2018)

mean_v_2018 <- mean(v_2018)

# ----- #
x11()
vectorplot(raster::stack(mean_u_2018, mean_v_2018),
           isField = 'dXY',
           aspX = 0.4,
           region = T,
           narrows = 500,
           lwd.arrows = 1)


############################################
# ---- > Calculating the wind direction ####
############################################

# LET'S GET TRYING #
# https://gis.stackexchange.com/questions/306138/get-wind-direction-from-raster-stack-u-and-v-in-r

# YEAR 2017 ####
################

dirs_2017 <- 180 * atan2(v_2017, u_2017) / pi # atan2 gives direction in radian, then *180/pi allows the conversion in degree from -180 to 180
# In addition, atan2 gives the angle with METEOROLOGICAL convention
# N = 0 = 360, E = 90, S = 180, W = 270
dirs_2017 # From -180 to 180

abs_wind_sp_2017 <- sqrt(u_2017^2 + v_2017^2)
abs_wind_sp_2017

# YEAR 2018 ####
################

dirs_2018 <- 180 * atan2(v_2018, u_2018) / pi # atan2 gives direction in radian, then *180/pi allows the conversion in degree from -180 to 180
# In addition, atan2 gives the angle with METEOROLOGICAL convention
# N = 0 = 360, E = 90, S = 180, W = 270
dirs_2018 # From -180 to 180

abs_wind_sp_2018 <- sqrt(u_2018^2 + v_2018^2)
abs_wind_sp_2018

#############################################
# ---- > Visualization of wind direction ####
#############################################

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
          col = viridis(5, option = "A"))

}

# summary(wind)
# x11()
# hist(wind$wd0_360)
# x11()
# hist(wind$wd1)

# x11()

# From 0 to 360
x11()
windRose(mydata = wind,
         wd = "wd2",
         ws = "ws",
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = T,
         grid.line = 5,
     #     key = list(labels = c(">0 - 2",
     #                           ">2 - 5",
     #                           ">5 - 8",
     #                           ">8 - 11",
     #                           "> 11")),
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
         col = viridis(5))

# Next step - Work with the type argument of the function
dim(wind)
wind$type <- rep(1:15, 13363)
wind$type <- as.factor(wind$type)

x11()
windRose(mydata = wind,
         wd = "wd0_360",
         ws = "ws",
         type = "type",
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = T,
         grid.line = 5,
     #     key = list(labels = c(">0 - 2",
     #                           ">2 - 5",
     #                           ">5 - 8",
     #                           ">8 - 11",
     #                           "> 11")),
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
         col = viridis(5))


##############################################
#### Bird speed vs wind speed exploration ####
##############################################

plot(argos_df$wind_speed[argos_df$speed.m.sec < 1000],
     argos_df$speed.m.sec[argos_df$speed.m.sec < 1000])

plot(argos_df$wind_speed[argos_df$speed.m.sec < 200],
     argos_df$speed.m.sec[argos_df$speed.m.sec < 200])

plot(argos_df$wind_speed[argos_df$speed.m.sec < 50],
     argos_df$speed.m.sec[argos_df$speed.m.sec < 50])

################################

names(argos_df)
dim(argos_df)

argos_list <- split(argos_df, argos_df$Vessel)
par(mfrow = c(3, 5))

lapply(argos_list, function(x) {
    plot(x$wind_speed)
    })

lapply(argos_list, function(x) {
    plot(x$speed.m.sec,
         type = 'l')
    })


par(mfrow = c(3, 1))
plot(argos_list[[1]]$wind_speed,
     type = 'l')
plot(argos_list[[1]]$CHLO,
     col = 'darkgreen',
     type = 'l')
plot(argos_list[[1]]$SST,
     col = 'darkorange',
     type = 'l')
View()
