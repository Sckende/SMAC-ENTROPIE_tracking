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

######################################
#### Wind orientation exploration ####
######################################

# -----> Map of wind orientation #####
# u = Zonal velocity = x = east
# v = Meridional velocity = y = north

zon_stack <-readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_east_stack.rds")

mer_stack <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_north_stack.rds")

all(names(zon_stack) == names(mer_stack))

#############
# YEAR 1 ####
#############

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
#############
# YEAR 2 ####
#############

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


# ---- > Calculating  the wind direction ####
# LET'S GET TRYING #
# https://gis.stackexchange.com/questions/306138/get-wind-direction-from-raster-stack-u-and-v-in-r

dirs_2017 <- 180 * atan2(v_2017, u_2017) / pi # atan2 gives direction in radian, then *180/pi allows the conversion in degree from -180 to 180
# In addition, atan2 gives the angle with METEOROLOGICAL convention
# N = 0 = 360, E = 90, S = 180, W = 270

dirs_2017 # From -180 to 180

abs_wind_sp <- sqrt(u_2017^2 + v_2017^2)
abs_wind_sp

x11()
par(mfrow = c(3, 5))

t <- sample(1:744, 15) # random sampling of 15 layers

for(i in t){
     wind <- data.frame(wd1 = values(dirs_2017[[i]]),
                        ws = values(abs_wind_sp[[i]]))
     
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
          col = viridis(5))

}

dim(wind)
summary(wind)


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

circular::windrose(x = wind$wd1,
                   y = wind$ws,
                   bins = 36)
hist(wind$wd1)

# ANOTHER TRY !!! #
# help(atan2) # return the angle in radians & WARNING, y before x in the arguments
# Tries to computation of the wind orientation
# https://stackoverflow.com/questions/21484558/how-to-calculate-wind-direction-from-u-and-v-wind-components-in-r

u <- u_2017[[1]]
# u_mean <- mean_u_2017
v <- v_2017[[1]]
# v_mean <- mean_v_2017

# Calculating the wind VECTOR AZIMUTH in radian
wind_POLAR_rad <- atan2(v, u)
hist(values(wind_POLAR_rad))

# Converting from radian angles to degree angle
wind_POLAR_degree <- wind_POLAR_rad * (180/pi)
hist(values(wind_POLAR_degree))

# Calculating the METEOROLOGICAL WIND DIRECTION
wind_METEO_dir <- wind_POLAR_degree + 180
hist(values(wind_METEO_dir))

x11()
vectorplot(wind_METEO_dir)

x11()
vectorplot(wind_POLAR_degree)

x11()
vectorplot(wind_POLAR_rad)

x11()
vectorplot(raster::stack(u, v),
           isField = 'dXY')





# Normalization of components
abs_wind <- sqrt(u^2 + v^2)
mean_abs_wind <- mean(abs_wind)

# Conversion
wind_dir_trig_to <- atan2(v_mean/mean_abs_wind, u_mean/mean_abs_wind)
hist(values(wind_dir_trig_to))
 
wind_dir_trig_to_degrees = wind_dir_trig_to * 180/pi ## -111.6 degrees
hist(values(wind_dir_trig_to_degrees))

wind_dir_trig_from_degrees = wind_dir_trig_to_degrees + 180 ## 68.38 degrees
hist(values(wind_dir_trig_from_degrees))

wind_dir_cardinal = 90 - wind_dir_trig_from_degrees
hist(values(wind_dir_cardinal))

x11()
vectorplot(wind_dir_cardinal)









#########################################
# Visualization of wind direction #
#####################################

################
# ----> test 1 #
################

angles <- c(0, 45, 90, 180, 270) # in degrees
angles <- sample(values(wind_METEO_dir), 1000)
x0 <- 4
y0 <- 4
lineLength <- 2

x11()
plot(x0, y0, xlim=c(1, 8), ylim=c(1, 8), pch=15, cex=3)

for (i in 1:length(angles)) {
    x1 <- x0 + lineLength * cos(angles[i] * pi / 180)
    y1 <- y0 + lineLength * sin(angles[i] * pi / 180)
    
    segments(x0,
             y0,
             x1,
             y1,
          #    lwd = i,
             lwd = 1,
             col = rgb(0, 0, 255, max = 255, alpha = 50))
    
    
#     x1 <- x0 + (lineLength * 1.3) * cos(angles[i] * pi / 180)
#     y1 <- y0 + (lineLength * 1.3) * sin(angles[i] * pi / 180)

#     text(x1, y1, labels=angles[i])
    
}
print("Ayééé")

################
# ----> test 2 #
################
# https://www.youtube.com/watch?v=X3ENW8v_m0c

library(openair)

windRose(mydata = mydata)
help(windRose)

u <- u_2017
v <- v_2017

# Calculating the wind VECTOR AZIMUTH in radian
wind_POLAR_rad <- atan2(v, u)

# Converting from radian angles to degree angle
wind_POLAR_degree <- wind_POLAR_rad * (180/pi)

# Calculating the METEOROLOGICAL WIND DIRECTION
wind_METEO_dir <- wind_POLAR_degree + 180 # I'm really not sure here

# Calculating the absolute WIND SPEED
abs_wind_sp <- sqrt(u^2 + v^2)


wind <- data.frame(wd = values(mean(wind_METEO_dir)),
                   wd2 = values(mean(wind_POLAR_degree)),
                   ws = values(mean(abs_wind)))
summary(wind)
x11()
windRose(mydata = wind,
         wd = "wd2",
         ws = "ws",
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key = list(labels = c(">0 - 2",
                               ">2 - 5",
                               ">5 - 8",
                               ">8 - 11",
                               "> 11")),
         key.footer = "WSP (m/s)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
     #     col = c("#4f4f4f",
     #             "#0a7cb9",
     #             "#f9be00",
     #             "#ff7f2f",
     #             "#d7153a"),
          col = viridis(5))

vectorplot(raster::stack(u, v))

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
