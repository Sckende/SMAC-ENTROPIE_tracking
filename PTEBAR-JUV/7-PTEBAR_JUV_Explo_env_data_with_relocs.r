rm(list = ls())
library(languageserver)
library(sf)
library(rasterVis)
library(viridis)
library(stringr)

################
# Data loading #
################


argos_sf <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA.rds")
argos_df <- st_drop_geometry(argos_sf)


list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/")

head(argos_df)
class(argos_sf)
class(argos_df)

summary(argos_df)

################################
# Wind orientation exploration #
################################

# -----> Map of wind orientation #####
# u = Zonal velocity = x = east
# v = Meridional velocity = y = north

zon_stack <-readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_east_stack.rds")

mer_stack <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_north_stack.rds")

all(names(zon_stack) == names(mer_stack))
# YEAR 1
period_1 <- names(zon_stack)[str_which(names(zon_stack), "X2017.")]

u_2017 <- zon_stack[[which(names(zon_stack) %in% period_1)]]
names(u_2017)
# ----- #
v_2017 <- mer_stack[[which(names(mer_stack) %in% period_1)]]
names(v_2017)
# ----- #
vec_stack_2017 <- atan2(u_2017, v_2017) + 180
vec_stack_2017

# YEAR 2
u_2018 <- dropLayer(zon_stack,
                    match(period_1,
                          names(zon_stack)))
names(u_2018)
# ----- #
v_2018 <- dropLayer(mer_stack,
                    match(period_1,
                          names(mer_stack)))
names(v_2018)
# ----- #
vec_stack_2018 <- atan2(u_2018, v_2018) + 180
vec_stack_2018

hist(values(vec_stack))
mean_vec <- mean(vec_stack)
hist(mean_vec)

vectorplot(mean_vec,
           unit = 'degrees',
           col.regions = viridis(150),
           cuts = 149,
           region = T)


########################################
# Bird speed vs wind speed exploration #
########################################

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
