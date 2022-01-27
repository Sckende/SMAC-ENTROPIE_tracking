getwd()
library(languageserver)
library(sf)
rm(list = ls())

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



plot(argos_env_list[[1]]$wind_speed,
     type = 'l')
plot(argos_env_list[[1]]$CHLO,
     col = 'darkgreen',
     type = 'l')
plot(argos_env_list[[1]]$SST,
     col = 'darkorange',
     type = 'l')
View()
