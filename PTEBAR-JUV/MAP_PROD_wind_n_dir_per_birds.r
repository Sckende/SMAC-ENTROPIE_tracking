#################################################################
# -----> OBJ.: wind map per week with single bird trajectory ####
#################################################################
rm(list = ls())
# -----> packages ####
######################

source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r')

# -----> Data ####
##################

argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")

## YEAR 1 - 2017 ####
speed1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652088629_YEAR1_SPEED.nc')

zon1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637651769964_YEAR1_ZONAL.nc')
mer1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637651880863_YEAR1_MERIDIONAL.nc')

## YEAR 2 - 2018 ####
speed2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637653002349_YEAR2_SPEED.nc')

zon2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652399617_YEAR2_ZONAL.nc')
mer2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652499961_YEAR2_MERIDIONAL.nc')

# -----> Week information in names of raster lyers ####
#######################################################

all(names(mer1) == names(zon1))
all(names(mer1) == names(speed1))

all(names(mer2) == names(zon2))
all(names(mer2) == names(speed2))

# -----> 2017
#############

date1 <- names(mer1)
date1 <- substr(date1,
                2,
                20)
date1 <- as.POSIXlt(date1,
                    format = "%Y.%m.%d.%H.%M.%S")
date11 <- paste(names(mer1),
                "-W",
                week(date1),
                sep = "")

names(mer1) <- date11
names(zon1) <- date11
names(speed1) <- date11

# -----> 2018
#############

date2 <- names(mer2)
date2 <- substr(date2,
                2,
                20)
date2 <- as.POSIXlt(date2,
                    format = "%Y.%m.%d.%H.%M.%S")
date22 <- paste(names(mer2),
                "-W",
                week(date2),
                sep = "")

names(mer2) <- date22
names(zon2) <- date22
names(speed2) <- date22

# -----> test ####
##################
argos_ls <- split(argos,
                  argos$Vessel)

# color option for wind scale
#############################

nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)


lapply(argos_ls,
       function(x){
           
print(unique(x$Vessel))
print(table(x$week_numb))
year_dep <- unique(year(x$deploy))           
weeks <- paste("W",
               unique(x$week_numb),
               sep = "")


x_sp <- SpatialPointsDataFrame(coords = x[, c("Longitude", "Latitude")],
                               data = x,
                               proj4string = CRS("EPSG:4326"))

for(i in weeks){
    
    print(i)
    arg_data <- x[x$week_numb == substr(i, 2, 3),] # subset data
    arg_data_2 <- arg_data[!is.na(arg_data$speed.m.s),] # retrait des des NA pour les vitesses des oiseaux
    
    if(dim(arg_data_2)[1] <= 2){
        next
    } # Avoid week with 2 points and less
    
    if(year_dep == 2017){
        mer <- mean(mer1[[names(mer1)[which(str_detect(names(mer1), i))]]])
        zon <- mean(zon1[[names(zon1)[which(str_detect(names(zon1), i))]]])
        speed <- mean(speed1[[names(speed1)[which(str_detect(names(speed1), i))]]])        
    } else {
        mer <- mean(mer2[[names(mer2)[which(str_detect(names(mer2), i))]]])
        zon <- mean(zon2[[names(zon2)[which(str_detect(names(zon2), i))]]])
        speed <- mean(speed2[[names(speed2)[which(str_detect(names(speed2), i))]]])
    }
    
    grey <- x_sp[x_sp$week_numb != substr(i, 2, 3),]
    print(grey)
    white <- x_sp[x_sp$week_numb == substr(i, 2, 3),]
    print(white)

    # x11()
    # WIND MAP
    ##########
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/WEEKLY/",
              year_dep,
              "-WindMap-",
              unique(x_sp$Vessel),
              "-",
              i,
              ".png",
              sep = ""),
        res = 300,
        width = 50,
        height = 30,
        pointsize = 12,
        unit = "cm",
        bg = "transparent")
    # x11()
    print(
    vectorplot(stack(zon, mer),
               isField = 'dXY',
               narrows = 200,
               lwd.arrows = 1,
               aspX = 0.4,
               region = speed,
               at = my_at,
               col.regions = my_cols,
               main = list(paste(year_dep,
                                 " - ",
                                 unique(x_sp$Vessel),
                                 " - ",
                                 i,
                                 " - n=",
                                 dim(x_sp[x_sp$week_numb == substr(i, 2, 3),])[1],
                                 sep = ""),
                           cex = 2),
               xlab = list("Longitude", 
                           cex = 2),
               ylab = list("Latitude",
                           cex = 2))  +
    layer(c(sp.points(grey,
                    col = "grey",
                    cex = 2),
            sp.points(white,
                    col = "white",
                    cex = 2)))
    )
    dev.off()
    
    # Wind Roses
    ############
#     png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/WindRoses/WEEKLY/",
#               year_dep,
#               "-BirdDir-",
#               unique(arg_data_2$Vessel),
#               "-",
#               i,
#               ".png",
#               sep = ""),
#         res = 300,
#         width = 30,
#         height = 30,
#         pointsize = 12,
#         unit = "cm",
#         bg = "white")
    
#     windRose(mydata = arg_data_2,
#          wd = "bird_0_360_METEO_TOWARD",
#          ws = "speed.m.s",
#          max.freq = 35,
#          breaks = c(0, 2, 5, 8, 11, 17),
#          auto.text = F,
#          paddle = F,
#          annotate = F,
#          grid.line = 5,
#          key.footer = "WSP (m/s)",
#          key.position = "bottom",
#          par.settings = list(axis.line = list(col = "lightgray")),
#          col = viridis(5, option = "D", direction = -1),
#          main = paste(year_dep,
#                        " - bird direction - ",
#                        unique(arg_data_2$Vessel),
#                        " - ",
#                        i,
#                        " - n=",
#                        dim(arg_data_2)[1],
#                        sep = ""))
# dev.off()

#     png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/WindRoses/WEEKLY/",
#               year_dep,
#               "-WindDir-",
#               unique(arg_data_2$Vessel),
#               "-",
#               i,
#               ".png",
#               sep = ""),
#         res = 300,
#         width = 30,
#         height = 30,
#         pointsize = 12,
#         unit = "cm",
#         bg = "white")
    
# windRose(mydata = arg_data_2,
#          wd = "wind_dir_0_360",
#          ws = "abs_ws",
#          max.freq = 80,
#          breaks = c(0, 2, 5, 8, 11, 17),
#          auto.text = F,
#          paddle = F,
#          annotate = F,
#          grid.line = 15,
#          key.footer = "WSP (m/s)",
#          key.position = "bottom",
#          par.settings = list(axis.line = list(col = "lightgray")),
#          col = viridis(5, option = "D", direction = -1),
#          main = paste(year_dep,
#                       " - wind direction - ",
#                       unique(arg_data_2$Vessel),
#                       " - ",
#                       i,
#                       " - n=",
#                       dim(arg_data_2)[1],
#                       sep = ""))
# dev.off()
}           
       })

# -----> Debugging space
##########################
PRINT DES WIND MAPS !!!!!!! ESSAYER EN SORTANT DE lapply et fair eune boucle à la place