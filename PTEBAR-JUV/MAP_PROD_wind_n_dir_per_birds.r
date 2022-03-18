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

# -----> Speed of birds exploration ####
########################################
summary(argos$abs_bird_speed)
summary(argos$speed.m.sec) # calcul perso
summary(argos$speed.m.s) # calcul à partir de ltraj
summary(argos$speed.km.h) # à partir de ltraj
summary(argos$speed.m.sec * 3.6)
argos$speed_km_h_perso <- argos$speed.m.sec * 3.6

hard_speed <- argos[which(argos$speed_km_h_perso > 115),]
dim(hard_speed)
summary(hard_speed$reloc.del.sec/60)

argos <- argos[-which(argos$speed_km_h_perso > 115),]
summary(argos$speed_km_h_perso)

# -----> Figures production ####
########################################
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
weeks_lett <- paste("W",
                    unique(x$week_numb),
                    sep = "")
weeks_num <- unique(x$week_numb)



x_sp <- SpatialPointsDataFrame(coords = x[, c("Longitude", "Latitude")],
                               data = x,
                               proj4string = CRS("EPSG:4326"))
class(x_sp)
print(dim(x_sp))

for(i in 1:length(weeks_lett)){
    
    print(weeks_lett[i])
    arg_data <- x[x$week_numb == weeks_num[i],] # subset data
    arg_data_2 <- arg_data[!is.na(arg_data$speed_km_h_perso),] # retrait des des NA pour les vitesses des oiseaux
    arg_data_2$abs_ws_km_h <- arg_data_2$abs_ws * 3.6
    
    if(dim(arg_data_2)[1] <= 2){
        next
    } # Avoid week with 2 points and less
    
    if(year_dep == 2017){
        mer <- mean(mer1[[names(mer1)[which(str_detect(names(mer1), weeks_lett[i]))]]])
        zon <- mean(zon1[[names(zon1)[which(str_detect(names(zon1), weeks_lett[i]))]]])
        speed <- mean(speed1[[names(speed1)[which(str_detect(names(speed1), weeks_lett[i]))]]])        
    } else {
        mer <- mean(mer2[[names(mer2)[which(str_detect(names(mer2), weeks_lett[i]))]]])
        zon <- mean(zon2[[names(zon2)[which(str_detect(names(zon2), weeks_lett[i]))]]])
        speed <- mean(speed2[[names(speed2)[which(str_detect(names(speed2), weeks_lett[i]))]]])
    }

    # x11()
    # WEEKLY WIND MAP
    ##################
    # png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/WEEKLY/",
    #           year_dep,
    #           "-WindMap-",
    #           unique(x_sp$Vessel),
    #           "-",
    #           weeks_lett[i],
    #           ".png",
    #           sep = ""),
    #     res = 300,
    #     width = 50,
    #     height = 30,
    #     pointsize = 12,
    #     unit = "cm",
    #     bg = "transparent")
    # # x11()
    # print(
    # rasterVis::vectorplot(raster::stack(zon, mer),
    #            isField = 'dXY',
    #            narrows = 200,
    #            lwd.arrows = 1,
    #            aspX = 0.4,
    #            region = speed,
    #            at = my_at,
    #            col.regions = my_cols,
    #            main = list(paste(year_dep,
    #                              " - ",
    #                              unique(x_sp$Vessel),
    #                              " - ",
    #                              weeks_lett[i],
    #                              " - n=",
    #                              dim(x_sp[x_sp$week_numb == weeks_num[i],])[1],
    #                              sep = ""),
    #                        cex = 2),
    #            xlab = list("Longitude", 
    #                        cex = 2),
    #            ylab = list("Latitude",
    #                        cex = 2))  +
    # layer(c(sp.points(x_sp[x_sp$week_numb == weeks_num[i],], col = "white", lwd = 3),
    #         sp.points(x_sp[x_sp$week_numb != weeks_num[i],], col = "grey", lwd = 3)))
    
    # )
    # dev.off()
    
    # 6 hours WIND MAP
    ##################
    # 2017
    for(i in 1:nlayers(mer1)){
        
        png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/Indian_Ocean/2017/2017_WindMap-IO",
            names(mer1[[i]]),
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
    rasterVis::vectorplot(raster::stack(zon1[[i]], mer1[[i]]),
               isField = 'dXY',
               narrows = 800,
               lwd.arrows = 1,
               aspX = 0.3,
               region = speed1[[i]],
               at = my_at,
               col.regions = my_cols,
               main = names(mer1[[i]]))
    
    )
    dev.off()
    }

    # 2018
    for(i in 13:665){
        
        png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/Indian_Ocean/2018/2018_WindMap-IO",
            names(mer2[[i]]),
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
    rasterVis::vectorplot(raster::stack(zon2[[i]], mer2[[i]]),
               isField = 'dXY',
               narrows = 800,
               lwd.arrows = 1,
               aspX = 0.3,
               region = speed2[[i]],
               at = my_at,
               col.regions = my_cols,
               main = names(mer2[[i]]))
    
    )
    dev.off()
    }
    
    # -----> PPT creation ####
##########################

# File list
wind_2017 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/Indian_Ocean/2017/",
                        full.names = TRUE)
length(wind_2017)

wind_2018 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/Indian_Ocean/2018/",
                        full.names = TRUE)
length(wind_2018)

# Doc creation
doc <- read_pptx()
doc <- add_slide(doc,
                 layout = "Two Content",
                 master = "Office Theme")

for (i in 1:length(wind_2017)) {
    
    # file path
    img_2017 <- wind_2017[i]
    img_2018 <- wind_2018[i]
    
    doc <- ph_with(x = doc,
                   value = paste("%mm.%d.%h.%m = ",
                                 substr(wind_2017[i],
                                      107,
                                      117)),
                       location = ph_location_type(type = "title"))
        doc <- ph_with(x = doc,
                       value = paste(i, "/", length(wind_2017), sep = ""),
                       location = ph_location_type(type = "ftr"))
        doc <- ph_with(x = doc,
                       value = external_img(img_2017,
                                            width = 4.96,
                                            height = 2.98),
                       location = ph_location_left(),
                       use_loc_size = FALSE)
        doc <- ph_with(x = doc,
                       value = external_img(img_2018,
                                            width = 4.96,
                                            height = 2.98),
                       location = ph_location_right(),
                       use_loc_size = FALSE)
        doc <- add_slide(doc)
    print(i)
}

print(doc,
      target = "C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/Meeting_Henri_MARS_2022/Animation_MP4/input/WIND_IO_2017-2018.pptx")
    
# Wind Roses
############
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/WindRoses/WEEKLY/",
              year_dep,
              "-BirdDir-",
              unique(arg_data_2$Vessel),
              "-",
              weeks_lett[i],
              ".png",
              sep = ""),
        res = 300,
        width = 30,
        height = 30,
        pointsize = 12,
        unit = "cm",
        bg = "white")
    
    windRose(mydata = arg_data_2,
         wd = "bird_0_360_METEO_TOWARD",
         ws = "speed_km_h_perso",
         max.freq = 35,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 5,
         key.footer = "Bird speed (km/h)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
         col = viridis(5, option = "D", direction = -1),
         main = paste(year_dep,
                       " - bird direction - ",
                       unique(arg_data_2$Vessel),
                       " - ",
                       weeks_lett[i],
                       " - n=",
                       dim(arg_data_2)[1],
                       sep = ""))
dev.off()

    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/WindRoses/WEEKLY/",
              year_dep,
              "-WindDir-",
              unique(arg_data_2$Vessel),
              "-",
              weeks_lett[i],
              ".png",
              sep = ""),
        res = 300,
        width = 30,
        height = 30,
        pointsize = 12,
        unit = "cm",
        bg = "white")
    
windRose(mydata = arg_data_2,
         wd = "wind_dir_0_360",
         ws = "abs_ws_km_h",
         max.freq = 80,
         breaks = c(0, 2, 5, 8, 11, 17),
         auto.text = F,
         paddle = F,
         annotate = F,
         grid.line = 15,
         key.footer = "abs ws (km/h)",
         key.position = "bottom",
         par.settings = list(axis.line = list(col = "lightgray")),
         col = viridis(5, option = "D", direction = -1),
         main = paste(year_dep,
                      " - wind direction - ",
                      unique(arg_data_2$Vessel),
                      " - ",
                      weeks_lett[i],
                      " - n=",
                      dim(arg_data_2)[1],
                      sep = ""))
dev.off()

    }           
        })
