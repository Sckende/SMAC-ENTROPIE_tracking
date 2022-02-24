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
library(magick)
library(latticeExtra)

zon <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_north_stack.rds")
mer <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_east_stack.rds")

speed <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_speed_stack.rds")

argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
argos_sp <- SpatialPointsDataFrame(coords = argos[, c("Longitude", "Latitude")],
                                  data = argos,
                                  proj4string = CRS("+init=epsg:4326"))
# -----> Global map ####
########################
# --- Options --- #
nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev + 1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       alpha = 0.9,
                       option = "A")(nlev)

# --- Map --- #
x11()
 vectorplot(stack(mer[[1]], zon[[1]]),
                  isField = "dXY",
                  region =  speed[[1]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = names(zon[[1]])) +
    layer(sp.points(argos_sp[year(argos_sp$deploy) == 2017,], col = "grey", lwd = 1, cex = 1))

# ----- > Test on 166566 #####
###############################

d_166566 <- argos_sp[argos_sp$Vessel == "166566",]
dim(d_166566)
summary(d_166566$Latitude)
summary(d_166566$Longitude)

# CROP 1 #
##########

crop1 <- d_166566[d_166566$point.group %in% 1:6,]
# ----- #
raster_min_c1 <- d_166566$raster_layer[1]
raster_max_c1 <- d_166566$raster_layer[length(d_166566$raster_layer)]
# ----- #
x_min_c1 <- min(crop1$Longitude) - 10
x_max_c1 <- max(crop1$Longitude) + 10
y_min_c1 <- min(crop1$Latitude) - 10
y_max_c1 <- max(crop1$Latitude) + 10
# ----- #
# Layer subset of rasters
zon_1 <- zon[[str_which(names(zon), raster_min_c1):
              str_which(names(zon), raster_max_c1)]]
mer_1 <- mer[[str_which(names(mer), raster_min_c1):
              str_which(names(mer), raster_max_c1)]]
spe_1 <- speed[[str_which(names(speed), raster_min_c1):
              str_which(names(speed), raster_max_c1)]]
# ----- #
# Crop the rasters
#extent(xmin, xmax, ymin, ymax)
new_ext_1 <- extent(x_min_c1, x_max_c1, y_min_c1, y_max_c1)
new_z_1 <- crop(x = zon_1,
                y = new_ext_1)
new_m_1 <- crop(x = mer_1,
                y = new_ext_1)
new_s_1 <- crop(x = spe_1,
                y = new_ext_1)
# ----- #
for(i in 1:nlayers(new_z_1)){
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166566/166566-",
              names(new_z_1[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    dat <- substr(names(new_z_1[[i]]), 2, 17)
    locs <- crop1[crop1$raster_layer == dat,]
    back_locs <- crop1[crop1$Date < as.POSIXlt(dat,
                                               format = "%Y.%m.%d"),]
    # x11()
    print(
    vectorplot(stack(new_m_1[[i]], new_z_1[[i]]),
                  isField = 'dXY',
                  region =  new_s_1[[i]],
                  at = my_at,
                  lwd.arrows = 0.5,
                  aspX = 0.1,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("166566 - CROP 1 - ",
                               names(new_m_1[[i]]))) +
    layer(c(sp.points(back_locs,
                      col = "grey",
                      lwd = 3,
                      cex = 2),
            sp.points(locs,
                      col = viridis(7)[2],
                      lwd = 3,
                      cex = 2,
                      cex.lab = 2)))
# )
    )

dev.off()
print(i)
}
# ----- #
library(officer)
files_166566 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166566/")
length(files_166566)

doc <- read_pptx()
doc <- add_slide(doc,
                 layout = "Two Content",
                 master = "Office Theme")

for(i in 1:length(files_166566)){
    
    # file path
    img_files_166566 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166566/",
                          files_166566[i],
                          sep = "")
        doc <- ph_with(x = doc,
                       value = substr(files_166566[i],
                                      8,
                                      23),
                       location = ph_location_type(type = "title"))
        doc <- ph_with(x = doc,
               external_img(img_files_166566),
               location = ph_location_left(),
               use_loc_size = TRUE)
        doc <- add_slide(doc)
    print(i)
    }

print(doc,
      target = "C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166566.pptx")

# CROP 2 #
##########
crop2 <- d_166566[d_166566$point.group %in% 7:15,]
x_min_c2 <- min(crop2$Longitude)
x_max_c2 <- max(crop2$Longitude)
y_min_c2 <- min(crop2$Latitude)
y_max_c2 <- max(crop2$Latitude)
# ----- #
new_ext_2 <- extent(x_min_c2, x_max_c2, y_min_c2, y_max_c2)
new_z_2 <- crop(x = zon,
                y = new_ext_2)
new_m_2 <- crop(x = mer,
                y = new_ext_2)
new_s_2 <- crop(x = speed,
                y = new_ext_2)

# CROP 3 #
crop3 <- d_166566[d_166566$point.group %in% 16:25,]
x_min_c3 <- min(crop3$Longitude)
x_max_c3 <- max(crop3$Longitude)
y_min_c3 <- min(crop3$Latitude)
y_max_c3 <- max(crop3$Latitude)
# ----- #
new_ext_3 <- extent(x_min_c3, x_max_c3, y_min_c3, y_max_c3)
new_z_3 <- crop(x = zon,
                y = new_ext_3)
new_m_3 <- crop(x = mer,
                y = new_ext_3)
new_s_3 <- crop(x = speed,
                y = new_ext_3)