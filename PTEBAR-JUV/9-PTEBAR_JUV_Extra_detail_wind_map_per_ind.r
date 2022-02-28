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
library(mapview)

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

# -----> Explo 2017 ####
########################

argos_2017_sp <- argos_sp[year(argos_sp$deploy) == 2017, ]
dim(argos_2017_sp)

# -----> Typical tracks
#######################
div_2017_566 <- argos_2017_sp[argos_2017_sp$Vessel == "166566", ]
div_2017_568 <- argos_2017_sp[argos_2017_sp$Vessel == "166568", ]

# -----> Atypical tracks
########################
div_2017_569 <- argos_2017_sp[argos_2017_sp$Vessel == "166569", ]
div_2017_572 <- argos_2017_sp[argos_2017_sp$Vessel == "166572", ]

mapview(div_2017_566, col.regions = "#0863eb") +
    mapview(div_2017_568, col.regions = "#0b728b") +
        mapview(div_2017_569, col.regions = "#fa8807") +
            mapview(div_2017_572, col.regions = "#d60dab")

# -----> Extend organization ####
#################################
# -----> CROP 1 ####
####################
# From 1 April 2017 to 9 May 2017

# -----> Subset of argos data

# <----- Device 166568 ----->
div_2017_568_crop1 <- div_2017_568[div_2017_568$point.group <= 19, ]
dim(div_2017_568_crop1)
summary(div_2017_568_crop1$Date)
# <----- Device 166566 ----->
div_2017_566_crop1 <- div_2017_566
summary(div_2017_566_crop1$Date)

# -----> Subset of raster layers - CROP 1
raster_min_2017_crop1 <- 1
# ----- #
div_2017_566_crop1$raster_layer
layer <- paste(substr(div_2017_566_crop1$raster_layer[93], 1, 11),
               "18.00",
               sep = "") # Pour avoir les toutes les couches de lma dernière date
raster_max_2017_crop1 <- str_which(names(zon),
                                   layer)


zon_1 <- zon[[raster_min_2017_crop1:raster_max_2017_crop1]]
mer_1 <- mer[[raster_min_2017_crop1:raster_max_2017_crop1]]
spe_1 <- speed[[raster_min_2017_crop1:raster_max_2017_crop1]]

# -----> Cropping of raster layers - CROP 1
ext_2017_1 <- extent(40, 70, -35, 0)
z_2017_1 <- crop(x = zon_1,
                 y = ext_2017_1)
m_2017_1 <- crop(x = mer_1,
                 y = ext_2017_1)
s_2017_1 <- crop(x = spe_1,
                 y = ext_2017_1)

# -----> CROP 2 ####
####################
# From 10 May to the end
div_2017_568_crop2 <- div_2017_568[div_2017_568$point.group > 19, ]
dim(div_2017_568_crop2)
summary(div_2017_568_crop2$Date)

# -----> Subset of raster layers - CROP 2
raster_min_2017_crop1 <- raster_max_2017_crop1 + 1
raster_max_2017_crop1 <- nlayers(z_2017_1)


# zon_1 <- zon[[str_which(names(zon), raster_min_c1):
#               str_which(names(zon), raster_max_c1)]]
# mer_1 <- mer[[str_which(names(mer), raster_min_c1):
#               str_which(names(mer), raster_max_c1)]]
# spe_1 <- speed[[str_which(names(speed), raster_min_c1):
#               str_which(names(speed), raster_max_c1)]]

# # -----> Cropping of raster layers - CROP 1
# ext_2017_1 <- extent(40, 70, -35, 0)
# z_2017_1 <- crop(x = zon_1,
#                  y = ext_2017_1)
# m_2017_1 <- crop(x = mer_1,
#                  y = ext_2017_1)
# s_2017_1 <- crop(x = spe_1,
#                  y = ext_2017_1)

# -----> 2017 - 166568 - crop 1 ####
####################################
for(i in 1:nlayers(z_2017_1)){
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166568/crop1/",
                "2017-166568-",
                names(new_z_1[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    dat <- substr(names(z_2017_1[[i]]), 2, 17)
    locs568 <- div_2017_568_crop1[div_2017_568_crop1$raster_layer == dat,]
    back_locs_568 <- div_2017_568_crop1[div_2017_568_crop1$Date < as.POSIXlt(dat,
                                                                             format = "%Y.%m.%d"),]

    # x11()
    print(
    vectorplot(stack(m_2017_1[[i]], z_2017_1[[i]]),
                  isField = 'dXY',
                  region =  s_2017_1[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2017 - TYPICAL - CROP 1 - ",
                               names(m_2017_1[[i]]))) +
    layer(c(sp.points(back_locs_568,
                      col = "#d7f3b7",
                      lwd = 3,
                      cex = 2),
            sp.points(locs568,
                      col = "#4a8a02",
                      lwd = 3,
                      cex = 2,
                      cex.lab = 2)))
    )

dev.off()
print(i)
}

# -----> 2017 - 166566 - crop 1 ####
####################################

for(i in 1:nlayers(z_2017_1)){
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166566/crop1/",
                "2017-166566-",
                names(new_z_1[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    dat <- substr(names(z_2017_1[[i]]), 2, 17)
    locs566 <- div_2017_566_crop1[div_2017_566_crop1$raster_layer == dat,]
    back_locs_566 <- div_2017_566_crop1[div_2017_566_crop1$Date < as.POSIXlt(dat,
                                                                             format = "%Y.%m.%d"),]
    # x11()
    print(
    vectorplot(stack(m_2017_1[[i]], z_2017_1[[i]]),
                  isField = 'dXY',
                  region =  s_2017_1[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2017 - TYPICAL - CROP 1 - ",
                               names(m_2017_1[[i]]))) +
    layer(c(sp.points(back_locs_566,
                      col = "#aec1f5",
                      lwd = 3,
                      cex = 2),
            sp.points(locs566,
                      col = "#0042f8",
                      lwd = 3,
                      cex = 2)))
# )
    )

dev.off()
print(i)
}
