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
library(officer)

zon <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_north_stack.rds")
zon <- zon[[order(names(zon))]]

mer <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_east_stack.rds")
mer <- mer[[order(names(mer))]]

speed <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Pre_treat/wind_speed_stack.rds")
speed <- speed[[order(names(speed))]]

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
div_2017_568 <- argos_2017_sp[argos_2017_sp$Vessel == "166568", ] # the longest

# -----> Atypical tracks
########################
div_2017_569 <- argos_2017_sp[argos_2017_sp$Vessel == "166569", ]
div_2017_572 <- argos_2017_sp[argos_2017_sp$Vessel == "166572", ] # the longest

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
#############################

# <----- Device 166568 ----->
div_2017_568_crop1 <- div_2017_568[div_2017_568$point.group <= 19, ]
dim(div_2017_568_crop1)
summary(div_2017_568_crop1$Date)
# <----- Device 166566 ----->
div_2017_566_crop1 <- div_2017_566
summary(div_2017_566_crop1$Date)
# ==> min date = 2017-04-06
# ==> max date = 2017-05-09

# -----> Temporal selection of layers - CROP 1
###############################################
# Number of first layer
raster_min_2017_crop1 <- 1
# Number of last layer based on the maximal date - 2017-05-09
raster_max_2017_crop1 <- str_which(names(zon),
                                   "2017.05.09.18.00")
# Selection of layers
zon_1 <- zon[[raster_min_2017_crop1:raster_max_2017_crop1]]
mer_1 <- mer[[raster_min_2017_crop1:raster_max_2017_crop1]]
spe_1 <- speed[[raster_min_2017_crop1:raster_max_2017_crop1]]

# -----> Spatial selection of layers - CROP 1
##############################################
ext_2017_crop1 <- extent(40, 60, -30, -10) # xmin, xmax, ymin, ymax
z_2017_crop1 <- crop(x = zon_1,
                     y = ext_2017_crop1)
m_2017_crop1 <- crop(x = mer_1,
                     y = ext_2017_crop1)
s_2017_crop1 <- crop(x = spe_1,
                     y = ext_2017_crop1)

# -----> CROP 2 ####
####################
# From 10 May 2017 to the end
div_2017_568_crop2 <- div_2017_568[div_2017_568$point.group >= 20, ]
dim(div_2017_568_crop2)
summary(div_2017_568_crop2$Date)

# ==> min date = 2017-05-09 16:57:00
# ==> max date = 2017-05-22 16:00:00

# -----> Temporal selection of layers - CROP 2
##############################################
# Number of first layer based on the minimal date - 2017-05-09
raster_min_2017_crop2 <- str_which(names(zon),
                                   "2017.05.09.00.00")
# Number of last layer based on the maximal date - 2017-05-22
raster_max_2017_crop2 <- str_which(names(zon),
                                   "2017.05.22.18.00")
# Selection of layers
zon_2 <- zon[[raster_min_2017_crop2:raster_max_2017_crop2]]
mer_2 <- mer[[raster_min_2017_crop2:raster_max_2017_crop2]]
spe_2 <- speed[[raster_min_2017_crop2:raster_max_2017_crop2]]

# -----> Spatial selection of layers - CROP 2
##############################################
ext_2017_crop2 <- extent(35, 55, -15, 5)
z_2017_crop2 <- crop(x = zon_2,
                     y = ext_2017_crop2)
m_2017_crop2 <- crop(x = mer_2,
                     y = ext_2017_crop2)
s_2017_crop2 <- crop(x = spe_2,
                     y = ext_2017_crop2)

# -----> 2017 - 166568 - crop 1 ####
####################################
for (i in 1:nlayers(z_2017_crop1)) {
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166568/crop1/",
                "CROP1-2017-166568-",
                names(z_2017_crop1[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    dat <- substr(names(z_2017_crop1[[i]]), 2, 17)
    locs568 <- div_2017_568_crop1[div_2017_568_crop1$raster_layer == dat, ]
    dat_conv <- as.POSIXlt(dat,
                           format = "%Y.%m.%d.%H.%M")
    back_locs_568 <- div_2017_568_crop1[div_2017_568_crop1$Date < dat_conv, ]

    # x11()
    print(
    vectorplot(stack(m_2017_crop1[[i]], z_2017_crop1[[i]]),
                  isField = 'dXY',
                  region =  s_2017_crop1[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2017 - TYPICAL - CROP 1 - ",
                               names(m_2017_crop1[[i]]))) +
    layer(c(sp.points(back_locs_568,
                      col = "#d7f3b7",
                      lwd = 3,
                      cex = 2),
            sp.points(locs568,
                      col = "#4a8a02",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
print(paste(i, "/", nlayers(z_2017_crop1), sep = ""))
}

# -----> 2017 - 166568 - crop 2 ####
####################################
for (i in 1:nlayers(z_2017_crop2)) {
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166568/crop2/",
                "CROP2-2017-166568-",
                names(z_2017_crop2[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    dat <- substr(names(z_2017_crop2[[i]]), 2, 17)
    locs568 <- div_2017_568_crop2[div_2017_568_crop2$raster_layer == dat, ]
    dat_conv <- as.POSIXlt(dat,
                           format = "%Y.%m.%d.%H.%M")
    back_locs_568 <- div_2017_568_crop2[div_2017_568_crop2$Date < dat_conv, ]

    # x11()
    print(
    vectorplot(stack(m_2017_crop2[[i]], z_2017_crop2[[i]]),
                  isField = 'dXY',
                  region =  s_2017_crop2[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2017 - TYPICAL - CROP 2 - ",
                               names(m_2017_crop2[[i]]))) +
    layer(c(sp.points(back_locs_568,
                      col = "#caf599",
                      lwd = 3,
                      cex = 2),
            sp.points(locs568,
                      col = "#5aa505",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
print(paste(i, "/", nlayers(z_2017_crop2), sep = ""))
}

# -----> 2017 - 166566 - crop 1 ####
####################################

for (i in 1:nlayers(z_2017_crop1)) {
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166566/crop1/",
                "CROP1-2017-166566-",
                names(z_2017_crop1[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    dat <- substr(names(z_2017_crop1[[i]]), 2, 17)
    locs566 <- div_2017_566_crop1[div_2017_566_crop1$raster_layer == dat, ]
    dat_conv <- as.POSIXlt(dat,
                           format = "%Y.%m.%d.%H.%M")
    back_locs_566 <- div_2017_566_crop1[div_2017_566_crop1$Date < dat_conv, ]
    # x11()
    print(
    vectorplot(stack(m_2017_crop1[[i]], z_2017_crop1[[i]]),
                  isField = 'dXY',
                  region =  s_2017_crop1[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2017 - TYPICAL - CROP 1 - ",
                               names(m_2017_crop1[[i]]))) +
    layer(c(sp.points(back_locs_566,
                      col = "#aec1f5",
                      lwd = 3,
                      cex = 2),
            sp.points(locs566,
                      col = "#0042f8",
                      lwd = 6,
                      cex = 4)))
# )
    )

dev.off()
print(paste(i, "/", nlayers(z_2017_crop1), sep = ""))
}

# -----> PPT creation ####
##########################

# File list
files_2017_568 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166568/")
length(files_2017_568)

files_2017_566 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166566/")
length(files_2017_566)

# Doc creation
doc <- read_pptx()
doc <- add_slide(doc,
                 layout = "Two Content",
                 master = "Office Theme")

for (i in 1:length(files_2017_568)) {
    
    # file path
    img_file_568 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166568/",
                          files_2017_568[i],
                          sep = "")
    if (!is.na(files_2017_566[i])) {
        img_file_566 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166566/",
                              files_2017_566[i],
                              sep = "")
        doc <- ph_with(x = doc,
                       value = paste("Typical tracks in 2017 - ",
                       substr(files_2017_568[i],
                                      20,
                                      35),
                       sep = ""),
                       location = ph_location_type(type = "title"))
        doc <- ph_with(x = doc,
                       value = "166568",
                       location = ph_location_left())
        doc <- ph_with(x = doc,
                       value = "166566",
                       location = ph_location_right())
        doc <- ph_with(x = doc,
                       value = external_img(img_file_568),
                       location = ph_location_left(),
                       use_loc_size = TRUE)
        doc <- ph_with(x = doc,
                       value = external_img(img_file_566),
                       location = ph_location_right(),
                       use_loc_size = TRUE)
        doc <- add_slide(doc)
    } else {
                doc <- ph_with(x = doc,
                       value = substr(files_2017_568[i],
                                      20,
                                      35),
                       location = ph_location_type(type = "title"))
        doc <- ph_with(x = doc,
                       value = "166568",
                       location = ph_location_left())
        doc <- ph_with(x = doc,
                       value = external_img(img_file_568),
                       location = ph_location_left(),
                       use_loc_size = TRUE)
        doc <- add_slide(doc)
    }
    print(i)
}

print(doc,
      target = "C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/2017_TYPICAL_TRACKS.pptx")
