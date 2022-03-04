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

# -----> TYPICAL TRACKS <----- ####
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
    # png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166568/crop1/",
    #             "CROP1-2017-166568-",
    #             names(z_2017_crop1[[i]]),
    #           ".png",
    #           sep = ""),
    # res = 300,
    # width = 40,
    # height = 50,
    # pointsize = 12,
    # unit = "cm",
    # bg = "transparent")

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
    # png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166568/crop2/",
    #             "CROP2-2017-166568-",
    #             names(z_2017_crop2[[i]]),
    #           ".png",
    #           sep = ""),
    # res = 300,
    # width = 40,
    # height = 50,
    # pointsize = 12,
    # unit = "cm",
    # bg = "transparent")

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
    # png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-166566/crop1/",
    #             "CROP1-2017-166566-",
    #             names(z_2017_crop1[[i]]),
    #           ".png",
    #           sep = ""),
    # res = 300,
    # width = 40,
    # height = 50,
    # pointsize = 12,
    # unit = "cm",
    # bg = "transparent")

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
files_2017_568 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-TYP-166568/")
length(files_2017_568)

files_2017_566 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-TYP-166566/")
length(files_2017_566)

# Doc creation
# doc <- read_pptx()
# doc <- add_slide(doc,
#                  layout = "Two Content",
#                  master = "Office Theme")

for (i in 1:length(files_2017_568)) {
    
    # file path
    img_file_568 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-TYP-166568/",
                          files_2017_568[i],
                          sep = "")
    if (!is.na(files_2017_566[i])) {
        img_file_566 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-TYP-166566/",
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
                       value = paste(i, "/", length(files_2017_568), sep = ""),
                       location = ph_location_type(type = "ftr"))
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
                       value = paste(i, "/", length(files_2017_568), sep = ""),
                       location = ph_location_type(type = "ftr"))
        doc <- ph_with(x = doc,
                       value = external_img(img_file_568),
                       location = ph_location_left(),
                       use_loc_size = TRUE)
        doc <- add_slide(doc)
    }
    print(i)
}

# print(doc,
#       target = "C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/2017_TYPICAL_TRACKS.pptx")

# -----> TYPICAL TRACKS <----- ####
# -----> Extend organization ####
#################################

# -----> Temporal selection of layers
######################################
summary(div_2017_572$Date)
# min date = 2017-04-06 11:45:00
# max date = 2017-09-15 14:53:00

summary(div_2017_569$Date)
# min date = 2017-04-06 14:13:00
# max date = 2017-06-13 10:32:00

# Number of first layer
raster_min_2017_atyp <- 1
# Number of last layer based on the maximal date - 2017-09-15 14:53:00
raster_max_2017_atyp <- str_which(names(zon),
                                   "2017.09.15.18.00")

# Selection of layers
zon_3 <- zon[[raster_min_2017_atyp:raster_max_2017_atyp]]
mer_3 <- mer[[raster_min_2017_atyp:raster_max_2017_atyp]]
spe_3 <- speed[[raster_min_2017_atyp:raster_max_2017_atyp]]

# -----> Spatial selection of layers
####################################
ext_2017_atyp <- extent(45, 75, -25, 5) # xmin, xmax, ymin, ymax
z_2017_atyp <- crop(x = zon_3,
                    y = ext_2017_atyp)
m_2017_atyp <- crop(x = mer_3,
                    y = ext_2017_atyp)
s_2017_atyp <- crop(x = spe_3,
                    y = ext_2017_atyp)

# -----> 2017 - ATYPICAL - MAPS PRODUCTION ####
##############################################

for (i in 1:nlayers(z_2017_atyp)) {
    
    dat <- substr(names(z_2017_atyp[[i]]), 2, 17)
    dat_conv <- as.POSIXlt(dat,
                           format = "%Y.%m.%d.%H.%M")
    # ----- #
    locs_572 <- div_2017_572[div_2017_572$raster_layer == dat, ]
    back_locs_572 <- div_2017_572[div_2017_572$Date < dat_conv, ]
    # ----- #
    locs_569 <- div_2017_569[div_2017_569$raster_layer == dat, ]
    back_locs_569 <- div_2017_569[div_2017_569$Date < dat_conv, ]
    # ----- #
    # png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-ATYP-166572/",
    #             "2017-ATYP-166572-",
    #             names(z_2017_atyp[[i]]),
    #           ".png",
    #           sep = ""),
    # res = 300,
    # width = 40,
    # height = 50,
    # pointsize = 12,
    # unit = "cm",
    # bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2017_atyp[[i]], z_2017_atyp[[i]]),
                  isField = 'dXY',
                  region =  s_2017_atyp[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2017 - ATYP - 166572 - ",
                               names(m_2017_atyp[[i]]))) +
    layer(c(sp.points(back_locs_572,
                      col = "#caf599",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_572,
                      col = "#5aa505",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
print(paste("166572 - ", i, "/", nlayers(z_2017_atyp), sep = ""))
# ----- #
if (dat_conv <= max(div_2017_569$Date)) {
    # ----- #
    # png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-ATYP-166569/",
    #             "2017-ATYP-166569-",
    #             names(z_2017_atyp[[i]]),
    #           ".png",
    #           sep = ""),
    # res = 300,
    # width = 40,
    # height = 50,
    # pointsize = 12,
    # unit = "cm",
    # bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2017_atyp[[i]], z_2017_atyp[[i]]),
                  isField = 'dXY',
                  region =  s_2017_atyp[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2017 - ATYP - 166569 -",
                               names(m_2017_atyp[[i]]))) +
    layer(c(sp.points(back_locs_569,
                      col = "#aec1f5",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_569,
                      col = "#0042f8",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
    }
}

# -----> PPT creation ####
##########################

# File list
files_2017_572 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-ATYP-166572/")
length(files_2017_572)

files_2017_569 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-ATYP-166569/")
length(files_2017_569)

# Doc creation
# doc <- read_pptx()
# doc <- add_slide(doc,
#                  layout = "Two Content",
#                  master = "Office Theme")

for (i in 1:length(files_2017_572)) {
    
    # file path
    img_file_572 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-ATYP-166572/",
                          files_2017_572[i],
                          sep = "")
    if (!is.na(files_2017_569[i])) {
        img_file_569 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2017-ATYP-166569/",
                              files_2017_569[i],
                              sep = "")
        doc <- ph_with(x = doc,
                       value = paste("Atypical tracks in 2017 - ",
                       substr(files_2017_572[i],
                                      20,
                                      35),
                       sep = ""),
                       location = ph_location_type(type = "title"))
        doc <- ph_with(x = doc,
                       value = "166572",
                       location = ph_location_left())
        doc <- ph_with(x = doc,
                       value = "166569",
                       location = ph_location_right())
        doc <- ph_with(x = doc,
                       value = paste(i, "/", length(files_2017_572), sep = ""),
                       location = ph_location_type(type = "ftr"))
        doc <- ph_with(x = doc,
                       value = external_img(img_file_572),
                       location = ph_location_left(),
                       use_loc_size = TRUE)
        doc <- ph_with(x = doc,
                       value = external_img(img_file_569),
                       location = ph_location_right(),
                       use_loc_size = TRUE)
        doc <- add_slide(doc)
    } else {
                doc <- ph_with(x = doc,
                       value = substr(files_2017_572[i],
                                      20,
                                      35),
                       location = ph_location_type(type = "title"))
        doc <- ph_with(x = doc,
                       value = "166572",
                       location = ph_location_left())
        doc <- ph_with(x = doc,
                       value = paste(i, "/", length(files_2017_572), sep = ""),
                       location = ph_location_type(type = "ftr"))
        doc <- ph_with(x = doc,
                       value = external_img(img_file_572),
                       location = ph_location_left(),
                       use_loc_size = TRUE)
        doc <- add_slide(doc)
    }
    print(i)
}

# print(doc,
#       target = "C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/2017_ATYPICAL_TRACKS.pptx")

# -----> Explo 2018 ####
########################
zon_2018 <- dropLayer(zon,
                      str_which(names(zon),
                                "X2017"))
mer_2018 <- dropLayer(mer,
                      str_which(names(mer),
                                "X2017"))
speed_2018 <- dropLayer(speed,
                        str_which(names(speed),
                                  "X2017"))

argos_2018_sp <- argos_sp[year(argos_sp$deploy) == 2018, ]
dim(argos_2018_sp)
argos_2018_sp$Vessel <- as.character(argos_2018_sp$Vessel)

# -----> Typical tracks
#######################
argos_2018_typ <- argos_2018_sp[argos_2018_sp$Vessel %in% c("162070",
                                                            "166563",
                                                            "162072",
                                                            "162073",
                                                            "166561"), ]
dim(argos_2018_typ)
table(argos_2018_typ$Vessel)
mapview(argos_2018_typ,
        zcol = "Vessel",
        burst = T)

# -----> Atypical tracks
########################
argos_2018_atyp <- argos_2018_sp[argos_2018_sp$Vessel %in% c("166564",
                                                            "166565"), ]
dim(argos_2018_atyp)
table(argos_2018_atyp$Vessel)
x11()
mapview(argos_2018_atyp,
        zcol = "Vessel",
        burst = F)
# Global
mapview(argos_2018_typ, col.regions = "#0863eb") +
        mapview(argos_2018_atyp, col.regions = "#fa8807")

# -----> ATYPICAL TRACKS <----- ####
####################################
# Crops are based on temporal scale
# -----> Extend organization ####
#################################
# -----> CROP 1 ####
####################
# From 1 April 2018 to 19 May 2018

# -----> Subset of argos data
#############################

# <----- Device 166564 & 166565 ----->
max_date <- max(argos_2018_atyp$Date[argos_2018_atyp$Vessel == "166564"])
argos_2018_atyp_crop1 <- argos_2018_atyp[argos_2018_atyp$Date <= max_date, ]
summary(argos_2018_atyp_crop1$Date)
# ==> min date = 2018-04-04 12:00:00
# ==> max date = 2018-05-19 17:28:00

# -----> Temporal selection of layers - CROP 1
###############################################
# Number of first layer
raster_min_20178_crop1 <- 1
# Number of last layer based on the maximal date - 2017-05-19
raster_max_2018_crop1 <- str_which(names(zon_2018),
                                   "2018.05.19.18.00")
# Selection of layers
zon_1 <- zon_2018[[raster_min_20178_crop1:raster_max_2018_crop1]]
mer_1 <- mer_2018[[raster_min_20178_crop1:raster_max_2018_crop1]]
speed_1 <- speed_2018[[raster_min_20178_crop1:raster_max_2018_crop1]]

# -----> Spatial selection of layers - CROP 1
##############################################
ext_2018_crop1 <- extent(40, 80, -40, 0) # xmin, xmax, ymin, ymax
z_2018_atyp_crop1 <- crop(x = zon_1,
                           y = ext_2018_crop1)
m_2018_atyp_crop1 <- crop(x = mer_1,
                          y = ext_2018_crop1)
s_2018_atyp_crop1 <- crop(x = speed_1,
                          y = ext_2018_crop1)

# -----> CROP 2 ####
####################
# From 20 May 2018 to 24 November 2018

# -----> Subset of argos data
#############################
mapview(argos_2018_atyp[argos_2018_atyp$Vessel == "166565" & date(argos_2018_atyp$Date) >= as.POSIXlt("2018-05-20",
                           format = "%Y-%m-%d"), ])
mapview(argos_2018_atyp[argos_2018_atyp$Vessel == "166565" & date(argos_2018_atyp$Date) < as.POSIXlt("2018-05-20",
                           format = "%Y-%m-%d"), ])

# <----- Device 166565 ----->
max_date_C1 <- max(argos_2018_atyp_crop1$Date)
argos_2018_atyp_crop2 <- argos_2018_atyp[date(argos_2018_atyp$Date) >= as.POSIXlt("2018-05-20", format = "%Y-%m-%d"), ]
summary(argos_2018_atyp_crop2$Date)
# ==> min date = 2018-05-20 00:30:00
# ==> max date = 2018-11-24 04:42:00

# -----> Temporal selection of layers - CROP 2
###############################################
# Number of first layer - 2018-05-20 00:00:00
raster_min_20178_crop2 <- str_which(names(zon_2018),
                                   "2018.05.20.00.00")
# Number of last layer based on the maximal date - 2018-11-24 04:42:00
raster_max_2018_crop2 <- str_which(names(zon_2018),
                                   "2018.11.24.18.00")
# Selection of layers
zon_2 <- zon_2018[[raster_min_20178_crop2:raster_max_2018_crop2]]
mer_2 <- mer_2018[[raster_min_20178_crop2:raster_max_2018_crop2]]
speed_2 <- speed_2018[[raster_min_20178_crop2:raster_max_2018_crop2]]

# -----> Spatial selection of layers - CROP 2
##############################################
ext_2018_crop2 <- extent(65, 105, -40, 0) # xmin, xmax, ymin, ymax
z_2018_atyp_crop2 <- crop(x = zon_2,
                          y = ext_2018_crop2)
m_2018_atyp_crop2 <- crop(x = mer_2,
                          y = ext_2018_crop2)
s_2018_atyp_crop2 <- crop(x = speed_2,
                          y = ext_2018_crop2)

# -----> 2018 - ATYPICAL - CROP 1 MAPS PRODUCTION ####
#######################################################
for (i in 1:nlayers(z_2018_atyp_crop1)) {
    
    dat <- substr(names(z_2018_atyp_crop1[[i]]), 2, 17)
    dat_conv <- as.POSIXlt(dat,
                           format = "%Y.%m.%d.%H.%M")
    # ----- #
    locs_565 <- argos_2018_atyp_crop1[argos_2018_atyp_crop1$Vessel == "166565"
                                      & argos_2018_atyp_crop1$raster_layer == dat, ]
    back_locs_565 <- argos_2018_atyp_crop1[argos_2018_atyp_crop1$Vessel == "166565"
                                           & argos_2018_atyp_crop1$Date < dat_conv, ]
    # ----- #
    locs_564 <- argos_2018_atyp_crop1[argos_2018_atyp_crop1$Vessel == "166564"
                                      & argos_2018_atyp_crop1$raster_layer == dat, ]
    back_locs_564 <- argos_2018_atyp_crop1[argos_2018_atyp_crop1$Vessel == "166564"
                                      & argos_2018_atyp_crop1$Date < dat_conv, ]
    # ----- 166565 ----- #
    # png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-ATYP-166565/",
    #             "2018-ATYP-166565-CROP1-",
    #             names(z_2018_atyp_crop1[[i]]),
    #           ".png",
    #           sep = ""),
    # res = 300,
    # width = 40,
    # height = 50,
    # pointsize = 12,
    # unit = "cm",
    # bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2018_atyp_crop1[[i]], z_2018_atyp_crop1[[i]]),
                  isField = 'dXY',
                  region =  s_2018_atyp_crop1[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2018 - ATYP - 166565 - ",
                               names(m_2018_atyp_crop1[[i]]))) +
    layer(c(sp.points(back_locs_565,
                      col = "#caf599",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_565,
                      col = "#5aa505",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
print(paste("166565 - ", i, "/", nlayers(z_2018_atyp_crop1), sep = ""))
# ----- 166564 ----- #
    # png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-ATYP-166564/",
    #             "2018-ATYP-166564-CROP1-",
    #             names(z_2018_atyp_crop1[[i]]),
    #           ".png",
    #           sep = ""),
    # res = 300,
    # width = 40,
    # height = 50,
    # pointsize = 12,
    # unit = "cm",
    # bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2018_atyp_crop1[[i]], z_2018_atyp_crop1[[i]]),
                  isField = 'dXY',
                  region =  s_2018_atyp_crop1[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2018 - ATYP - 166564 -",
                               names(m_2018_atyp_crop1[[i]]))) +
    layer(c(sp.points(back_locs_564,
                      col = "#aec1f5",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_564,
                      col = "#0042f8",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
}

# -----> 2018 - ATYPICAL - CROP 2 MAPS PRODUCTION ####
#######################################################
for (i in 1:nlayers(z_2018_atyp_crop2)) {
    
    dat <- substr(names(z_2018_atyp_crop2[[i]]), 2, 17)
    dat_conv <- as.POSIXlt(dat,
                           format = "%Y.%m.%d.%H.%M")
    # ----- #
    locs_565 <- argos_2018_atyp_crop2[argos_2018_atyp_crop2$raster_layer == dat, ]
    back_locs_565 <- argos_2018_atyp_crop2[argos_2018_atyp_crop2$Date < dat_conv, ]
    # ----- 166565 ----- #
    # png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-ATYP-166565/",
    #             "2018-ATYP-166565-CROP2-",
    #             names(z_2018_atyp_crop2[[i]]),
    #           ".png",
    #           sep = ""),
    # res = 300,
    # width = 40,
    # height = 50,
    # pointsize = 12,
    # unit = "cm",
    # bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2018_atyp_crop2[[i]], z_2018_atyp_crop2[[i]]),
                  isField = 'dXY',
                  region =  s_2018_atyp_crop2[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2018 - ATYP - 166565 - ",
                               names(m_2018_atyp_crop2[[i]]))) +
    layer(c(sp.points(back_locs_565,
                      col = "#caf599",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_565,
                      col = "#5aa505",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
print(paste("166565 - ", i, "/", nlayers(z_2018_atyp_crop2), sep = ""))

}

# -----> PPT creation ####
##########################

# File list
files_2018_564 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-ATYP-166564/")
length(files_2018_564) # the shortest

files_2018_565 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-ATYP-166565/")
length(files_2018_565)

# Doc creation
doc <- read_pptx()
doc <- add_slide(doc,
                 layout = "Two Content",
                 master = "Office Theme")

for (i in 1:length(files_2018_565)) {############################ !!!!!!!!!!!!!!!!!!!!!
    
    # file path
    img_file_565 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-ATYP-166565/",
                          files_2018_565[i],
                          sep = "")
    if (!is.na(files_2018_564[i])) {
        img_file_564 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-ATYP-166564/",
                              files_2018_564[i],
                              sep = "")
        doc <- ph_with(x = doc,
                       value = paste("Atypical tracks in 2018 - ",
                       substr(files_2018_565[i],
                                      25,
                                      40),
                       sep = ""),
                       location = ph_location_type(type = "title"))
        doc <- ph_with(x = doc,
                       value = "166565",
                       location = ph_location_left())
        doc <- ph_with(x = doc,
                       value = "166564",
                       location = ph_location_right())
        doc <- ph_with(x = doc,
                       value = paste(i, "/", length(files_2018_565), sep = ""),
                       location = ph_location_type(type = "ftr"))
        doc <- ph_with(x = doc,
                       value = external_img(img_file_565),
                       location = ph_location_left(),
                       use_loc_size = TRUE)
        doc <- ph_with(x = doc,
                       value = external_img(img_file_564),
                       location = ph_location_right(),
                       use_loc_size = TRUE)
        doc <- add_slide(doc)
    } else {
                doc <- ph_with(x = doc,
                       value = substr(files_2018_565[i],
                                      25,
                                      40),
                       location = ph_location_type(type = "title"))
        doc <- ph_with(x = doc,
                       value = "166565",
                       location = ph_location_left())
        doc <- ph_with(x = doc,
                       value = paste(i, "/", length(files_2018_565), sep = ""),
                       location = ph_location_type(type = "ftr"))
        doc <- ph_with(x = doc,
                       value = external_img(img_file_565),
                       location = ph_location_left(),
                       use_loc_size = TRUE)
        doc <- add_slide(doc)
    }
    print(i)
}

# print(doc,
#       target = "C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/2018_ATYPICAL_TRACKS.pptx")

# -----> NORTHERN TYPICAL TRACKS <----- ####
############################################
# 2 Devices : 162070 & 166563
 
# Crops are based on temporal scale
# -----> Extend organization ####
#################################
# -----> CROP 1 ####
####################

# Time range : 1 avril 2018 to 5 mai 2018
# -----> Subset of argos data
#############################
dim(argos_2018_typ)
argos_2018_typ_N <- argos_2018_typ[argos_2018_typ$Vessel %in% c("162070", "166563"), ]
dim(argos_2018_typ_N)
table(argos_2018_typ_N$Vessel)

mapview(argos_2018_typ_N, zcol = "Vessel")

# <----- Device 162070 & 166563 ----->
max_date <- as.POSIXlt("2018-05-05", format = "%Y-%m-%d")
argos_2018_typ_N_crop1 <- argos_2018_typ_N[date(argos_2018_typ_N$Date) <= max_date, ]
summary(argos_2018_typ_N_crop1$Date)
# ==> min date = 2018-04-04 12:00:00
# ==> max date = 2018-05-05 04:47:00

# -----> Temporal selection of layers - CROP 1
###############################################
# Number of first layer
raster_min_2018_crop1 <- 1
# Number of last layer based on the maximal date - 2017-05-19
raster_max_2018_crop1 <- str_which(names(zon_2018),
                                   "2018.05.05.18.00")
# Selection of layers
zon_1 <- zon_2018[[raster_min_2018_crop1:raster_max_2018_crop1]]
mer_1 <- mer_2018[[raster_min_2018_crop1:raster_max_2018_crop1]]
speed_1 <- speed_2018[[raster_min_2018_crop1:raster_max_2018_crop1]]

# -----> Spatial selection of layers - CROP 1
##############################################
ext_2018_crop1 <- extent(40, 70, -40, -5) # xmin, xmax, ymin, ymax
z_2018_typ_N_crop1 <- crop(x = zon_1,
                           y = ext_2018_crop1)
m_2018_typ_N_crop1 <- crop(x = mer_1,
                           y = ext_2018_crop1)
s_2018_typ_N_crop1 <- crop(x = speed_1,
                           y = ext_2018_crop1)

# -----> 2018 - NORTHERN TYPICAL - CROP 1 MAPS PRODUCTION ####
##############################################################
for (i in 1:nlayers(z_2018_typ_N_crop1)) {
    
    dat <- substr(names(z_2018_typ_N_crop1[[i]]), 2, 17)
    dat_conv <- as.POSIXlt(dat,
                           format = "%Y.%m.%d.%H.%M")
    # ----- #
    locs_070 <- argos_2018_typ_N_crop1[argos_2018_typ_N_crop1$Vessel == "162070"
                                      & argos_2018_typ_N_crop1$raster_layer == dat, ]
    back_locs_070 <- argos_2018_typ_N_crop1[argos_2018_typ_N_crop1$Vessel == "162070"
                                           & argos_2018_typ_N_crop1$Date < dat_conv, ]
    # ----- #
    locs_563 <- argos_2018_typ_N_crop1[argos_2018_typ_N_crop1$Vessel == "166563"
                                      & argos_2018_typ_N_crop1$raster_layer == dat, ]
    back_locs_563 <- argos_2018_typ_N_crop1[argos_2018_typ_N_crop1$Vessel == "166563"
                                      & argos_2018_typ_N_crop1$Date < dat_conv, ]
    # ----- 162070 ----- #
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-162070/",
                "2018-TYP-N-162070-CROP1-",
                names(z_2018_typ_N_crop1[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2018_typ_N_crop1[[i]], z_2018_typ_N_crop1[[i]]),
                  isField = 'dXY',
                  region =  s_2018_typ_N_crop1[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2018 - TYP N - 162070 - ",
                               names(m_2018_typ_N_crop1[[i]]))) +
    layer(c(sp.points(back_locs_070,
                      col = "#caf599",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_070,
                      col = "#5aa505",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
print(paste("162070 - ", i, "/", nlayers(z_2018_typ_N_crop1), sep = ""))
# ----- 166563 ----- #
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-166563/",
                "2018-TYP-N-166563-CROP1-",
                names(z_2018_typ_N_crop1[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2018_typ_N_crop1[[i]], z_2018_typ_N_crop1[[i]]),
                  isField = 'dXY',
                  region =  s_2018_typ_N_crop1[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2018 - TYP N - 166563 -",
                               names(m_2018_typ_N_crop1[[i]]))) +
    layer(c(sp.points(back_locs_563,
                      col = "#aec1f5",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_563,
                      col = "#0042f8",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
}

# -----> CROP 2 ####
####################

# Time range : 6 mai 2018 to 27 juin 2018
# -----> Subset of argos data
#############################
# <----- Device 162070 & 166563 ----->
min_date <- as.POSIXlt("2018-05-06", format = "%Y-%m-%d")
max_date <- as.POSIXlt("2018-06-27", format = "%Y-%m-%d")
argos_2018_typ_N_crop2 <- argos_2018_typ_N[date(argos_2018_typ_N$Date) <= max_date & date(argos_2018_typ_N$Date) >= min_date, ]
summary(argos_2018_typ_N_crop2$Date)
# ==> min date = 2018-05-06 02:17:00 
# ==> max date = 2018-06-27 22:43:00

# -----> Temporal selection of layers - CROP 2
###############################################
# Number of first layer
raster_min_2018_crop2 <- str_which(names(zon_2018),
                                   "2018.05.06.00.00")
# Number of last layer based on the maximal date
raster_max_2018_crop2 <- str_which(names(zon_2018),
                                   "2018.06.27.18.00")
# Selection of layers
zon_2 <- zon_2018[[raster_min_2018_crop2:raster_max_2018_crop2]]
mer_2 <- mer_2018[[raster_min_2018_crop2:raster_max_2018_crop2]]
speed_2 <- speed_2018[[raster_min_2018_crop2:raster_max_2018_crop2]]

# -----> Spatial selection of layers - CROP 2
##############################################
ext_2018_crop2 <- extent(40, 75, -15, 15) # xmin, xmax, ymin, ymax
z_2018_typ_N_crop2 <- crop(x = zon_2,
                           y = ext_2018_crop2)
m_2018_typ_N_crop2 <- crop(x = mer_2,
                           y = ext_2018_crop2)
s_2018_typ_N_crop2 <- crop(x = speed_2,
                           y = ext_2018_crop2)

# -----> 2018 - NORTHERN TYPICAL - CROP 2 MAPS PRODUCTION ####
##############################################################
for (i in 1:nlayers(z_2018_typ_N_crop2)) {
    
    dat <- substr(names(z_2018_typ_N_crop2[[i]]), 2, 17)
    dat_conv <- as.POSIXlt(dat,
                           format = "%Y.%m.%d.%H.%M")
    # ----- #
    locs_070 <- argos_2018_typ_N_crop2[argos_2018_typ_N_crop2$Vessel == "162070"
                                      & argos_2018_typ_N_crop2$raster_layer == dat, ]
    back_locs_070 <- argos_2018_typ_N_crop2[argos_2018_typ_N_crop2$Vessel == "162070"
                                           & argos_2018_typ_N_crop2$Date < dat_conv, ]
    # ----- #
    locs_563 <- argos_2018_typ_N_crop2[argos_2018_typ_N_crop2$Vessel == "166563"
                                      & argos_2018_typ_N_crop2$raster_layer == dat, ]
    back_locs_563 <- argos_2018_typ_N_crop2[argos_2018_typ_N_crop2$Vessel == "166563"
                                      & argos_2018_typ_N_crop2$Date < dat_conv, ]
    # ----- 162070 ----- #
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-162070/",
                "2018-TYP-N-162070-CROP2-",
                names(z_2018_typ_N_crop2[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2018_typ_N_crop2[[i]], z_2018_typ_N_crop2[[i]]),
                  isField = 'dXY',
                  region =  s_2018_typ_N_crop2[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2018 - TYP N - 162070 - ",
                               names(m_2018_typ_N_crop2[[i]]))) +
    layer(c(sp.points(back_locs_070,
                      col = "#caf599",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_070,
                      col = "#5aa505",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
print(paste("162070 - ", i, "/", nlayers(z_2018_typ_N_crop2), sep = ""))
# ----- 166563 ----- #
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-166563/",
                "2018-TYP-N-166563-CROP2-",
                names(z_2018_typ_N_crop2[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2018_typ_N_crop2[[i]], z_2018_typ_N_crop2[[i]]),
                  isField = 'dXY',
                  region =  s_2018_typ_N_crop2[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2018 - TYP N - 166563 -",
                               names(m_2018_typ_N_crop2[[i]]))) +
    layer(c(sp.points(back_locs_563,
                      col = "#aec1f5",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_563,
                      col = "#0042f8",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
}

# -----> CROP 3 ####
####################

# Time range : 28 juin 2018 to 11 juillet 2018
# -----> Subset of argos data
#############################
# <----- Device 162070 & 166563 ----->
min_date <- as.POSIXlt("2018-06-28", format = "%Y-%m-%d")
max_date <- as.POSIXlt("2018-07-11", format = "%Y-%m-%d")
argos_2018_typ_N_crop3 <- argos_2018_typ_N[date(argos_2018_typ_N$Date) <= max_date & date(argos_2018_typ_N$Date) >= min_date, ]
summary(argos_2018_typ_N_crop3$Date)
# ==> min date = 2018-06-28 00:25:00 
# ==> max date = 2018-07-11 15:14:00
mapview(argos_2018_typ_N_crop3, zcol = "Vessel")

# -----> Temporal selection of layers - CROP 3
###############################################
# Number of first layer
raster_min_2018_crop3 <- str_which(names(zon_2018),
                                   "2018.06.28.00.00")
# Number of last layer based on the maximal date
raster_max_2018_crop3 <- str_which(names(zon_2018),
                                   "2018.07.11.18.00")
# Selection of layers
zon_3 <- zon_2018[[raster_min_2018_crop3:raster_max_2018_crop3]]
mer_3 <- mer_2018[[raster_min_2018_crop3:raster_max_2018_crop3]]
speed_3 <- speed_2018[[raster_min_2018_crop3:raster_max_2018_crop3]]

# -----> Spatial selection of layers - CROP 3
##############################################
ext_2018_crop3 <- extent(70, 100, -15, 15) # xmin, xmax, ymin, ymax
z_2018_typ_N_crop3 <- crop(x = zon_3,
                           y = ext_2018_crop3)
m_2018_typ_N_crop3 <- crop(x = mer_3,
                           y = ext_2018_crop3)
s_2018_typ_N_crop3 <- crop(x = speed_3,
                           y = ext_2018_crop3)

# -----> 2018 - NORTHERN TYPICAL - CROP 3 MAPS PRODUCTION ####
##############################################################
for (i in 1:nlayers(z_2018_typ_N_crop3)) {
    
    dat <- substr(names(z_2018_typ_N_crop3[[i]]), 2, 17)
    dat_conv <- as.POSIXlt(dat,
                           format = "%Y.%m.%d.%H.%M")
    # ----- #
    locs_070 <- argos_2018_typ_N_crop3[argos_2018_typ_N_crop3$Vessel == "162070"
                                      & argos_2018_typ_N_crop3$raster_layer == dat, ]
    back_locs_070 <- argos_2018_typ_N_crop3[argos_2018_typ_N_crop3$Vessel == "162070"
                                           & argos_2018_typ_N_crop3$Date < dat_conv, ]
    # ----- #
    locs_563 <- argos_2018_typ_N_crop3[argos_2018_typ_N_crop3$Vessel == "166563"
                                      & argos_2018_typ_N_crop3$raster_layer == dat, ]
    back_locs_563 <- argos_2018_typ_N_crop3[argos_2018_typ_N_crop3$Vessel == "166563"
                                      & argos_2018_typ_N_crop3$Date < dat_conv, ]
    # ----- 162070 ----- #
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-162070/",
                "2018-TYP-N-162070-CROP3-",
                names(z_2018_typ_N_crop3[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2018_typ_N_crop3[[i]], z_2018_typ_N_crop3[[i]]),
                  isField = 'dXY',
                  region =  s_2018_typ_N_crop3[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2018 - TYP N - 162070 - ",
                               names(m_2018_typ_N_crop3[[i]]))) +
    layer(c(sp.points(back_locs_070,
                      col = "#caf599",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_070,
                      col = "#5aa505",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
print(paste("162070 - ", i, "/", nlayers(z_2018_typ_N_crop3), sep = ""))
# ----- 166563 ----- #
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-166563/",
                "2018-TYP-N-166563-CROP3-",
                names(z_2018_typ_N_crop3[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2018_typ_N_crop3[[i]], z_2018_typ_N_crop3[[i]]),
                  isField = 'dXY',
                  region =  s_2018_typ_N_crop3[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2018 - TYP N - 166563 -",
                               names(m_2018_typ_N_crop3[[i]]))) +
    layer(c(sp.points(back_locs_563,
                      col = "#aec1f5",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_563,
                      col = "#0042f8",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()
}
# -----> CROP 4 ####
####################

# Time range : from 12 juillet 2018
# -----> Subset of argos data
#############################
# <----- Device 166563 ----->
min_date <- as.POSIXlt("2018-07-12", format = "%Y-%m-%d")
argos_2018_typ_N_crop4 <- argos_2018_typ_N[date(argos_2018_typ_N$Date) >= min_date, ]
summary(argos_2018_typ_N_crop4$Date)
# ==> min date = 2018-07-12 14:39:00
# ==> max date = 018-10-24 10:17:00
mapview(argos_2018_typ_N_crop4, zcol = "Vessel")

# -----> Temporal selection of layers - CROP 4
###############################################
# Number of first layer
raster_min_2018_crop4 <- str_which(names(zon_2018),
                                   "2018.07.12.00.00")
# Number of last layer based on the maximal date
raster_max_2018_crop4 <- str_which(names(zon_2018),
                                   "2018.10.24.18.00")
# Selection of layers
zon_4 <- zon_2018[[raster_min_2018_crop4:raster_max_2018_crop4]]
mer_4 <- mer_2018[[raster_min_2018_crop4:raster_max_2018_crop4]]
speed_4 <- speed_2018[[raster_min_2018_crop4:raster_max_2018_crop4]]

# -----> Spatial selection of layers - CROP 4
##############################################
ext_2018_crop4 <- extent(70, 110, -25, 5) # xmin, xmax, ymin, ymax
z_2018_typ_N_crop4 <- crop(x = zon_4,
                           y = ext_2018_crop4)
m_2018_typ_N_crop4 <- crop(x = mer_4,
                           y = ext_2018_crop4)
s_2018_typ_N_crop4 <- crop(x = speed_4,
                           y = ext_2018_crop4)

# -----> 2018 - NORTHERN TYPICAL - CROP 4 MAPS PRODUCTION ####
##############################################################
for (i in 1:nlayers(z_2018_typ_N_crop4)) {
    
    dat <- substr(names(z_2018_typ_N_crop4[[i]]), 2, 17)
    dat_conv <- as.POSIXlt(dat,
                           format = "%Y.%m.%d.%H.%M")
    # ----- #
    locs_563 <- argos_2018_typ_N_crop4[argos_2018_typ_N_crop4$raster_layer == dat, ]
    back_locs_563 <- argos_2018_typ_N_crop4[argos_2018_typ_N_crop4$Date < dat_conv, ]
# ----- 166563 ----- #
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-166563/",
                "2018-TYP-N-166563-CROP4-",
                names(z_2018_typ_N_crop4[[i]]),
              ".png",
              sep = ""),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    # x11()
    print(
    vectorplot(stack(m_2018_typ_N_crop4[[i]], z_2018_typ_N_crop4[[i]]),
                  isField = 'dXY',
                  region =  s_2018_typ_N_crop4[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("2018 - TYP N - 166563 -",
                               names(m_2018_typ_N_crop4[[i]]))) +
    layer(c(sp.points(back_locs_563,
                      col = "#aec1f5",
                      lwd = 3,
                      cex = 2),
            sp.points(locs_563,
                      col = "#0042f8",
                      lwd = 6,
                      cex = 4)))
    )

dev.off()

print(paste("166563 - ", i, "/", nlayers(z_2018_typ_N_crop4), sep = ""))
}

# -----> PPT creation ####
##########################

# File list
files_2018_563 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-166563/")
length(files_2018_563) # the shortest

files_2018_070 <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-162070/")
length(files_2018_070)

# Doc creation
doc <- read_pptx()
doc <- add_slide(doc,
                 layout = "Two Content",
                 master = "Office Theme")

for (i in 1:length(files_2018_563)) {
    
    # file path
    img_file_563 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-166563/",
                          files_2018_563[i],
                          sep = "")
    if (!is.na(files_2018_070[i])) {
        img_file_070 <- paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/2018-TYP-N-162070/",
                              files_2018_070[i],
                              sep = "")
        doc <- ph_with(x = doc,
                       value = paste("Typical (North) tracks in 2018 - ",
                       substr(files_2018_563[i],
                                      26,
                                      41),
                       sep = ""),
                       location = ph_location_type(type = "title"))
        doc <- ph_with(x = doc,
                       value = "166563",
                       location = ph_location_left())
        doc <- ph_with(x = doc,
                       value = "162070",
                       location = ph_location_right())
        doc <- ph_with(x = doc,
                       value = paste(i, "/", length(files_2018_563), sep = ""),
                       location = ph_location_type(type = "ftr"))
        doc <- ph_with(x = doc,
                       value = external_img(img_file_563),
                       location = ph_location_left(),
                       use_loc_size = TRUE)
        doc <- ph_with(x = doc,
                       value = external_img(img_file_070),
                       location = ph_location_right(),
                       use_loc_size = TRUE)
        doc <- add_slide(doc)
    } else {
                doc <- ph_with(x = doc,
                       value = substr(files_2018_563[i],
                                      26,
                                      41),
                       location = ph_location_type(type = "title"))
        doc <- ph_with(x = doc,
                       value = "166563",
                       location = ph_location_left())
        doc <- ph_with(x = doc,
                       value = paste(i, "/", length(files_2018_563), sep = ""),
                       location = ph_location_type(type = "ftr"))
        doc <- ph_with(x = doc,
                       value = external_img(img_file_563),
                       location = ph_location_left(),
                       use_loc_size = TRUE)
        doc <- add_slide(doc)
    }
    print(i)
}

print(doc,
      target = "C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/2018_TYPICAL_NORTH_TRACKS.pptx")
