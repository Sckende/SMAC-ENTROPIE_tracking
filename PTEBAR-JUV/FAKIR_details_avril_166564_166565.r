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

# -----> Fakir period ####
##########################

# -----> zon & mer
zon
mer
fak_period <- names(zon)[str_which(names(zon), "X2018.04")]
fak_period <- fak_period[order(fak_period)]

zon1 <- zon[[which(names(zon) %in% fak_period)]]
zon1 <- zon1[[order(names(zon1))]]
names(zon1)

mer1 <- mer[[which(names(mer) %in% fak_period)]]
mer1 <- mer1[[order(names(mer1))]]
names(mer1)

speed1 <- speed[[which(names(speed) %in% fak_period)]]
speed1 <- speed1[[order(names(speed1))]]
names(speed1)

# -----> Crop the rasters
#extent(xmin, xmax, ymin, ymax)
new_ext <- extent(40, 70, -35, 0)
new_z <- crop(x = zon1,
              y = new_ext)
new_m <- crop(x = mer1,
              y = new_ext)
new_s <- crop(x = speed1,
              y = new_ext)
# -----> regarder à partir du 10 avril
# zon2 <- zon1[[37:100]]
# mer2 <- mer1[[37:100]]
# speed2 <- speed1[[37:100]]

# -----> Données argos pour les 2 individus particuliers
argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
arg <- argos[argos$Vessel %in% c("166564", "166565"),]
arg2 <- arg[month(arg$Date) == 4,]
arg2 <- droplevels(arg2)
dim(arg2)
summary(arg2)
head(arg2$raster_layer)

# -----> conversion objet spatial sp
arg2_sp <- SpatialPointsDataFrame(coords = arg2[, c("Longitude", "Latitude")],
                                  data = arg2,
                                  proj4string = CRS("+init=epsg:4326"))
mapview::mapview(arg2_sp)

######################
# ----- > 166564 #####
######################
arg2_sp_564 <- arg2_sp[arg2_sp$Vessel == "166564",]
dim(arg2_sp_564) 
summary(arg2_sp_564$Date)
#                  Min.                  Max.
# "2018-04-04 12:00:00" "2018-04-27 03:27:00"
min_date <- min(arg2_sp_564$Date)
max_date <- max(arg2_sp_564$Date)
seq_date <- seq(date(min_date), date(max_date), 1)

seq_layer <- gsub("-", ".", seq_date)
seq_layer <- paste("X", seq_layer, sep = "")

names_zon_564 <- vector()

for(i in 1:length(seq_layer)){
    v <- str_subset(names(new_z), seq_layer[i])
    
    names_zon_564 <- c(names_zon_564, v)
}

zon_564 <- new_z[[which(names(new_z) %in% names_zon_564)]]
names(zon_564)
mer_564 <- new_m[[which(names(new_m) %in% names_zon_564)]]
names(mer_564)
speed_564 <- new_s[[which(names(new_s) %in% names_zon_564)]]
names(speed_564)

######################
# ----- > 166565 #####
######################
arg2_sp_565 <- arg2_sp[arg2_sp$Vessel == "166565",]
dim(arg2_sp_565) 
summary(arg2_sp_565$Date)
#                  Min.                  Max.
# "2018-04-04 12:00:00" "2018-04-26 18:05:00"
min_date <- min(arg2_sp_565$Date)
max_date <- max(arg2_sp_565$Date)
seq_date <- seq(date(min_date), date(max_date), 1)

seq_layer <- gsub("-", ".", seq_date)
seq_layer <- paste("X", seq_layer, sep = "")

names_zon_565 <- vector()

for(i in 1:length(seq_layer)){
    v <- str_subset(names(new_z), seq_layer[i])
    
    names_zon_565 <- c(names_zon_565, v)
}

zon_565 <- new_z[[which(names(new_z) %in% names_zon_565)]]
names(zon_565)
mer_565 <- new_m[[which(names(new_m) %in% names_zon_565)]]
names(mer_565)
speed_565 <- new_s[[which(names(new_s) %in% names_zon_565)]]
names(speed_565)

# -----> PLOTS ####
###################

# --- Global levelplot --- #
nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       alpha = 0.9,
                       option = "A")(nlev)

# -----> 166564
###############
for(i in 1:nlayers(zon_564)){
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/166564",
              names(zon_564[[i]]),
              ".png"),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    dat <- substr(names(zon_564[[i]]), 2, 17)
    locs <- arg2_sp_564[arg2_sp_564$raster_layer == dat,]
    back_locs <- arg2_sp_564[arg2_sp_564$Date < as.POSIXlt(dat,
                                                           format = "%Y.%m.%d.%H.%M"),]
    # x11()
    print(
    vectorplot(stack(mer_564[[i]], zon_564[[i]]),
                  isField = 'dXY',
                  region =  speed_564[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("166564",
                               names(zon_564[[i]]))) +
    layer(c(sp.points(back_locs, col = "grey", lwd = 5, cex = 2, cex.lab = 2),
            sp.points(locs, col = viridis(7)[3], lwd = 5, cex = 2, cex.lab = 2)))
# )
    )

dev.off()
print(i)
}

# -----> 166565
###############
for(i in 1:nlayers(zon_565)){
    png(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/166565/166565",
              names(zon_565[[i]]),
              ".png"),
    res = 300,
    width = 40,
    height = 50,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")

    dat <- substr(names(zon_565[[i]]), 2, 17)
    locs <- arg2_sp_565[arg2_sp_565$raster_layer == dat,]
    back_locs <- arg2_sp_565[arg2_sp_565$Date < as.POSIXlt(dat,
                                                           format = "%Y.%m.%d.%H.%M"),]
    # x11()
    print(
    vectorplot(stack(mer_565[[i]], zon_565[[i]]),
                  isField = 'dXY',
                  region =  speed_565[[i]],
                  at = my_at,
                  lwd.arrows = 1,
                  aspX = 0.2,
                  narrows = 300,
                  col.regions = my_cols,
                  main = paste("166565",
                               names(zon_565[[i]]))) +
    layer(c(sp.points(back_locs, col = "grey", lwd = 5, cex = 2, cex.lab = 2),
            sp.points(locs, col = viridis(7)[2], lwd = 5, cex = 2, cex.lab = 2)))
# )
    )

dev.off()
print(i)
}
