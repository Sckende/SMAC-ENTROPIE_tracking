rm(list = ls())

source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")
require(aniMotum)
require(patchwork)
# infos papier Pinet et al 2011 - Migration, wintering distribution and habitat use of an endangered tropical seabird, Barau’s petrel Pterodroma baraui
pap <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/Infos_gls_tracks_Pinet_2011.txt",
                  sep = "\t",
                  h = T)
dim(pap)
pap <- pap[pap$STATUS == "Breed", ] # uniquement les inds en repro
unique(pap$ID)
pap$ID2 <- substr(pap$ID, 1, 4)

# GLS data from Audrey 
ad_gls <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_2008-2009_clean_from_Audrey.txt",
                     h = T,
                     sep = "\t")
head(ad_gls)
table(ad_gls$STATUT)
unique(ad_gls$ID)

# Correction des IDs
ad_gls$ID[ad_gls$ID == "8095-FS758-8-GBNT1"] <- "8095-FS758_8-GBNT1"
ad_gls$ID[ad_gls$ID == "8111-FS---91-MM4-R"] <- "8111-FS___91-MM4-R"
ad_gls$ID[ad_gls$ID == "8123-FS77-86-GBNT1"] <- "8123-FS77_86-GBNT1"

ID1 <- str_split(ad_gls$ID, "-")

ID11 <- lapply(ID1, function(x){
  paste(x[1], x[3], sep = "-")
})

ad_gls$ID2 <- unlist(ID11)
unique(ad_gls$ID2)[unique(ad_gls$ID2) %in% pap$ID == F]
unique(ad_gls$ID2)[unique(ad_gls$ID2) %in% pap$ID == T] # 16 IDS en commun au total en retirant les non-breeders et les fails - CHECK

# --------------- #
# keep only individuals in Pinet ms
ad_nr <- ad_gls[ad_gls$ID2 %in% pap$ID,]
unique(ad_nr$ID2)
ad_nr$DATE <- as.POSIXct(ad_nr$DATE,
                         format = "%d/%m/%Y %H:%M")

# add dates of departure and arrival in wintering core area
gls <- left_join(ad_nr,
                 pap[, c("ID", "DEPARTURE", "ARRIVAL_CORE_WINTER", "DEPART_CORE_WINTER")],
                 by = c("ID2" = "ID"))
head(gls)
dim(gls)
unique(gls$ID2)

# verif
lapply(split(gls, gls$ID2), dim)


#### ---- Correction des dates & retrait des individus non utilisables après exploration visuelle---- ####
# VOIR
# 8105-MM14 - remplacer date de départ de colonie par 10 avril 2009
# 8108-GBN72 - remplacer date de départ de colonie par 3 avril 2009
# 8123-GBNT1 - à retirer car les deux dates erronnées

gls1 <- gls[!gls$ID2 == "8123-GBNT1",]

gls1$DEPARTURE[gls1$ID2 == "8105-MM14"] <- "10/04/2009"
gls1$DEPARTURE[gls1$ID2 == "8108-GBN72"] <- "3/04/2009"

summary(gls1)
gls1 <- gls1[!is.na(gls1$DATE), ]
# subset loc between colony departure & wintering area arrival
ls <- split(gls1, gls1$ID2)

# x <- ls[[4]]

migr_ls <- lapply(ls, function(x) {
  dep <- date(strptime(unique(x$DEPARTURE),
                  "%d/%m/%Y"))
  arr <- date(strptime(unique(x$ARRIVAL_CORE_WINTER),
                  "%d/%m/%Y"))
  
  x <- x[date(x$DATE) <= arr & date(x$DATE) >= dep, ]
  print(paste("##########", unique(x$ID2), "##########"))
  print(paste("depart - ", as.character(dep), " & arrivée - ", as.character(arr)))
  print(range(date(x$DATE)))
  x
})


#### ---- Retrait de la 1ere loc pour 8108-GBN72 & 8105-MM14 ---- ####
# car anterieur au départ de la colonie - détection visuelle

migr_ls[["8108-GBN72"]] <- migr_ls[["8108-GBN72"]][-1,]
migr_ls[["8105-MM14"]] <- migr_ls[["8105-MM14"]][-1,]


#### ---- Obtention du DF avec loc migration vers wintering area ---- ####
migr <- do.call("rbind",
                migr_ls)

dim(migr)
summary(migr)
summary(migr$DATE)
migr <- migr[!is.na(migr$DATE), ]
table(month(migr$DATE)) # from march to May




#### ---- spatial object ---- ####
xy <- migr[,c('LON', 'LAT')]
migr_sp_ll <- SpatialPointsDataFrame(coords = xy,
                                   data = migr,
                                   proj4string = CRS("+proj=longlat +datum=WGS84"))
migr_sp_UTM <- spTransform(migr_sp_ll,
                         CRS('+init=epsg:32743'))

mapview(split(migr_sp_ll, migr_sp_ll$ID2))


pts_out <- migr_sp_ll
class(pts_out)
table(month(pts_out$DATE)) # mars avril mai

# Map
pts_out_sf <- sf::st_as_sf(pts_out,
                           coords = c("LON", "LAT"),
                           crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
x11()
mapview(pts_out_sf,
        zcol = "ID2")

#### ---- track creation ---- ####
tracks <- pts_out_sf %>%
  group_by(ID2) %>%
  arrange(DATE) %>%
  summarize(do_union = FALSE) %>% # do_union = FALSE for 
  st_cast("LINESTRING")

x11(); plot(st_geometry(tracks))
plot(st_geometry(tracks[2, ])) 
mapview(pts_out_sf,
        zcol = "ID2") + mapview(tracks,
                                zcol = "ID2")

for(i in unique(pts_out_sf$ID2)) {
  print(mapview(pts_out_sf[pts_out_sf$ID2 == i,]) + mapview(tracks[tracks$ID2 == i,]))
  } # <== montrer à Audrey

#### ---- Extraction of wind characteristics under locs ---- ####
# ---- Creation des rasters
env_folder <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2018" 
list_names <- list.files(env_folder,
                         pattern = "2008|2009",
                         full.names = T)

east_ls <- list_names[str_detect(list_names,
                                 pattern = "eastward")]
north_ls <- list_names[str_detect(list_names,
                                  pattern = "northward")]
speed_ls <- list_names[str_detect(list_names,
                                  pattern = "speed")]

wind_speed_stack <- terra::rast(speed_ls)
table(duplicated(time(wind_speed_stack)))
wind_speed_stack <- wind_speed_stack[[!duplicated(time(wind_speed_stack))]]
# time(wind_speed_stack)[order(time(wind_speed_stack))] 

wind_north_stack <- terra::rast(north_ls)
table(duplicated(time(wind_north_stack)))
wind_north_stack <- wind_north_stack[[!duplicated(time(wind_north_stack))]]
# time(wind_north_stack)[order(time(wind_north_stack))] 

wind_east_stack <- terra::rast(east_ls)
table(duplicated(time(wind_east_stack)))
wind_east_stack <- wind_east_stack[[!duplicated(time(wind_east_stack))]]
# time(wind_east_stack)[order(time(wind_east_stack))] 

#######################################
# ---- For WIND speed & orientation ####
########################################

head(pts_out, 50)
pts_out$time_loc_min <- hour(pts_out$DATE)*60 + minute(pts_out$DATE)

# raster aux 6h 
# deb      fin       raster.hour     raster.day
# 21:01 (1261) -> 03:00 (180)  ==> 00:00           J+1 or J depending on bef/aft midnight
# 03:01 (181)  -> 09:00 (540)  ==> 06:00            J
# 09:01 (541)  -> 15:00 (900)  ==> 12:00            J
# 15:01 (901)  -> 21:00 (1260) ==> 18:00            J
pts_out$loc_raster_hour <- NA
pts_out$loc_raster_hour[pts_out$time_loc_min >= 1261 | pts_out$time_loc_min <= 180] <- "00:00:00"
pts_out$loc_raster_hour[pts_out$time_loc_min >= 181 & pts_out$time_loc_min <= 540] <- "06:00:00"
pts_out$loc_raster_hour[pts_out$time_loc_min >= 541 & pts_out$time_loc_min <= 900] <- "12:00:00"
pts_out$loc_raster_hour[pts_out$time_loc_min >= 901 & pts_out$time_loc_min <= 1260] <- "18:00:00"

pts_out$loc_raster_date <- ifelse(pts_out$time_loc_min >= 1261,
                                  as.character(date(pts_out$DATE)+1),
                                  as.character(date(pts_out$DATE)))

pts_out$loc_raster_layer <- paste(pts_out$loc_raster_date,
                                  pts_out$loc_raster_hour,
                                  sep = ' ')


pts_out_list <- split(pts_out,
                      pts_out$loc_raster_layer)
length(pts_out_list)


fl3 <- lapply(pts_out_list, function(x) {
  # ----- extraction at location #
  speed_raster <- wind_speed_stack[[str_which(as.character(time(wind_speed_stack)),
                                              unique(x$loc_raster_layer))]]
  
  north_raster <- wind_north_stack[[str_which(as.character(time(wind_north_stack)),
                                              unique(x$loc_raster_layer))]]
  
  east_raster <- wind_east_stack[[str_which(as.character(time(wind_east_stack)),
                                            unique(x$loc_raster_layer))]]
  # ----- fusion with df #
  wind_speed_extr <- terra::extract(speed_raster,
                                    terra::vect(x[, c("LON", "LAT")]))
  wind_north_extr <- terra::extract(north_raster,
                                    terra::vect(x[, c("LON", "LAT")]))
  wind_east_extr <- terra::extract(east_raster,
                                   terra::vect(x[, c("LON", "LAT")]))
  
  # ----- #
  x$wind_speed_loc <- wind_speed_extr[, 2]
  x$wind_north_loc <- wind_north_extr[, 2]
  x$wind_east_loc <- wind_east_extr[, 2]
  
  # ---- #
  print(unique(x$loc_raster_layer))
  # ----- #
  x
})
print("ayééé")

# creation du DF
pts_out_wind <- do.call("rbind", fl3)

# formatage des variables vents
summary(pts_out_wind)

pts_out_wind$wind_spd_loc_km_h <- pts_out_wind$wind_speed_loc* 3.6 # conversion wind speed en km/h
summary(pts_out_wind$wind_spd_loc_km_h) # COMPARER AVEC LES BEBES

# calcul orientation vent
# wind_dir <- 180 * atan2(v, u) / pi # with u = east and v = north AND atan2 gives direction in radian, then *180/pi allows the conversion in degree from -180 to 180
# In addition, HERE the atan2 gives the angle with METEOROLOGICAL convention
# N = 0 = 360, E = 90, S = 180, W = 270

# for locs
pts_out_wind$wind_meteo_dir_loc <- 180 * atan2(pts_out_wind$wind_north_loc, pts_out_wind$wind_east_loc) / pi # From -180 to 180°

summary(pts_out_wind$wind_meteo_dir_loc)
hist(pts_out_wind$wind_meteo_dir_loc)

pts_out_wind$wind_meteo_dir0_360_loc <- ifelse(pts_out_wind$wind_meteo_dir_loc >= 0,
                                               pts_out_wind$wind_meteo_dir_loc,
                                               360 + pts_out_wind$wind_meteo_dir_loc) # From 0 to 360
hist(pts_out_wind$wind_meteo_dir0_360_loc,
     breaks = seq(0, 360, 5)) # vent du sud

# visualisation des points avec un vectorplot
nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)

rasterVis::vectorplot(raster::stack(raster(wind_east_stack[[month(time(wind_east_stack)) == 4]]),
                                    raster(wind_north_stack[[month(time(wind_north_stack)) == 4]])),
                      isField = 'dXY',
                      units = "degrees",
                      narrows = 1000,
                      lwd.arrows = 2,
                      aspX = 0.1,
                      region = raster(wind_speed_stack[[month(time(wind_speed_stack)) == 4]]),
                      at = my_at,
                      col.regions = my_cols,
                      colorkey = list(labels = list(cex = 1.5),
                                      title = list("wind speed (km/h)",
                                                   cex = 2,
                                                   vjust = 0)),
                      #    main = list(label = paste("week # ", i, " 2018", sep = ""),
                      #    cex = 3),
                      xlab = list(label = "Longitude", 
                                  cex = 2),
                      ylab = list(label = "Latitude",
                                  cex = 2),
                      scales = list(x = list(cex = 1.5),
                                    y = list(cex = 1.5))) +
  layer(c(sp.points(as_Spatial(pts_out[month(pts_out$DATE) == 4,]),
                    col = "white",
                    cex = 3,
                    lwd = 3),
          sp.polygons(ne_countries(),
                      col = "#e3d0d0",
                      fill = "#e3d0d0")))

#### ---- Calcul de l'orientation des oiseaux ---- ####
library(trajr)

l <- split(pts_out_wind, pts_out_wind$ID)

dir <- lapply(l, function(x){
  t <- as.data.frame(x)
  # conversion sp object
  t_sp <- SpatialPoints(t[, c("LON", "LAT")],
                        proj4string = CRS("+proj=longlat"))
  # obtention UTM coord
  t_UTM <- spTransform(t_sp,
                       CRS("+init=epsg:32743"))
  # obtention object trajectory
  coord <- data.frame(x = t_UTM$LON,
                      y = t_UTM$LAT,
                      date = as.numeric(t$DATE))
  trj <- TrajFromCoords(coord,
                        timeCol = "date",
                        spatialUnits = "m")
  # obtention des angles
  rad <- TrajAngles(trj,
                    compass.direction = 0)
  deg <- rad*180/pi
  
  x$dir_bird_deg <- c(deg, NA)
  x$x_trj <- trj$x
  x$y_trj <- trj$y
  x
})

dir_DF <- do.call("rbind", dir)
head(dir_DF)

dir_DF$dir_bird_deg0_360 <- ifelse(dir_DF$dir_bird_deg >= 0,
                                   dir_DF$dir_bird_deg,
                                   360 + dir_DF$dir_bird_deg) # From 0 to 360
hist(dir_DF$dir_bird_deg0_360,
     breaks = seq(0, 360, 5))

#### ---- Difference orientation wind and bird ---- ####
dir_DF$diff_wind_bird <- (dir_DF$dir_bird_deg0_360 - dir_DF$wind_meteo_dir0_360_loc) %% 360

# png("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/Wind_bird_diff_orientation/DIFF_bird_wind_ADULTS_go_wintering.png",
#     res = 300,
#     width = 15,
#     height = 20,
#     pointsize = 12,
#     units = "cm",
#     bg = "white")
hist(dir_DF$diff_wind_bird,
     breaks = seq(0, 360, 5),
     freq = F,
     main = "Diff orient° vent & adultes PTEBAR",
     xlab = "angle (°)")
lines(density(dir_DF$diff_wind_bird,
              from = 0,
              to = 360,
              na.rm = T))
# dev.off()

# moyenne circulaire de la difference entre orientation ent & oiseaux
library(circular)
ang_circ <- circular(dir_DF$diff_wind_bird,
                     type = "angles",
                     units = "degrees",
                     modulo = "2pi")
mean.circular(ang_circ, na.rm = T) # 213.9126°

# WindRose

library(ggplot2)
library(patchwork)

head(dir_DF)
names(dir_DF)
class(dir_DF)

dir2 <- as.data.frame(dir_DF)



# x11()
ggplot(dir2) +
  geom_histogram(mapping = aes(diff_wind_bird),
                 fill = "olivedrab3",
                 color = "olivedrab3",
                 alpha = 0.5,
                 binwidth = 1,
                 breaks = seq(0, 360, 10)) +
  scale_x_continuous(breaks = seq(0, 360, 10),
                     limits = c(0, 360)) +
  scale_y_continuous(limits = c(-10, 50)) +
  coord_polar(theta = "x",
              start = 0,
              direction = 1,
              clip = "on") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank()) # ==> Top rendu !


#### ---- Pour comparaison avec juveniles ---- ####
loc <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_diff_wind_bird_dir_110max.rds")

# png("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/Wind_bird_diff_orientation/COMP_DIFF_bird_wind_ADULTS_JUV_mars_mai.png",
#     res = 300,
#     width = 30,
#     height = 20,
#     pointsize = 12,
#     units = "cm",
#     bg = "white")
par(mfrow = c(1, 2))

hist(dir_DF$diff_wind_bird,
     breaks = seq(0, 360, 5),
     freq = F,
     main = "Diff orient° vent & adultes PTEBAR",
     xlab = "angle (°)")
lines(density(dir_DF$diff_wind_bird,
              from = 0,
              to = 360,
              na.rm = T))

hist(loc$diff_wind_bird_loc[month(loc$date) %in% 3:5],
     breaks = seq(0, 360, 5),
     freq = F,
     main = "Diff orient° vent & juveniles PTEBAR",
     xlab = "angle (°)")  
lines(density(loc$diff_wind_bird_loc[month(loc$date) %in% 3:5],
              from = 0,
              to = 360,
              na.rm = T))
# graphics.off()


# ---- windroses ADULTS vs JUVENILES

loc2 <- loc[!is.na(loc$diff_wind_bird_loc),]

par(mfrow = c(1, 2))

# ---- Juveniles
png("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/Wind_bird_diff_orientation/WINDROSES_juveniles.png",
    res = 300,
    width = 30,
    height = 20,
    pointsize = 12,
    units = "cm",
    bg = "white")
ggplot(loc2) +
  geom_histogram(mapping = aes(diff_wind_bird_loc),
                 fill = "dodgerblue4",
                 color = "dodgerblue4",
                 alpha = 0.5,
                 binwidth = 1,
                 breaks = seq(0, 360, 10)) +
  scale_x_continuous(breaks = seq(0, 360, 10),
                     limits = c(0, 360)) +
  scale_y_continuous(limits = c(-70, 350)) +
  coord_polar(theta = "x",
              start = 0,
              direction = 1,
              clip = "on") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())

dev.off()


# ---- Adults
library(cowplot)
# png("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/Wind_bird_diff_orientation/WINDROSES_adults.png",
#     res = 300,
#     width = 30,
#     height = 20,
#     pointsize = 12,
#     units = "cm",
#     bg = "white")

g1 <- ggplot(dir2) +
  geom_histogram(mapping = aes(diff_wind_bird),
                 fill = "olivedrab3",
                 color = "olivedrab3",
                 alpha = 0.5,
                 binwidth = 1,
                 breaks = seq(0, 360, 10)) +
  scale_x_continuous(breaks = seq(0, 360, 10),
                     limits = c(0, 360)) +
  scale_y_continuous(limits = c(-10, 50)) +
  coord_polar(theta = "x",
              start = 0,
              direction = 1,
              clip = "on") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        axis.title = element_blank())

ggdraw() +
  draw_plot(g1) +
  draw_image("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/Petrel2.png",
             x = 0.4538,
             y = 0.46,
             width = 0.095,
             height = 0.095) 
# dev.off()

#### ---- production du KERNEL 90 pour les locs entre l'arrivée et le départ de la wintering core area ---- ####

winter_ls <- lapply(ls, function(x) {

  arr <- date(strptime(unique(x$ARRIVAL_CORE_WINTER),
                       "%d/%m/%Y"))
  dep <- date(strptime(unique(x$DEPART_CORE_WINTER),
                       "%d/%m/%Y"))
  
  x <- x[date(x$DATE) >= arr & date(x$DATE) <= dep, ]
  print(paste("##########", unique(x$ID2), "##########"))
  print(paste("arrivée - ", as.character(arr), " & départ - ", as.character(dep)))
  print(range(date(x$DATE)))
  x
})

winter <- do.call("rbind", winter_ls)

# production du kernel
xy <- winter[,c('LON', 'LAT')]
winter_LONLAT <- SpatialPointsDataFrame(coords = xy,
                                        data = winter,
                                        proj4string = CRS("+proj=longlat +datum=WGS84"))
mapview(winter_LONLAT)

KUD = kernelUD(winter_LONLAT,
               h = 1,
               grid = 500)
KUDvol <- getvolumeUD(KUD)
ver90 <- getverticeshr(KUDvol, 90)

mapview(ver90)

# visualisation tracks migration & kernel

mapview(ver90) + mapview(tracks, color = "darkgrey")
