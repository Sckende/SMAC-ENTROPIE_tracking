rm(list = ls())
# ---- Load packages ------ #
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

# ---- Juvenile tracks ----- #
# juv_argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")

# ---- Juvenile tracks corrected with aniMotum ----- #

juv_argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param.rds")
head(juv_argos)

# data manipulations
juv_argos$depl <- NA
a_l <- split(juv_argos, juv_argos$id)
a_ll <- lapply(a_l,
               function(x){
                    y_dep <- year(x$date[1])
                    x$depl <- y_dep
                    x
               })
juv_argos <- do.call("rbind", a_ll)
names(juv_argos)[1] <- "Vessel"
names(juv_argos)[c(3, 4)] <- c("Longitude", "Latitude")

length(unique(juv_argos$Vessel))

class(juv_argos)
juv_sp <- SpatialPointsDataFrame(coords = juv_argos[, c("Longitude", "Latitude")],
                                 data = juv_argos,
                                 proj4string = CRS("+init=epsg:4326"))

juv_sp_list <- split(juv_sp, juv_sp$Vessel)
track <- lapply(juv_sp_list,
                function(x) {
                    Lines(list(Line(coordinates(x))),
                          x$Vessel[1L])
                })

lines <- SpatialLines(track)
data_juv_track <- data.frame(Vessel = unique(juv_sp$Vessel))
rownames(data_juv_track) <- data_juv_track$Vessel
argos_lines <- SpatialLinesDataFrame(lines, data_juv_track)
argos_lines
plot(argos_lines)

# ---- Adulte tracks ----- #
ad_gls <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_2008-2009_clean_from_Audrey.txt",
                     h = T,
                     sep = "\t")

ad_gls <- ad_gls[ad_gls$STATUT == "NR", ]

length(unique(ad_gls$ID))

class(ad_gls)
ad_sp <- SpatialPointsDataFrame(coords = ad_gls[, c("LON", "LAT")],
                                 data = ad_gls,
                                 proj4string = CRS("+init=epsg:4326"))

ad_sp_list <- split(ad_sp, ad_sp$ID)
track <- lapply(ad_sp_list,
                function(x) {
                    Lines(list(Line(coordinates(x))),
                          x$ID[1L])
                })

lines <- SpatialLines(track)
data_ad_track <- data.frame(Vessel = unique(ad_sp$ID))
rownames(data_ad_track) <- data_ad_track$Vessel
gls_lines <- SpatialLinesDataFrame(lines, data_ad_track)
gls_lines
plot(gls_lines)


# ----- Maps ----- #
library(maps)
library(maptools)
library(geodata)

lon_min = 30
lon_max = 120 #defini la fenetre de l'ocean indien ou les petrels peuvent potentiellement s'alimenter
lat_min = -45
lat_max = 20

w <- world(path = getwd())
x11(); 
plot(w,
     col = "grey",
     border = "white",
     xlim = c(lon_min, lon_max),
     ylim = c(lat_min, lat_max))
plot(ad_sp,
     col = "#0d850d",
     add = TRUE)
plot(juv_sp,
     col = "turquoise",
     add = TRUE)
#### map with full tracks ad & juv ####
# different colors depending on year of deployment and ad vs juv
# png("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/carto_globale_locs_corrigees_aniMotum/ad_juv_tracks.png",
#     res = 300,
#     width = 50,
#     height = 40,
#     pointsize = 20,
#     unit = "cm",
#     bg = "white")
x11()

plot(w,
     col = "grey",
     border = "white",
     xlim = c(lon_min, lon_max),
     ylim = c(lat_min, lat_max))
plot(gls_lines,
     col = "#b66309",
     add = TRUE,
     lwd = 3)
plot(argos_lines[argos_lines$Vessel %in% c("162070",
                                           "162072",
                                           "162073",
                                           "166561",
                                           "166563",
                                           "166564",
                                           "166565"),],
     col = "darkgreen",
     ad = T,
     lwd = 3) # 2018
plot(argos_lines[argos_lines$Vessel %in% c("166566",
                                           "166568",
                                           "166569",
                                           "166572"),],
     add = T,
     col = "#6ed96e",
     lwd = 3) # 2017

dev.off()

#### map with full tracks juv & adult kernel ####
ad_k <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/MS_DATA/PTEBAR_AD_GLS_kernels.rds")
head(ad_k)

# png("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/PTEBAR_ARGOS_figures/carto_globale_locs_corrigees_aniMotum/juv_tracks_kern_ad_colorblind.png",
#     res = 300,
#     width = 50,
#     height = 40,
#     pointsize = 20,
#     unit = "cm",
#     bg = "white")
x11()

plot(w,
     col = "grey",
     border = "white",
     xlim = c(lon_min, lon_max),
     ylim = c(lat_min, lat_max))
plot(ad_k[[2]],
     col = "#d55c009d",
     add = TRUE,
     lwd = 0.5)
plot(argos_lines[argos_lines$Vessel %in% c("162070",
                                           "162072",
                                           "162073",
                                           "166561",
                                           "166563",
                                           "166564",
                                           "166565"),],
     col = "#009e73",
     ad = T,
     lwd = 3) # 2018
plot(argos_lines[argos_lines$Vessel %in% c("166566",
                                           "166568",
                                           "166569",
                                           "166572"),],
     add = T,
     col = "#56b4e9",
     lwd = 3) # 2017

dev.off()