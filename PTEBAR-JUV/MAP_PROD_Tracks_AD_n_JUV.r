rm(list = ls())
# ---- Load packages ------ #
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

# ---- Juvenile tracks ----- #
juv_argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")

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

lon_min = 21
lon_max = 130 #defini la fenetre de l'ocean indien ou les petrels peuvent potentiellement s'alimenter
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










# ---
IndOcean <- maps::map("world",
             fill = T,
             xlim = c(lon_min, lon_max),
             ylim = c(lat_min, lat_max),
             col = "grey")

IndOcean_sp <- maptools::map2SpatialPolygons(IndOcean,
                                             IDs = IndOcean$names,
                                             proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
x11(); plot(IndOcean)
levelplot(log_JFM_2018,
          main = "JFM 2018") +
layer(c(sp.points(ad_gls_sp[ad_gls_sp$year_period == "JFM", ],
                      col = rgb(0, 0, 1, alpha = 0.9),
                      lwd = 2),
            sp.points(juv_argos_sp[juv_argos_sp$year_period == "JFM" & year(juv_argos_sp$deploy) == 2018, ],
                      col = "white",
                      lwd = 2),
            sp.polygons(IndOcean_sp,
                        col = "grey",
                        fill = "white")))

# ---
mapview(argos_lines, col.regions = "blue")

# ---
x11()
plot(argos_lines,
     col = "blue", add = T)
plot(gls_lines,
     col = "green",
     add = TRUE)

maps::map("world",
          fill = TRUE,
          add = T)

x11()
plot(gls_lines)
maps::map("world",
          fill = TRUE,
          add = TRUE)