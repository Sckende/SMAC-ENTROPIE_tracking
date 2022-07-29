# ----- #
# Wind map + bird points and tracks during the peak of Fakir #####
# ----- #

# targeted devices
# -- 162072
# -- 166561
# -- 166563
# -- 162070
# -- 162073
# -- 166564
# -- 166565

# pic de FAKIR au plus près de la RUN = 24 avril 2018 à 00:00

# ----- #
# Extraction des données vents et visualisation
# ----- #
rm(list = ls())
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

sp <- raster::stack("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2019/WIND_GLO_WIND_L4_REP_OBSERVATIONS_012_006-TDS__wind_speed-2018-1.nc")
spe <- sp[["X2018.04.24.00.00.00"]]

no <- raster::stack("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2019/WIND_GLO_WIND_L4_REP_OBSERVATIONS_012_006-TDS__northward_wind-2018-1.nc")
nor <- no[["X2018.04.24.00.00.00"]]

ea <- raster::stack("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/wind_2008-2019/WIND_GLO_WIND_L4_REP_OBSERVATIONS_012_006-TDS__eastward_wind-2018-1.nc")
eas <- ea[["X2018.04.24.00.00.00"]]

# ----- #
# Zoom in to Reunion #####
# ----- #
new_ext <- extent(40, 70, -35, 0)
speed <- crop(x = spe,
              y = new_ext)
north <- crop(x = nor,
              y = new_ext)
east <- crop(x = eas,
              y = new_ext)

# ----- #
require(maps)
IndOcean <- map("world",
             fill = T,
             xlim = c(40, 70),
             ylim = c(-35, 0),
             col = "grey")

IndOcean_sp <- maptools::map2SpatialPolygons(IndOcean,
                                          IDs = IndOcean$names,
                                proj4string = CRS(projection(speed)))

nlev <- 100
my_at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)
vectorplot(stack(east, north),
                 isField = 'dXY',
                 region = speed,
                 at = my_at,
                 lwd.arrows = 1,
                 aspX = 0.15,
                 narrows = 500,
                 col.regions = my_cols) +
    layer(sp.polygons(IndOcean_sp,
                        col = "grey",
                        fill = "white"))

# ----- #
# Juvenile data #####
# ----- #

argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")

head(argos)
unique(argos$Vessel)
summary(argos)
dim(argos)
argos <- argos[year(argos$Date) == 2018, ]
argos <- argos[argos$Date <= as.Date("2018-05-06"), ]
argos <- argos[argos$Vessel %in% c("162072",
                                   "166561",
                                   "166563",
                                   "162070",
                                   "162073",
                                   "166564",
                                   "166565"), ]
argos <- droplevels(argos)

# ----- #
# Bird POINTS #####
# ----- #

argos_sp <- SpatialPointsDataFrame(coords = argos[, c("Longitude", "Latitude")],
                                  data = argos,
                                  proj4string = CRS("+init=epsg:4326"))
head(argos_sp)
unique(argos_sp$Vessel)
table(argos_sp$Vessel)

argos_sp$color <- "white"
argos_sp$color[as.Date(argos_sp$Date) == as.Date("2018-04-24")] <- "darkgreen" 

# ----- #
# Bird TRACKS #####
# ----- #
argos_sp_list <- split(argos_sp, argos_sp$Vessel)
track <- lapply(argos_sp_list,
                function(x) {
                    Lines(list(Line(coordinates(x))),
                          x$Vessel[1L])
                })

lines <- SpatialLines(track)
data <- data.frame(Vessel = unique(argos_sp$Vessel))
rownames(data) <- data$Vessel
argos_lines <- SpatialLinesDataFrame(lines, data)
argos_lines
head(argos_lines)
argos_lines[argos_lines$Vessel == "166561", ]
# ----- #
# Map prod with wind & tracks #####
# ----- #

for (i in unique(argos_lines$Vessel)) {
    
    # x11()
    png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/PTEBAR_JUV_Carto/FAKIR/FAKIR-",
                i,
                ".png",
                sep = ""),
        res = 300,
        width = 50,
        height = 40,
        pointsize = 20,
        unit = "cm",
        bg = "white")
    print(
    vectorplot(stack(east, north),
                 isField = 'dXY',
                 region = speed,
                 at = my_at,
                 lwd.arrows = 1,
                 aspX = 0.15,
                 narrows = 500,
                 col.regions = my_cols,
                 main = i) +
    layer(c(sp.polygons(IndOcean_sp,
                        col = "grey",
                        fill = "grey"),
            sp.lines(argos_lines[argos_lines$Vessel == i, ],
                     col = "darkgrey",
                     lwd = 5),
            sp.points(argos_sp[argos_sp$Vessel == i, ],
                      col = "darkgrey",
                      cex = 2,
                      lwd = 2),
            sp.points(argos_sp[argos_sp$Vessel == i & as.Date(argos_sp$Date) == as.Date("2018-04-24"), ],
                      col = "#35b835",
                      cex = 4,
                      lwd = 4))
    ))
    dev.off()
}

graphics.off()
