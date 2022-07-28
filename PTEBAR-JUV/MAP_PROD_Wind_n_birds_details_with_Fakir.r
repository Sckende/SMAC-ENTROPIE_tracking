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
                 col.regions = my_cols)
#  +
#   layer(sp.lines(argos.track.sp[argos.track.sp$year == 2018,], col = viridis(8), lwd = 3))