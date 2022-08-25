#### Bathymetry export ####
library(raster)
library(marmap)
library(maps)
library(mapdata)
library(shape)
library(RColorBrewer)

indian <- getNOAA.bathy(lon1 = 20,
                        lon2 = 130,
                        lat1 = -50,
                        lat2 = 30,
                        resolution = 4)
bathy_raster <- marmap::as.raster(indian)
# terra::writeRaster(bathy_raster,
#                    "C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/Indian_Ocean_Bathy.tif")
# Creating color palettes
blues <- c("lightsteelblue4",
           "lightsteelblue3",
           "lightsteelblue2",
           "lightsteelblue1")

greys <- c(grey(0.6),
           grey(0.93),
           grey(0.99))

blues2 <- rev(brewer.pal(n = 9, name = "Blues"))
blues3 <- blues2[1:6]
# map('worldHires',
#     xlim = c(20, 130),
#     ylim = c(-50, 30),
#     fill = TRUE,
#     col = "grey")

x11(); 
plot.bathy(indian,
                  image = TRUE,
                  land = TRUE,
                  n = 0,
                  drawlabels = FALSE,
                  bpal = list(c(0,
                                max(indian),
                                greys),
                              c(min(indian),
                                0,
                                blues3)))
# scale bar
scaleBathy(indian,
           deg = 3,
           x = "bottomleft",
           inset = 5)

# color legend bar
colorlegend(zlim = c(-max(indian), 0),
            col = blues3,
            main = "depth (m)",
            posx = c(0.85, 0.88))




vignette("marmap-ImportExport")
vignette("marmap-DataAnalysis")
