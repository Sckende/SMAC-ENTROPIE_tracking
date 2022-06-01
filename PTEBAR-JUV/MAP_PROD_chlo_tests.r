# ---- Load packages ------ #
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")


# ----- GLS adult data ----- #
gls <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_2008-2009_clean_from_Audrey.txt",
                  h = T,
                  sep = "\t")
head(gls)


gls_NR <- gls[gls$STATUT == "NR",]
head(gls_NR)
gls_NR$DATE <- strptime(gls_NR$DATE, "%d/%m/%Y %H:%M")

gls_NR_sp <- SpatialPointsDataFrame(coords = gls_NR[, c("LON", "LAT")],
                                  data = gls_NR,
                                  proj4string = CRS("+init=epsg:4326"))

# ----- Chlo data ----- #
chlo2018 <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/",
                   pattern = "OCEANCOLOUR_GLO_CHL_L4_REP_OBSERVATIONS_009_082-TDS__CHLO-2018",
                   full.names = TRUE)

# ----- AVRIL ----- #
# ------------------- #

chlo_avr <- terra::rast(chlo2018[4])
summary(values(chlo_avr))

# ----- Restreindre à la zone d'hivernage des adultes ----- #
# lat. from 55E to 105E
# lon. from 7.5S to 35S

extend <- extent(55, 105, -35, -9) # xmin, xmax, ymin, ymax
chlo_avr_crop <- crop(chlo_avr,
                    extend)

chlo_avr2 <- sum(chlo_avr_crop)

# ----- Plot ----- #
nlev <- 40
my_at <- seq(from = 0,
             to = 0.6,
             length.out = nlev + 1)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)

levelplot(chlo_avr2,
          main = "Somme des concentrations - avril 2018",
          col.regions = my_cols,
          cuts = nlev - 1) + # cuts is for a continuous scale of colors
    layer(sp.points(gls_NR_sp[month(gls_NR_sp$DATE) == 4, ],
                    col = rgb(0, 0, 1, alpha = 0.1),
                    lwd = 6))
    
# plot(chlo_jui2, col = my_cols)

# -------------------------------------------- #
# ----- Automatisation of map producing ----- #
# -------------------------------------------- #


# ----- Map producing ----- #

for (i in 1:12) {
    
    print(paste(month.name[i], " - START", sep = ""))
    
    chlo <- terra::rast(chlo2018[i])
    chlo_crop <- crop(chlo,
                    extend)
    print("Cropped")
    
    chlo_mean <- mean(chlo_crop)
    
    print("mean values")
    
    png(paste("C:/Users/ccjuhasz/Desktop/",
              month.name[i],
              "-2018-chlo-a_mean.png",
              sep = ""),
    res = 200,
    width = 29.7,
    height = 21,
#     pointsize = 4,
    unit = "cm",
    bg = "white")

    print(
    
    levelplot(chlo_mean,
          main = paste("Moyenne des concentrations - ",
                       month.name[i],
                       " 2018",
                       sep = ""),
          col.regions = my_cols,
          cuts = nlev - 1,
          at = my_at) +
    layer(sp.points(gls_NR_sp[month(gls_NR_sp$DATE) == i, ],
                    col = rgb(0, 0, 1, alpha = 0.2),
                    lwd = 4))
    )

# plot(chlo_mean,
#      main = paste("Moyenne des concentrations - ",
#                   month.name[i],
#                   " 2018",
#                   sep = ""),
#      col = my_cols)
# plot(gls_NR_sp[month(gls_NR_sp$DATE) == i, ],
#      col = rgb(0, 0, 1, alpha = 0.2),
#      lwd = 4,
#      add = T)
    
    dev.off()
    
    print(paste(month.name[i],
                " - DONE",
                sep = ""))
}

# ----- Test de cartes de CHLO-a à plus large échelle spatiale pour les bébés ----- #

for (i in 1:12) {
test <- mean(terra::rast(chlo2018[i]))
values(test)[values(test) > 1] <- NA

png(paste("C:/Users/ccjuhasz/Desktop/",
              month.name[i],
              "-2018-chlo-a_test.png",
              sep = ""),
    res = 200,
    width = 29.7,
    height = 21,
    unit = "cm",
    bg = "white")

    print(
    
    levelplot(test,
          main = paste("Moyenne des concentrations - ",
                       month.name[i],
                       " 2018",
                       sep = ""),
          col.regions = my_cols,
          cuts = nlev - 1,
          at = my_at) +
    layer(sp.polygons(ne_countries()))
    )
    dev.off()
          
}

# ----- Test ségrégation de données en fonction du déplacement ----- #

gls_sp <- SpatialPointsDataFrame(coords = gls[, c("LON", "LAT")],
                                  data = gls,
                                  proj4string = CRS("+init=epsg:4326"))

gls_sp$DATE <- strptime(gls_sp$DATE, "%d/%m/%Y %H:%M")
names(gls_sp)
gls_sp$ID <- as.factor(gls_sp$ID)

mapview(gls_sp,
        zcol = "ID",
        burst = T)

gls_sf <- st_as_sf(gls_sp)
class(gls_sf$DATE)

gls_sf1 <- split(gls_sf, gls_sf$ID)[[1]]
date_lim <- strptime("2008-09-15 00:00", "%Y-%m-%d %H:%M")
gls_sf1_clean <- gls_sf1[gls_sf1$DATE <= date_lim, ]

mapview(gls_sf1_clean)


dist_gls1 <- st_distance(gls_sf1_clean)
dist_gls1_col <- dist_gls1[1, ]

barplot(as.numeric(dist_gls1_col))
hist(dist_gls1_col, breaks = 50)
as.numeric(dist_gls1_col)
unique(gls$ID)

install.packages("segclust2d")
plot(dist_gls1_col, type = "l")












library(segclust2d)
data(simulshift)
head(simulshift)
plot(simulshift$x,
     simulshift$y,
     type = "l")
shift_seg <- segmentation(simulshift,
                       seg.var = c("x", "y"),
                       lmin = 240,
                       Kmax = 25,
                       subsample_by = 60)

plot_likelihood(shift_seg)







df <-  test_data()$data
plot(df, type = "l")
#' # data is a data.frame with column 'x' and 'y'
# Simple segmentation with automatic subsampling
# if data has more than 1000 rows:
res <- segmentation(df, Kmax = 30, lmin = 10, seg.var = c("x","y"))
 # Plot results
 plot(res)
 segmap(res)
 # check likelihood of alternative number of segment possible. There should
 # be a clear break if the segmentation is good
 plot_likelihood(res)
## Not run: 
# Advanced options:
# Run with automatic subsampling if df has more than 500 rows:
res <- segmentation(df, Kmax = 30, lmin = 10,
 seg.var = c("x","y"),  subsample_over = 500)

# Run with subsampling by 2:
res <- segmentation(df, Kmax = 30, lmin = 10, 
seg.var = c("x","y"), subsample_by = 2)
 
# Disable subsampling:
res <- segmentation(df, Kmax = 30, lmin = 10,
 seg.var = c("x","y"), subsample = FALSE)




gls_sf1
plot(gls_sf1$LON,
     gls_sf1$LAT)


unique(gls$ID)
# Run with automatic subsampling if df has more than 500 rows:
res <- segmentation(gls[gls$ID == "8095-FS65297-F3-RU", ],
                    Kmax = 4,
                    lmin = 5,
                    seg.var = c("LON", "LAT"),  subsample_over = 500)
plot(res)
plot_likelihood(res)
 segmap(res)
 
 
 
 
 data(simulmode)
head(simulmode)
class(simulmode)

simulmode$abs_spatial_angle <- abs(simulmode$spatial_angle)
simulmode <- simulmode[!is.na(simulmode$abs_spatial_angle), ]
