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

month(gls_NR$DATE, label = FALSE)

gls_NR_sp <- SpatialPointsDataFrame(coords = gls_NR[, c("LON", "LAT")],
                                  data = gls_NR,
                                  proj4string = CRS("+init=epsg:4326"))

# ----- Chlo data ----- #
chlo2018 <- list.files("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/",
                   pattern = "OCEANCOLOUR_GLO_CHL_L4_REP_OBSERVATIONS_009_082-TDS__CHLO-2018",
                   full.names = TRUE)

# ----- JANVIER ----- #
# ------------------- #

chlo_jan <- terra::rast(chlo2018[1])
summary(values(chlo_jan))

# ----- Restreindre à la zone d'hivernage des adultes ----- #
# lat. from 55E to 105E
# lon. from 10S to 35S

extend <- extent(55, 105, -35, -10) # xmin, xmax, ymin, ymax
chlo_jan_crop <- crop(chlo_jan,
                    extend)

chlo_jan2 <- sum(chlo_jan_crop)

# ----- Plot ----- #
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(40)

levelplot(chlo_jan2,
          main = "Somme des concentrations - janvier 2018",
          col.regions = my_cols) +
    layer(sp.points(gls_NR_sp[month(gls_NR_sp$DATE) == 1,],
                    col = rgb(0, 0, 1, alpha = 0.5),
                    lwd = 2))
# -------------------------------------------- #
# ----- Automatisation of map producing ----- #
# -------------------------------------------- #


# ----- Map producing ----- #

for(i in 1:12){
    
    chlo <- terra::rast(chlo2018[i])
    chlo_crop <- crop(chlo,
                    extend)
    
    chlo_sum <- sum(chlo_crop)
    
    png(paste("C:/Users/ccjuhasz/Desktop/", month.name[i],"-2018-chlo-a_sum.png", sep = ""),
    res = 300,
    width = 50,
    height = 40,
    pointsize = 20,
    unit = "cm",
    bg = "white")
    
    print(
    levelplot(chlo_sum,
          main = paste("Somme des concentrations - ", month.name[i] ," 2018", sep = ""),
          col.regions = my_cols) +
    layer(sp.points(gls_NR_sp[month(gls_NR_sp$DATE) == i,],
                    col = rgb(0, 0, 1, alpha = 0.2),
                    lwd = 2))
    )
    
    dev.off()
    
    print(month.name[i])
}
