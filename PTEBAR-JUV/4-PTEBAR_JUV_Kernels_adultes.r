# ------------------------------------------------------ #
#### OBJECTIFS - Calcul des kernels 50 pour les adultes PTEBAR ####
#### DATA - Données GLS extraites avec TripEstimation Patrick Pinet 2008-2009 
#         - Données GLS extraites avec GeoLight Audrey Jager 2008-20xx
# ------------------------------------------------------ #
rm(list = ls())

# Loading packages ####
source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r')

# Loading data ####
# ____Pinet data
gls_p <- read.table('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Tous_trajets_20082009.txt',
                    sep = '\t',
                    h = T)
head(gls_p)

# ____Audrey data
gls_a <- read.table('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/LOC_TousTrajets_BP_RUN_ALL_YEARS_PHENO_OK_40S.txt',
                    sep = '\t',
                    h = T)[, 1:5]
head(gls_a)

# Conversion DATE ####
# ____Patrick
class(gls_p$DATE)
gls_p$DATE <- as.POSIXct(gls_p$DATE,
                         format = '%Y-%m-%d %H:%M:%S')

summary(gls_p$DATE)
# ____Audrey

class(gls_a$DATE)
gls_a$DATE <- as.POSIXct(gls_a$DATE,
                         format = '%d/%m/%y %H:%M:%S')

summary(gls_a$DATE)
unique(month(gls_a$DATE[gls_a$PHENO == 'HIVERNAGE']))
unique(month(gls_a$DATE[gls_a$PHENO == 'REPRO']))

gls_a$year <- as.factor(year(gls_a$DATE))

tst <- split(gls_a, year(gls_a$DATE))
lapply(tst, function(x){
  table(x$PHENO, useNA = 'always')
  # summary(x$DATE[x$PHENO == 'HIVERNAGE'])
})


# Spatial object conversion ####
# ____ Patrick
xy <- gls_p[,c('LON', 'LAT')]
gls_p_LONLAT <- SpatialPointsDataFrame(coords = xy,
                                 data = gls_p,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
# gls_p_UTM <- spTransform(gls_p_LONLAT,
#                          CRS('+init=epsg:32743'))

class(gls_p_LONLAT)
mapview(gls_p_LONLAT)

# ____ Audrey
xy <- gls_a[,c('LON', 'LAT')]
gls_a_LONLAT <- SpatialPointsDataFrame(coords = xy,
                                 data = gls_a,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
# gls_a_UTM <- spTransform(gls_a_LONLAT,
#                          CRS('+init=epsg:32743'))

class(gls_a_LONLAT)

mapview(gls_a_LONLAT, zcol = 'year', burst = T)
mapview(gls_a_LONLAT[gls_a_LONLAT$PHENO == 'HIVERNAGE',], zcol = 'year', burst = T)

# Retrieve the status (HIVERNAGE vs. REPRO) for Pinet data ####
# From the beginning of April to the end of August

hiv_p <- gls_p_LONLAT[month(gls_p_LONLAT$DATE) %in% 4:8,]
table(month(hiv_p$DATE), useNA = 'always')

mapview(hiv_p)

dat_hiv_p <- SpatialPoints(data.frame(cbind(hiv_p$LON, hiv_p$LAT)))


# Kernal 50 computation ####
# ____ Patrick
KUD_p <- kernelUD(dat_hiv_p,
                h = 2, # ici 1 degré (relié au type de projection, si lat/lon (non projeté) = degré, si UTM (projeté) = m) en rapport à la précision des GLS env. 180km (1deg = 111km) - pour ARGOS précision environ 1km, donc 1/100 degré
                grid = 500)# ici correspond 500x500 degrés (1deg = 111 km à l'équateur)
KUDvol_p <- getvolumeUD(KUD_p)
ver90_p <- getverticeshr(KUDvol_p, 90)
ver50_p <- getverticeshr(KUDvol_p, 50)
ver25_p <- getverticeshr(KUDvol_p, 25)

mapview(list(ver90_p, ver50_p, ver25_p))

ker_p <- list(ver90_p, ver50_p, ver25_p)

# ____ Audrey
hiv_a <- gls_a_LONLAT[gls_a_LONLAT$PHENO == 'HIVERNAGE',]

mapview(hiv_a, zcol = 'year', burst = T)

dat_hiv_a <- SpatialPoints(data.frame(cbind(hiv_a$LON, hiv_a$LAT)))

KUD_a <- kernelUD(dat_hiv_a,
                h = 2, # ici 1 degré (relié au type de projection, si lat/lon (non projeté) = degré, si UTM (projeté) = m) en rapport à la précision des GLS env. 180km (1deg = 111km) - pour ARGOS précision environ 1km, donc 1/100 degré
                grid = 500)# ici correspond 500x500 degrés (1deg = 111 km à l'équateur)
KUDvol_a <- getvolumeUD(KUD_a)

ver90_a <- getverticeshr(KUDvol_a, 90)
ver50_a <- getverticeshr(KUDvol_a, 50)
ver25_a <- getverticeshr(KUDvol_a, 25)

mapview(list(ver90_a, ver50_a, ver25_a))

ker_a <- list(ver90_a, ver50_a, ver25_a)

# mapview(ver90, col.regions = viridis(5)[1]) +
#   mapview(ver80, col.regions = viridis(5)[2])+
#   mapview(ver70, col.regions = viridis(5)[3])+
#   mapview(ver60, col.regions = viridis(5)[4])+
#   mapview(ver50, col.regions = viridis(5)[5])+

mapview(hiv_p,
        cex = 2,
        layer.name = 'GLS_2008-2009') +
  mapview(hiv_a,
          cex = 2,
          col.regions = 'darkorange',
          layer.name = 'GLS_2008-2012') +
mapview(ker_a,
        col.regions = viridis(length(ker_a)),
        layer.name = c('HR90_2008-2012', 'HR50_2008-2012', 'HR25_2008-2012')) +
  mapview(ker_p,
          col.regions = viridis(length(ker_p)),
          layer.name = c('HR90_2008-2009', 'HR50_2008-2009', 'HR25_2008-2009'))


# saveRDS(ker_a,
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_AD_GLS_hivernage_KERNEL905025_Audrey.rds')

# saveRDS(ker_p,
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_AD_GLS_hivernage_KERNEL905025_Patrick.rds')

# saveRDS(list(hiv_a, hiv_p),
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_AD_GLS_hivernage_points.rds')
