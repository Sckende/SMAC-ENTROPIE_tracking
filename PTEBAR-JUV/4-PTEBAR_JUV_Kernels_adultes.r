# ---------------------------------------------------------------- #
#### OBJECTIFS - Calcul des kernels 50 pour les adultes PTEBAR ####

# Calcul des kernels pour estimer la 'utilization distribution' basé sur les coordonnées non projetées en UTM
# Estimation du 'Smoothing parameter' h avec la méthode 'href' (the bivariate normal kernel) - h controls the width of the kernel function
# ---------------------------------------------------------------- #
#### DATA - Données GLS extraites avec TripEstimation Patrick Pinet 2008-2009 
#         - Données GLS extraites avec GeoLight Audrey Jager 2008-20xx
# -------------------------------------------------------------------------- #
rm(list = ls())

# Loading packages ####
# ------------------- #
source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r')

# Loading data ####
# -------------- #
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
# ------------------ #
# ____Patrick
class(gls_p$DATE)
gls_p$DATE <- as.POSIXct(gls_p$DATE,
                         format = '%Y-%m-%d %H:%M:%S')

summary(gls_p$DATE)

# Row deletion is.na(DATE)
gls_p <- gls_p[!is.na(gls_p$DATE),]

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

# Definition of deployment ID ####
# ----------------------------- #

# Split data based on deployment season 
# Details of deployment based on the Brice paper
# -------------------------------------------- #
# [1] => 1st deployment/retrieval : btw February and April 2008 --> btw September and December 2018 [12 GLS]
# [2] => 2nd deployment/retrieval : btw Nov 2008 & March 2009 --> Sept 2009 & Dec 2009 [9 additional GLS]
# [3] => 3rd deployment/retrieval : Dec 2011 --> November 2012 [additional 12 GLS]

# _____Audrey 
infos.dates.a <- gls_a %>% group_by(ID) %>% 
  summarise(locs = length(ID),
            min.date = min(DATE),
            max.date = max(DATE),
            depl.year = year(min(DATE))) # extraction of the year of the first reloc 

View(infos.dates.a[order(infos.dates.a$min.date),]) # overlap in 2008 between the first and second deployement
infos.dates.a %>% count(depl.year)


infos.dates.a[infos.dates.a$depl.year == 2008,]
infos.dates.a[infos.dates.a$depl.year == 2009,]
infos.dates.a[infos.dates.a$depl.year >= 2011,]

infos.dates.a$depl.numb <- NA
infos.dates.a$depl.numb[infos.dates.a$depl.year >= 2011] <- 3
infos.dates.a$depl.numb[infos.dates.a$depl.year == 2009] <- 2
infos.dates.a$depl.numb[infos.dates.a$depl.year == 2008 & month(infos.dates.a$min.date) > 4] <- 2
infos.dates.a$depl.numb[infos.dates.a$depl.year == 2008 & month(infos.dates.a$min.date) <= 4] <- 1

infos.dates.a %>% count(depl.numb)

# Adding the deployment ID (depl.numb) to AUDREY's DATA
gls_a.1 <- left_join(gls_a, infos.dates.a[, c(1, 6)], 'ID')

# _____Patrick
infos.dates.p <- gls_p %>% group_by(ID) %>% 
  summarise(locs = length(ID),
            min.date = min(DATE),
            max.date = max(DATE),
            depl.year = year(min(DATE))) # extraction of the year of the first reloc 
View(infos.dates.p[order(infos.dates.p$min.date),]) # overlap in 2008 between the first and second deployment
infos.dates.p %>% count(depl.year)

infos.dates.p[infos.dates.p$depl.year == 2008,]
infos.dates.p[infos.dates.p$depl.year == 2009,]

infos.dates.p$depl.numb <- NA

infos.dates.p$depl.numb[infos.dates.p$depl.year == 2009] <- 2
infos.dates.p$depl.numb[infos.dates.p$depl.year == 2008 & month(infos.dates.p$min.date) > 4] <- 2
infos.dates.p$depl.numb[infos.dates.p$depl.year == 2008 & month(infos.dates.p$min.date) <= 4] <- 1

infos.dates.p %>% count(depl.numb)
infos.dates.p[order(infos.dates.p$min.date),]
# Adding the deployment ID (depl.numb) to PP's DATA
gls_p.1 <- left_join(gls_p, infos.dates.p[, c(1, 6)], 'ID')


# Data comparison Audrey vs PP ####
# ------------------------------ #

# Unique ID
length(unique(gls_p.1$ID))
length(unique(gls_a.1$ID[gls_a.1$depl.numb %in% 1:2]))

# Reloc number per ID
gls_p.1 %>% count(ID) # 8 devices with less than 100 relocs
gls_a.1[gls_a.1$depl.numb %in% 1:2,] %>% count(ID) 

# Spatial object conversion ####
# --------------------------- #
# ____ Patrick
xy <- gls_p.1[,c('LON', 'LAT')]
gls_p_LONLAT <- SpatialPointsDataFrame(coords = xy,
                                 data = gls_p.1,
                                 proj4string = CRS("+proj=longlat +datum=WGS84"))
gls_p_UTM <- spTransform(gls_p_LONLAT,
                         CRS('+init=epsg:32743'))

class(gls_p_LONLAT)
mapview(gls_p_LONLAT)

# ____ Audrey
# Try 1
xy <- gls_a.1[,c('LON', 'LAT')]
gls_a_LONLAT <- SpatialPointsDataFrame(coords = xy,
                                 data = gls_a.1,
                                 proj4string = CRS("+proj=longlat +datum=WGS84")) # Engendre une erreur de range de coordonnées

# Try 2
# coordinates(gls_a.1) <- ~ LON + LAT
# gls_a_LONLAT <- gls_a.1
# proj4string(gls_a_LONLAT) <- "+init=epsg:4326"
# 
# class(gls_a_LONLAT)

gls_a_UTM <- spTransform(gls_a_LONLAT,
                         CRS('+init=epsg:32743'))

# Visualization of data Audrey vs. PP ####
# ------------------------------------- #
mapview(gls_a_UTM, zcol = 'depl.numb', burst = T) +
  mapview(gls_p_UTM)

gls_a_HIV <- gls_a_UTM[gls_a_UTM$PHENO == 'HIVERNAGE',]
mapview(gls_a_HIV,
        zcol = 'depl.numb',
        burst = T)

# Retrieve the status (HIVERNAGE vs. REPRO) for Pinet data ####
# From the beginning of April to the end of August

gls_p_HIV <- gls_p_UTM[month(gls_p_UTM$DATE) %in% 4:8,]
table(month(gls_p_HIV$DATE), useNA = 'always')

mapview(gls_a_HIV,
        zcol = 'depl.numb',
        burst = T) + 
  mapview(gls_p_HIV)

gls_a_HIV_list <- split(gls_a_HIV, gls_a_HIV$depl.numb)
gls_p_HIV_list <- split(gls_p_HIV, gls_p_HIV$depl.numb)

gls_HIV <- c(gls_a_HIV_list, gls_p_HIV_list)
names(gls_HIV) <- c('gls_a_1', 'gls_a_2', 'gls_a_3', 'gls_p_1', 'gls_p_2')

mapview(gls_HIV,
        col.regions = viridis((length(gls_HIV))))
# ---------------------------------------------------------------------------- #
# [**** WARNINGS ****] - Problemes de congruence entre les deux dataframes ####
# -------------------------------------------------------------------------- #

# Kernel 50 computation ####
# ----------------------- #
# ____ Patrick
# GLOBAL - href - h = 189001.1 m
KUD_p_href <- kernelUD(gls_p_HIV,
                h = 'href' # ici 1 degré (relié au type de projection, si lat/lon (non projeté) = degré, si UTM (projeté) = m) en rapport à la précision des GLS env. 180km (1deg = 111km) - pour ARGOS précision environ 1km, donc 1/100 degré
                #grid = 500
                )# ici correspond 500x500 degrés (1deg = 111 km à l'équateur)


KUDvol_p_href <- getvolumeUD(KUD_p_href)
ver90_p_href <- getverticeshr(KUDvol_p_href, 90)
ver50_p_href <- getverticeshr(KUDvol_p_href, 50)
ver25_p_href <- getverticeshr(KUDvol_p_href, 25)

ker_p_href <- list(ver90_p_href, ver50_p_href, ver25_p_href)

mapview(ker_p_href) + mapview(gls_p_HIV)

# GLOBAL - LSCV
# KUD_p_LSCV <- kernelUD(gls_p_HIV,
#                        h = 'LSCV') # h = 37609.3 m
# 
# KUDvol_p_LSCV <- getvolumeUD(KUD_p_LSCV)
# ver90_p_LSCV <- getverticeshr(KUDvol_p_LSCV, 90) # grid size problem
# ver50_p_LSCV <- getverticeshr(KUDvol_p_LSCV, 50)
# ver25_p_LSCV <- getverticeshr(KUDvol_p_LSCV, 25)
# 
# ker_p_LSCV <- list(ver50_p_LSCV, ver25_p_LSCV)
# 
# mapview(ker_p_LSCV) + mapview(gls_p_HIV)


# YEAR 1
KUD_p_1 <- kernelUD(gls_HIV[['gls_p_1']],
                    h = 'href')
KUD_p_1@h # 229082.7 m

KUDvol_p_1 <- getvolumeUD(KUD_p_1)
ver50_p_1 <- getverticeshr(KUDvol_p_1, 50)
mapview(ver50_p_1) + mapview(gls_HIV[['gls_p_1']])

# YEAR 2
KUD_p_2 <- kernelUD(gls_HIV[['gls_p_2']],
                    h = 'href')
KUD_p_2@h # 175475.3 m

KUDvol_p_2 <- getvolumeUD(KUD_p_2)
ver50_p_2 <- getverticeshr(KUDvol_p_2, 50)
mapview(ver50_p_2) + mapview(gls_HIV[['gls_p_2']])


# ____ Audrey
# GLOBAL
KUD_a <- kernelUD(gls_a_HIV,
                h = 'href')
KUD_a@h # 201454.8 m

KUDvol_a <- getvolumeUD(KUD_a)

ver90_a <- getverticeshr(KUDvol_a, 90)
ver50_a <- getverticeshr(KUDvol_a, 50)
ver25_a <- getverticeshr(KUDvol_a, 25)

mapview(list(ver90_a, ver50_a, ver25_a))

ker_a <- list(ver90_a, ver50_a, ver25_a)

# YEAR 1
KUD_a_1 <- kernelUD(gls_HIV[['gls_a_1']],
                    h = 'href')
KUD_a_1@h # 233123.7 m

KUDvol_a_1 <- getvolumeUD(KUD_a_1)
ver50_a_1 <- getverticeshr(KUDvol_a_1, 50)
mapview(ver50_a_1) + mapview(gls_HIV[['gls_a_1']])

# YEAR 2
KUD_a_2 <- kernelUD(gls_HIV[['gls_a_2']],
                    h = 'href')
KUD_a_2@h # 170282.1 m

KUDvol_a_2 <- getvolumeUD(KUD_a_2)
ver50_a_2 <- getverticeshr(KUDvol_a_2, 50)
mapview(ver50_a_2) + mapview(gls_HIV[['gls_a_2']])

# YEAR 3
KUD_a_3 <- kernelUD(gls_HIV[['gls_a_3']],
                    h = 'href')
KUD_a_3@h # 187696.8 m

KUDvol_a_3 <- getvolumeUD(KUD_a_3)
ver50_a_3 <- getverticeshr(KUDvol_a_3, 50)
mapview(gls_HIV[['gls_a_3']]) + mapview(ver50_a_3)


# ------------------------------------ #
# KERNELS COMPARISONS PP vs Audrey ####
# ---------------------------------- #
mapview(list(ver50_a_1, ver50_a_2, ver50_a_3, ver50_p_1, ver50_p_2),
        col.regions = viridis(5, alpha = 0.5))


# library(rgdal)
# 
# range(gls_a_HIV@coords[, 2])
# range(hiv_a_2@coords[, 1])
# 
# range(gls_a_HIV@data$LAT)
# range(gls_a_HIV@coords[, 2])
# 
# range(gls_a_HIV@data$LON)
# range(gls_a_HIV@coords[, 1])



# 
# mapview(hiv_p,
#         cex = 2,
#         layer.name = 'GLS_2008-2009') +
#   mapview(hiv_a,
#           cex = 2,
#           col.regions = 'darkorange',
#           layer.name = 'GLS_2008-2012') +
# mapview(ker_a,
#         col.regions = viridis(length(ker_a)),
#         layer.name = c('HR90_2008-2012', 'HR50_2008-2012', 'HR25_2008-2012')) +
#   mapview(ker_p,
#           col.regions = viridis(length(ker_p)),
#           layer.name = c('HR90_2008-2009', 'HR50_2008-2009', 'HR25_2008-2009'))


# saveRDS(ker_a,
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_AD_GLS_hivernage_KERNEL905025_Audrey.rds')
# 
# saveRDS(ker_p,
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_AD_GLS_hivernage_KERNEL905025_Patrick.rds')
# 
# saveRDS(list(hiv_a, hiv_p),
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_AD_GLS_hivernage_points.rds')
