# ----- Division des points GLS adultes et ARGOS juveniles ----- #
# ----- En fonction du deplacement ou non vers la zone d'hivernage ----- #
rm(list = ls())
# ---- Load packages ------ #
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

# ------------------- #
#### GLS ADULTES #####
# ----------------- #

# utilisation des dates extraites par P. Pinet dans le papier Pinet et al 2011

#### chargement informations papier ####
# ------------------------------------ #

pap <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/Infos_gls_tracks_Pinet_2011.txt",
                  sep = "\t",
                  h = T)
pap

#### Date conversion ####
# --------------------- #
pap$DEPARTURE <- strptime(pap$DEPARTURE,
                          "%d/%m/%Y")
pap$RETURN <- strptime(pap$RETURN,
                          "%d/%m/%Y")
pap$ARRIVAL_CORE_WINTER <- strptime(pap$ARRIVAL_CORE_WINTER,
                                    "%d/%m/%Y")
pap$DEPART_CORE_WINTER <- strptime(pap$DEPART_CORE_WINTER,
                                   "%d/%m/%Y")

#### ID list ####
# ------------- #
pap$ID

#### Recuperation de annee de deploiement et du short ID ####
# --------------------------------------------------------- #
pap$ID2 <- paste(pap$YEAR,
                 substr(pap$ID,
                        1,
                        4),
                 sep = "-")

#### Chargement des donnees GLS ####
# -------------------------------- #

gls <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_2008-2009_clean_from_Audrey.txt",
                  h = T,
                  sep = "\t")
head(gls)

#### Date conversion ####
# --------------------- #
gls$DATE <- strptime(gls$DATE, "%d/%m/%Y %H:%M")

#### Spatial conversion ####
# ----------------------- #
gls_sp <- SpatialPointsDataFrame(coords = gls[, c("LON", "LAT")],
                                  data = gls,
                                  proj4string = CRS("+init=epsg:4326"))

gls_sf <- st_as_sf(gls_sp)

#### ID list & comparaison avec ID pap ####
# --------------------------------------- #
unique(gls$ID)

table(substr(pap$ID,
       1,
       4))

table(substr(unique(gls$ID),
       1,
       4))

#### Recuperation de l'annee de deploiement avec annee du premier point ####
# ------------------------------------------------------------------------ #
gls_ls <- split(gls,
                gls$ID)
str(gls_ls)

gls_ls1 <- lapply(gls_ls, function(x) {
    y <- year(x$DATE[1])
    x$DEPLOI <- y
    
    id <- substr(unique(x$ID),
                 1,
                 4)
    x$ID2 <- paste(y,
                   id,
                   sep = "-")
    
    x    
})
head(gls_ls1[[1]])

gls2 <- do.call("rbind",
                gls_ls1)

head(gls2)

#### Segregation des points de gls ####
# ----------------------------------- #
# apres comparaison des donnees, retrait des balises
# 2008-8099
# 2008-8103
# 2008-8116
# 2008-8124
# 2008-8127
# 2008-8130
# 2008-9137
# 2008-9160

test <- gls_ls1[[1]]
test

arriv <- pap$ARRIVAL_CORE_WINTER[pap$ID2 == unique(test$ID2)]
dep <- pap$DEPART_CORE_WINTER[pap$ID2 == unique(test$ID2)]
test$WIN_BEHAV[test$DATE < arriv] <- "GO"
test$WIN_BEHAV[test$DATE > dep] <- "BACK"
test$WIN_BEHAV[test$DATE <= dep & test$DATE >= arriv] <- "STOP"

table(test$WIN_BEHAV)

my_col <- viridis(3)
plot(test$LON,
     test$LAT,
     col = my_col[as.integer(as.factor(test$WIN_BEHAV))])

gls3 <- gls2[!gls2$ID2 %in% c("2008-8099",
                             "2008-8103",
                             "2008-8116",
                             "2008-8124",
                             "2008-8127",
                             "2008-8130",
                             "2008-9137",
                             "2008-9160"), ]
gls_ls2 <- split(gls3,
                 gls3$ID2)

gls_ls3 <- lapply(gls_ls2,
                  function(x) {
                      arriv <- pap$ARRIVAL_CORE_WINTER[pap$ID2 == unique(x$ID2)]
                      dep <- pap$DEPART_CORE_WINTER[pap$ID2 == unique(x$ID2)]
                      x$WIN_BEHAV[x$DATE < arriv] <- "GO"
                      x$WIN_BEHAV[x$DATE > dep] <- "BACK"
                      x$WIN_BEHAV[x$DATE <= dep & x$DATE >= arriv] <- "STOP"
                      
                      print(unique(x$ID2))
                      print(table(x$WIN_BEHAV, useNA = "always"))
                      
                      x
                  })
# Conversion en DF
gls_behav <- do.call("rbind", gls_ls3)

# Exploration via la liste
lapply(gls_ls3,
       function(x) {
           x11()
           plot(x$LON,
                x$LAT,
                col = my_col[as.integer(as.factor(x$WIN_BEHAV))],
                main = unique(x$ID2))
       })

# Suite à l'exploration, probleme avec les balises
# 2008-8094
# 2009-8123
# 2009-8105
# 2009-8102
# 2009-8095

# ----- Exploration des behavs par oiseaux ----- #
# ---------------------------------------------- #

# ----- 2008 ----- #
gls2008 <- gls_behav[gls_behav$DEPLOI == 2008, ]
gls2008_ls <- split(gls2008,
                    gls2008$ID2)
x11(); par(mfrow = c(3, 4))
lapply(gls2008_ls,
       function(x) {
           plot(x$LON,
                x$LAT,
                col = my_col[as.integer(as.factor(x$WIN_BEHAV))],
                main = unique(x$ID2),
                bty = "n",
                xlim = c(30, 115),
                ylim = c(-35, -5))
           
           x_sp <- SpatialPointsDataFrame(coords = x[, c("LON", "LAT")],
                                  data = x,
                                  proj4string = CRS("+init=epsg:4326"))

KUD_p_href <- kernelUD(x_sp,
                h = 'href' # ici 1 degré (relié au type de projection, si lat/lon (non projeté) = degré, si UTM (projeté) = m) en rapport à la précision des GLS env. 180km (1deg = 111km) - pour ARGOS précision environ 1km, donc 1/100 degré
                #grid = 500
                )# ici correspond 500x500 degrés (1deg = 111 km à l'équateur)


KUDvol_p_href <- getvolumeUD(KUD_p_href)
ver50_p_href <- getverticeshr(KUDvol_p_href, 50)

plot(ver50_p_href, add = T)
           
       })

# ----- 2009 ----- #

gls2009 <- gls_behav[gls_behav$DEPLOI == 2009, ]
gls2009_ls <- split(gls2009,
                    gls2009$ID2)
x11(); par(mfrow = c(3, 4))
lapply(gls2009_ls,
       function(x) {
           plot(x$LON,
                x$LAT,
                col = my_col[as.integer(as.factor(x$WIN_BEHAV))],
                main = unique(x$ID2),
                bty = "n",
                xlim = c(30, 115),
                ylim = c(-35, -5))
           
           x_sp <- SpatialPointsDataFrame(coords = x[, c("LON", "LAT")],
                                  data = x,
                                  proj4string = CRS("+init=epsg:4326"))

KUD_p_href <- kernelUD(x_sp,
                h = "href" # ici 1 degré (relié au type de projection, si lat/lon (non projeté) = degré, si UTM (projeté) = m) en rapport à la précision des GLS env. 180km (1deg = 111km) - pour ARGOS précision environ 1km, donc 1/100 degré
                #grid = 500
                )# ici correspond 500x500 degrés (1deg = 111 km à l'équateur)


KUDvol_p_href <- getvolumeUD(KUD_p_href)
ver50_p_href <- getverticeshr(KUDvol_p_href, 50)
ver95_p_href <- getverticeshr(KUDvol_p_href, 95)
plot(ver50_p_href, add = T)
plot(ver95_p_href, add = T)
       })

# ----- Recuperation des dates à la main pour les balises abberantes ----- #
# ------------------------------------------------------------------------ #
test <- gls3[gls3$ID2 == "2009-8095", ]
head(test)
test_sp <- SpatialPointsDataFrame(coords = test[, c("LON", "LAT")],
                                  data = test,
                                  proj4string = CRS("+init=epsg:4326"))

KUD_p_href <- kernelUD(test_sp,
                h = 'href' # ici 1 degré (relié au type de projection, si lat/lon (non projeté) = degré, si UTM (projeté) = m) en rapport à la précision des GLS env. 180km (1deg = 111km) - pour ARGOS précision environ 1km, donc 1/100 degré
                #grid = 500
                )# ici correspond 500x500 degrés (1deg = 111 km à l'équateur)


KUDvol_p_href <- getvolumeUD(KUD_p_href)
# ver90_p_href <- getverticeshr(KUDvol_p_href, 90)
ver50_p_href <- getverticeshr(KUDvol_p_href, 50)
# ver25_p_href <- getverticeshr(KUDvol_p_href, 25)

x11()
plot(test$LON,
     test$LAT,
     type = "l")
# plot(ver50_p_href, add = T)

mapview(list(test_sp,
             ver50_p_href,
             test_sp[305, ]))

test[56,]
test[304,]

# ID2 | date entree HR50 | date sortie HR50
# 2008-8094 | 15/04/2008 | 27/08/2008
# 2009-8123 | 13/04/2009 | 21/08/2009
# 2009-8105 | 23/04/2009 | 23/08/2009
# 2009-8102 | 07/05/2009 | 01/09/2009
# 2009-8095 | 17/04/2009 | 19/08/2009

# ----- Modfification des metadonnees du ms Pinet et al 2011 ---- #
# --------------------------------------------------------------- #
modif <- data.frame(ID2 = c("2008-8094",
                            "2009-8123",
                            "2009-8105",
                            "2009-8102",
                            "2009-8095"),
                    ARRIVAL_CORE_WINTER = strptime(c("15/04/2008",
                                                     "13/04/2009",
                                                     "23/04/2009",
                                                     "07/05/2009",
                                                     "17/04/2009"),
                                                   "%d/%m/%Y"),
                    DEPART_CORE_WINTER = strptime(c("27/08/2008",
                                                    "21/08/2009",
                                                    "23/08/2009",
                                                    "01/09/2009",
                                                    "19/08/2009"),
                                                  "%d/%m/%Y"))
modif
modif <- modif[order(modif$ID2),]
# ----- #
pap <- pap[order(pap$ID2),]
pap$ARRIVAL_CORE_WINTER[pap$ID2 %in% c("2008-8094",
                                       "2009-8123",
                                       "2009-8105",
                                       "2009-8102",
                                       "2009-8095")] <- modif$ARRIVAL_CORE_WINTER

pap$DEPART_CORE_WINTER[pap$ID2 %in% c("2008-8094",
                                       "2009-8123",
                                       "2009-8105",
                                       "2009-8102",
                                       "2009-8095")] <- modif$DEPART_CORE_WINTER

pap[pap$ID2 %in% c("2008-8094",
                                       "2009-8123",
                                       "2009-8105",
                                       "2009-8102",
                                       "2009-8095"), ]

# ----- Nouvelle segregation de spoints par comportements ADULTES ----- #
# --------------------------------------------------------------------- #

gls_ls2 <- split(gls3,
                 gls3$ID2)

gls_ls3 <- lapply(gls_ls2,
                  function(x) {
                      arriv <- as.Date(pap$ARRIVAL_CORE_WINTER[pap$ID2 == unique(x$ID2)])
                      dep <- as.Date(pap$DEPART_CORE_WINTER[pap$ID2 == unique(x$ID2)])
                      x$WIN_BEHAV[as.Date(x$DATE) < arriv] <- "GO"
                      x$WIN_BEHAV[as.Date(x$DATE) > dep] <- "BACK"
                      x$WIN_BEHAV[as.Date(x$DATE) <= dep & as.Date(x$DATE) >= arriv] <- "STOP" # dep + 1 a cause des heures
                      
                      print(unique(x$ID2))
                      print(table(x$WIN_BEHAV, useNA = "always"))
                      
                      x
                  })
# Conversion en DF
gls_behav <- do.call("rbind", gls_ls3)

# Exploration via la liste
lapply(gls_ls3,
       function(x) {
           x11()
           plot(x$LON,
                x$LAT,
                col = my_col[as.integer(as.factor(x$WIN_BEHAV))],
                main = unique(x$ID2))
       })

# verif 1
plot(gls_behav$LON[gls_behav$ID2 == "2008-8094"],
     gls_behav$LAT[gls_behav$ID2 == "2008-8094"],
     col = my_col[as.integer(as.factor(gls_behav$WIN_BEHAV[gls_behav$ID2 == "2008-8094"]))],
     main = "2008-8094")

# verif 2
plot(gls_behav$LON[gls_behav$ID2 == "2009-8123"],
     gls_behav$LAT[gls_behav$ID2 == "2009-8123"],
     col = my_col[as.integer(as.factor(gls_behav$WIN_BEHAV[gls_behav$ID2 == "2009-8123"]))],
     main = "2009-8123")

gls_behav$DATE[gls_behav$WIN_BEHAV == "STOP" & gls_behav$ID2 == "2009-8123"]

# verif 3
plot(gls_behav$LON[gls_behav$ID2 == "2009-8105"],
     gls_behav$LAT[gls_behav$ID2 == "2009-8105"],
     col = my_col[as.integer(as.factor(gls_behav$WIN_BEHAV[gls_behav$ID2 == "2009-8105"]))],
     main = "2009-8105")

gls_behav$DATE[gls_behav$WIN_BEHAV == "STOP" & gls_behav$ID2 == "2009-8105"]

# verif 4
plot(gls_behav$LON[gls_behav$ID2 == "2009-8095"],
     gls_behav$LAT[gls_behav$ID2 == "2009-8095"],
     col = my_col[as.integer(as.factor(gls_behav$WIN_BEHAV[gls_behav$ID2 == "2009-8095"]))],
     main = "2009-8095")

gls_behav$DATE[gls_behav$WIN_BEHAV == "STOP" & gls_behav$ID2 == "2009-8095"]

# ----- Nouveau fichier de GLS avec comportements ----- #
# ----------------------------------------------------- #

# write.table(gls_behav,
#             "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_2008-2009_MIGRATION_BEHAV.txt",
#             sep = "\t")

# ----- kernel des adultes en zone hivernage ----- #
# ------------------------------------------------ #

gls_behav_sp <- SpatialPointsDataFrame(coords = gls_behav[, c("LON", "LAT")],
                                  data = gls_behav,
                                  proj4string = CRS("+init=epsg:4326"))

winter_sp <- gls_behav_sp[gls_behav_sp$WIN_BEHAV == "STOP",]
KUD_p_href <- kernelUD(winter_sp,
                h = 'href' # ici 1 degré (relié au type de projection, si lat/lon (non projeté) = degré, si UTM (projeté) = m) en rapport à la précision des GLS env. 180km (1deg = 111km) - pour ARGOS précision environ 1km, donc 1/100 degré
                #grid = 500
                )# ici correspond 500x500 degrés (1deg = 111 km à l'équateur)


KUDvol_p_href <- getvolumeUD(KUD_p_href)
winHR90 <- getverticeshr(KUDvol_p_href, 90)
winHR50 <- getverticeshr(KUDvol_p_href, 50)
winHR25 <- getverticeshr(KUDvol_p_href, 25)

mapview(list(winHR90,
             winHR50,
             winHR25))

# ----- Chargement des donnees ARGOS JUV ----- #
# -------------------------------------------- #

# argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_110max.rds")
names(argos)[c(1,3:4)] <- c("Vessel", "Longitude", "Latitude")
argos <- argos[argos$Vessel %in% c("162070",
                                   "162072",
                                   "162073",
                                   "166561",
                                   "166563",
                                   "166565",
                                   "166564"),] # deploiement 2018

argos_sp <- SpatialPointsDataFrame(coords = argos[, c("Longitude", "Latitude")],
                                  data = argos,
                                  proj4string = CRS("+init=epsg:4326"))

head(argos_sp)

# ----- Superposition kernels ADULTES et trajets JUV ----- #
# -------------------------------------------------------- #
mapview(argos_sp,
        zcol = "Vessel",
        burst = T) + mapview(list(winHR90,
                                  winHR50,
                                  winHR25))

# Vessel | HR95 | HR50 | HR25
# 162072 |  X   |      |
# 162073 |  X   |      |
# 166561 |  X   |  X   |
# 166563 |  X   |  X   |  X
# 166565 |  X   |  X   |  X
# 166572 |  X   |      |
# ----- Subset des JUV a kernel adultes ----- #
# ------------------------------------------- #

juv_k <- argos_sp[argos_sp$Vessel %in% c("162072",
                                         "162073",
                                         "166561",
                                         "166563",
                                         "166565",
                                         "166572"), ]

# ----- Retrait du début du parcours pour le calcul des kernels 
# consideration a partir de debut juin

table(juv_k$Vessel)
juv_k$Vessel <- droplevels(juv_k$Vessel)
summary(juv_k)
# juv_k_2 <- juv_k[month(juv_k$Date) > 5, ]

juv_k_ls <- split(juv_k,
                  juv_k$Vessel)

x11(); par(mfrow = c(2, 3))
lapply(juv_k_ls, function(x) {
  KUD_p_href <- kernelUD(x,
                h = 'href')
  KUDvol_p_href <- getvolumeUD(KUD_p_href)
#   HR90 <- getverticeshr(KUDvol_p_href, 90)
  HR50 <- getverticeshr(KUDvol_p_href, 50)
  HR25 <- getverticeshr(KUDvol_p_href, 25)

  plot(x,
       main = unique(x$Vessel),
       xlim = c(40, 120),
       ylim = c(-40, 10))
#   plot(HR90,
#        add = T)
  plot(HR50,
       add = T,
       col = "darkorange")
  plot(HR25,
       add = T,
       col = "darkred")
  axis(1)
  axis(2)
  })

# ----- Cas par cas ----- #
# 162072
juv <- juv_k[juv_k$Vessel == "162072", ]
# -----
KUD_href <- kernelUD(juv,
                     # h = 1
                     h = "href")
KUDvol_href <- getvolumeUD(KUD_href)
HR90 <- getverticeshr(KUDvol_href, 90)
HR50 <- getverticeshr(KUDvol_href, 50)
HR25 <- getverticeshr(KUDvol_href, 25)
# -----
mapview(list(juv,
             HR90,
             HR50,
             HR25)) + mapview(list(winHR50,
                                   winHR90),
                              col.regions = "darkorange")
juv[306, ]

# 162073
juv <- juv_k[juv_k$Vessel == "162073", ]
# -----
KUD_href <- kernelUD(juv,
                     # h = 1
                     h = "href")
KUDvol_href <- getvolumeUD(KUD_href)
HR90 <- getverticeshr(KUDvol_href, 90)
HR50 <- getverticeshr(KUDvol_href, 50)
HR25 <- getverticeshr(KUDvol_href, 25)
# -----
mapview(list(juv,
             HR90,
             HR50,
             HR25)) + mapview(list(winHR50,
                                   winHR90),
                              col.regions = "darkorange")

# 166561
juv <- juv_k[juv_k$Vessel == "166561", ]
# -----
KUD_href <- kernelUD(juv,
                     # h = 1
                     h = "href")
KUDvol_href <- getvolumeUD(KUD_href)
HR90 <- getverticeshr(KUDvol_href, 90)
HR50 <- getverticeshr(KUDvol_href, 50)
HR25 <- getverticeshr(KUDvol_href, 25)
# -----
mapview(list(juv,
             HR90,
             HR50,
             HR25)) + mapview(list(winHR50,
                                   winHR90),
                              col.regions = "darkorange")

# 166563
juv <- juv_k[juv_k$Vessel == "166563", ]
# -----
KUD_href <- kernelUD(juv,
                     # h = 1
                     h = "href")
KUDvol_href <- getvolumeUD(KUD_href)
HR90 <- getverticeshr(KUDvol_href, 90)
HR50 <- getverticeshr(KUDvol_href, 50)
HR25 <- getverticeshr(KUDvol_href, 25)
# -----
mapview(list(juv,
             HR90,
             HR50,
             HR25)) + mapview(list(winHR50,
                                   winHR90),
                              col.regions = "darkorange")

# 166565
juv <- juv_k[juv_k$Vessel == "166565", ]

juv_line <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_Spatial_tracks_ARGOS.rds')
juv_line <- juv_line[juv_line$PTT == "166565",]
# -----
KUD_href <- kernelUD(juv,
                     # h = 1
                     h = "href")
KUDvol_href <- getvolumeUD(KUD_href)
HR90 <- getverticeshr(KUDvol_href, 90)
HR50 <- getverticeshr(KUDvol_href, 50)
HR25 <- getverticeshr(KUDvol_href, 25)
# -----
mapview(list(juv,
             HR90,
             HR50,
             HR25)) + mapview(list(winHR50,
                                   winHR90),
                              col.regions = "darkorange") + mapview(juv_line)

# 166572
juv <- juv_k[juv_k$Vessel == "166572", ]

juv_line <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_Spatial_tracks_ARGOS.rds')
juv_line <- juv_line[juv_line$PTT == "166572",]
# -----
KUD_href <- kernelUD(juv,
                     # h = 1
                     h = "href")
KUDvol_href <- getvolumeUD(KUD_href)
HR90 <- getverticeshr(KUDvol_href, 90)
HR50 <- getverticeshr(KUDvol_href, 50)
HR25 <- getverticeshr(KUDvol_href, 25)
# -----
mapview(list(juv,
             HR90,
             HR50,
             HR25)) + mapview(list(winHR50,
                                   winHR90),
                              col.regions = "darkorange") + mapview(juv_line)

# ==> retrait de l'oiseau de 2017 (166572), trop atypique
# Vessel | date entree HR50 | date sortie HR50
# 162072 |    18/07/2018    |        x
# 162073 |    10/07/2018    |        x
# 166561 |    04/06/2018    |    20/11/2018
# 166563 |    30/06/2018    |    21/10/2018
# 166565 |    23/05/2018    |    21/11/2018

# ----- Segregation IMPARFAITE des track de juveniles ----- #
# --------------------------------------------------------- #

argos_sp2 <- argos_sp[argos_sp$Vessel %in% c("162072",
                                             "162073",
                                             "166561",
                                             "166563",
                                             "166565"), ]
table(argos_sp2$Vessel)
argos_sp2$Vessel <- droplevels(argos_sp2$Vessel)

# ----- #
infos <- data.frame(Vessel = c("162072",
                               "162073",
                               "166561",
                               "166563",
                               "166565"),
                    enter_HR50 = as.Date(c("18/07/2018",
                                           "10/07/2018",
                                           "04/06/2018",
                                           "30/06/2018",
                                           "23/05/2018"),
                                         "%d/%m/%Y"),
                    exit_HR50 = c(NA, NA, "20/11/2018",
                                                    "21/10/2018",
                                                    "21/11/2018"))
infos$exit_HR50 <- as.Date(infos$exit_HR50,
                           "%d/%m/%Y")

# ----- segregation #

argos_sp2_ls <- split(argos_sp2,
                      argos_sp2$Vessel)

juv_behav <- lapply(argos_sp2_ls, function(x) {
     id <- unique(x$Vessel)
     date_enter <- infos$enter_HR50[infos$Vessel == id]
     date_exit <- infos$exit_HR50[infos$Vessel == id]
     
     if (is.na(date_exit)) {
          x$behav[as.Date(x$Date) >= date_enter] <- "STOP"
          x$behav[as.Date(x$Date) < date_enter] <- "MOVE"
          } else {
               x$behav[as.Date(x$Date) >= date_enter & as.Date(x$Date) <= date_exit] <- "STOP"
               x$behav[as.Date(x$Date) < date_enter | as.Date(x$Date) > date_exit] <- "MOVE"
          }
     x
})

# ----- visualisation #

x11(); par(mfrow = c(2, 3))
lapply(juv_behav, function(x) {
     plot(x,
          col = my_col[as.integer(as.factor(x$behav))],
          main = unique(x$Vessel))
     
     KUD_href <- kernelUD(x[, c("Longitude", "Latitude")],
                     # h = 1
                     h = "href")
     KUDvol_href <- getvolumeUD(KUD_href)
     HR50 <- getverticeshr(KUDvol_href, 50)
     plot(HR50, add = T)
}) # ==> Verificatio OK

# ----- enregistrement du fichier #
# write.table(do.call("rbind", juv_behav),
#             "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_ARGOS_MIGRATION_BEHAV.txt",
#             sep = "\t")
#### --------------------- PARTIE II ------------------------------- ####
##### Carto de la chlorophylle avec les donnees adultes & juveniles #####
# --------------------------------------------------------------------- #

# Production des deux cartes
# periode defavorable = NDJFMA = REPRO & periode favorable = MJJASO = HIVERNAGE
# moyenne de 2008 a 2019 pour incllure la periode de donnees AD & JUV
rm(list = ls())
# ----- data #
chlo <- terra::rast(c("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/chlo/dataset-oc-glo-chl-multi_cci-l4-chl_4km_monthly-rep-v02_1654077104001.nc",
                         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/chlo/dataset-oc-glo-chl-multi_cci-l4-chl_4km_monthly-rep-v02_1654077266568.nc",
                         "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/chlo/dataset-oc-glo-chl-multi_cci-l4-chl_4km_monthly-rep-v02_1654077502137.nc"))

ad_behav <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_2008-2009_MIGRATION_BEHAV.txt",
                       sep = "\t",
                       header = T)
head(ad_behav)
ad_behav$DATE <- as.Date(ad_behav$DATE)
ad_behav_sp <- SpatialPointsDataFrame(coords = ad_behav[, c("LON", "LAT")],
                                  data = ad_behav,
                                  proj4string = CRS("+init=epsg:4326"))

juv_behav <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_ARGOS_MIGRATION_BEHAV.txt",
                       sep = "\t",
                       header = T)
head(juv_behav)
juv_behav$Date <- as.Date(juv_behav$Date)
juv_behav_sp <- SpatialPointsDataFrame(coords = juv_behav[, c("Longitude", "Latitude")],
                                  data = juv_behav,
                                  proj4string = CRS("+init=epsg:4326"))



# ----- periode favo = "WINTER" #
chlo_fav <- chlo[[month(time(chlo)) %in% 5:10]]
chlo_fav
time(chlo_fav)
chlo_fav_sg <- mean(chlo_fav, na.rm = T)
# values(chlo_fav_sg)[values(chlo_fav_sg) > 1] <- NA 

# ----- periode defav = "REPRO" #
chlo_defav <- chlo[[month(time(chlo)) %in% c(1:4, 11:12)]]
chlo_defav
time(chlo_defav)
chlo_defav_sg <- mean(chlo_defav, na.rm = T)
summary(values(chlo_defav_sg))
# values(chlo_defav_sg)[values(chlo_defav_sg) > 1] <- NA 

# ----- diff entre fav et defav ----- #
chlo_diff_sg <- chlo_fav_sg - chlo_defav_sg

log_chlo_diff <- log(chlo_diff_sg)
x11(); par(mfrow = c(1, 2))
plot(chlo_diff_sg)
plot(log_chlo_diff)

# writeRaster(log_chlo_diff,
#             filename = "C:/Users/ccjuhasz/Desktop/log_diff_chloA.tif")
# writeRaster(chlo_diff_sg,
#             filename = "C:/Users/ccjuhasz/Desktop/diff_chloA.tif")
# ----- #
nlev <- 100
ran <- range(values(c(log(chlo_defav_sg),
                      log(chlo_fav_sg))), na.rm = T)
my_at <- seq(from = ran[1],
             to = ran[2],
          #    length.out = nlev + 1
             length.out = 5)
my_cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)
jet_colors <- colorRampPalette(c("#00007F",
                                  "blue",
                                  "#007FFF",
                                  "cyan",
                                  "#7FFF7F",
                                  "yellow",
                                  "#FF7F00",
                                  "red",
                                  "#7F0000"))
mycolorkey <- list(labels = list(labels = round(exp(my_at), digit = 3),
                                 at = my_at))
x11()
levelplot(log(c(chlo_defav_sg, chlo_fav_sg)),
          main = c("NDJFMA - defav - repro", "MJJASO - fav - hivernage"),
          names.attr = c("", ""),
          col.regions = jet_colors,
          cuts = 99,
          at = seq(ran[1], ran[2], length.out = 100),
          colorkey = mycolorkey
          # labels = list(exp(my_at))
          # zscaleLog = "e"
          # zscaleLog = TRUE
          ) +
layer(sp.polygons(ne_countries())) # ==> NICE PLOT

# ----- utilisation des quantiles pour l'échelle de couleurs -----#
# Tentative de reproduction de la carte de QGis #
# --------------------------------------------- #
quan <- quantile(values(chlo_fav_sg),
         na.rm = T,
         probs = seq(0, 1, 0.10))
my_at <- quan
my_cols <- colorRampPalette((RColorBrewer::brewer.pal(9,
                                    "Greens")))(99)
mycolorkey <- list(labels = list(labels = round(my_at, digit = 3),
                                 at = my_at))
x11()
# png("C:/Users/ccjuhasz/Desktop/FAV_chloA.png")
levelplot(chlo_fav_sg,
          main = "MJJASO - fav - hivernage",
          col.regions = my_cols,
          cuts = 100,
          at = quan,
          colorkey = mycolorkey,
          maxpixels = 1e7
          ) +
layer(sp.polygons(ne_countries()))
dev.off()
# file.show("C:/Users/ccjuhasz/Desktop/FAV_chloA.png")
# writeRaster(log(chlo_defav_sg),
#             filename = "C:/Users/ccjuhasz/Desktop/log_chloA_DEFAV.tif")
# writeRaster(chlo_defav_sg,
#             filename = "C:/Users/ccjuhasz/Desktop/chloA_DEFAV.tif")
# writeRaster(log(chlo_fav_sg),
#             filename = "C:/Users/ccjuhasz/Desktop/log_chloA_FAV.tif")
# writeRaster(chlo_fav_sg,
#             filename = "C:/Users/ccjuhasz/Desktop/chloA_FAV.tif")
rrr <- c(chlo_defav_sg, chlo_fav_sg)
plot(log(rrr, at = my_at))
x11()
levelplot(log(chlo_fav_sg, base = 10),
          main = "MJJASO - fav",
          col.regions = jet_colors,
          cuts = nlev - 1,
          at = my_at)
          # zscaleLog = "e") 
          +
layer(sp.polygons(ne_countries()))

# ----- ajout des kernel adultes et des points juv
# ----- periode fav #
table(month(ad_behav$DATE[ad_behav$WIN_BEHAV == "STOP"]))
table(month(ad_behav$DATE[ad_behav$WIN_BEHAV %in% c("BACK", "GO")]))

table(month(juv_behav$Date[juv_behav$behav == "STOP"]))
table(month(juv_behav$Date[juv_behav$behav == "MOVE"]))

ad_stop <- ad_behav_sp[ad_behav_sp$WIN_BEHAV == "STOP", ]
KUD_href <- kernelUD(ad_stop,
                     # h = 1
                     h = "href")
KUDvol_href <- getvolumeUD(KUD_href)
HR50_ad_stop <- getverticeshr(KUDvol_href, 50)

juv_stop <- juv_behav_sp[juv_behav_sp$behav == "STOP", ]
# ----- #
x11()
levelplot(log(chlo_fav_sg),
          main = "MJJASON - fav",
          col.regions = jet_colors,
          cuts = nlev - 1,
          at = my_at) +
layer(sp.polygons(ne_countries())) + 
layer(sp.polygons(HR50_ad_stop)) +
layer(sp.points(juv_stop,
                col = rgb(0, 0, 1, alpha = 0.2),
                lwd = 1))

# writeOGR(HR50_ad_stop,
#          dsn = "C:/Users/ccjuhasz/Desktop/shapefile_HR50_ad_stop",
#          layer = "HR50_ad_stop",
#          driver = "ESRI Shapefile")

# ----- ajout des kernel adultes/STOP et des kernel juv/STOP
# ----- periode fav #
KUD_href <- kernelUD(juv_stop,
                     # h = 1
                     h = "href")
KUDvol_href <- getvolumeUD(KUD_href)
HR50_juv_stop <- getverticeshr(KUDvol_href, 50)
# ----- #
x11()
levelplot(log(chlo_fav_sg),
          main = "MJJASON - fav",
          col.regions = jet_colors,
          cuts = nlev - 1,
          at = my_at) +
layer(sp.polygons(ne_countries())) + 
layer(sp.polygons(HR50_ad_stop)) +
layer(sp.polygons(HR50_juv_stop, col = "grey"))

# writeOGR(HR50_juv_stop,
#          dsn = "C:/Users/ccjuhasz/Desktop/shapefile_HR50_juv_stop",
#          layer = "HR50_juv_stop",
#          driver = "ESRI Shapefile")

# ----- Ajout des points AD & JUV lors des dplts 
# ----- Periode defav
ad_move <- ad_behav_sp[ad_behav_sp$WIN_BEHAV %in% c("GO", "BACK"), ]
juv_move <- juv_behav_sp[juv_behav_sp$behav == "MOVE", ]
x11()
levelplot(log(chlo_defav_sg),
          main = "NDJFMA - defav",
          col.regions = jet_colors,
          cuts = nlev - 1,
          at = my_at) +
layer(sp.polygons(ne_countries())) +
layer(sp.points(ad_behav_sp[ad_behav_sp$WIN_BEHAV %in% c("GO", "BACK"), ],
                col = c("red", "blue")[as.numeric(as.factor(ad_behav_sp$WIN_BEHAV[ad_behav_sp$WIN_BEHAV %in% c("GO", "BACK")]))],
                pch = 20,
                cex = 0.5)) +
layer(sp.points(juv_behav_sp[juv_behav_sp$behav == "MOVE", ], col = "grey", pch = 20, cex = 0.5))