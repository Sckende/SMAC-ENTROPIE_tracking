# ----- Division des points GLS adultes et ARGOS juveniles ----- #
# ----- En fonction du deplacement ou non vers la zone d'hivernage ----- #

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
#             "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_2008-2009_MIGRATION_BEHAV.txt")
