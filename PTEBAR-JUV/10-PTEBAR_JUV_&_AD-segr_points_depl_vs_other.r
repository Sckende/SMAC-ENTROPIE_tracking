# ----- Division des points GLS adultes et ARGOS juveniles ----- #
# ----- En fonction du deplacement ou non vers la zone d'hivernage ----- #

# ---- Load packages ------ #
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

# ------------------- #
#### GLS ADULTES #####
# ----------------- #

# utilisation des dates extraites par P. Pinet dans le papier Pinet et al 2011

#### chargement informations papier ####

pap <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/Infos_gls_tracks_Pinet_2011.txt",
                  sep = "\t",
                  h = T)
pap

#### Date conversion ####
pap$DEPARTURE <- strptime(pap$DEPARTURE,
                          "%d/%m/%Y")
pap$RETURN <- strptime(pap$RETURN,
                          "%d/%m/%Y")
pap$ARRIVAL_CORE_WINTER <- strptime(pap$ARRIVAL_CORE_WINTER,
                                    "%d/%m/%Y")
pap$DEPART_CORE_WINTER <- strptime(pap$DEPART_CORE_WINTER,
                                   "%d/%m/%Y")

#### ID list ####
pap$ID

#### Recuperation de annee de deploiement et du short ID ####

pap$ID2 <- paste(pap$YEAR,
                 substr(pap$ID,
                        1,
                        4),
                 sep = "-")

#### Chargement des donnees GLS ####

gls <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_ADULT_GLS_2008-2009_clean_from_Audrey.txt",
                  h = T,
                  sep = "\t")
head(gls)

#### Date conversion ####
gls$DATE <- strptime(gls$DATE, "%d/%m/%Y %H:%M")

#### Spatial conversion ####
gls_sp <- SpatialPointsDataFrame(coords = gls[, c("LON", "LAT")],
                                  data = gls,
                                  proj4string = CRS("+init=epsg:4326"))

gls_sf <- st_as_sf(gls_sp)

#### ID list & comparaison avec ID pap ####
unique(gls$ID)

table(substr(pap$ID,
       1,
       4))

table(substr(unique(gls$ID),
       1,
       4))

#### Recuperation de l'annee de deploiement avec annee du premier point ####
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
