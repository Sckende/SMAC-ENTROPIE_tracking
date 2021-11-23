wargos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_weimerskirch_argos_data.txt",
                     sep = '\t',
                     h = T)
wargos$PTT <- as.factor(wargos$PTT)

wargos %>% group_by(PTT) %>% count() # deletion of 162074 (2 locs), 166562 (2 locs) & 166567 (5 locs)

wargos.sp <- sf::st_as_sf(wargos,
                       coords = c('Longitude', 'Latitude'),
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # Spatial Points

lost.birds <- c('162074', '166562', '166567')

mapview(wargos.sp[wargos$PTT %in% lost.birds,],
        zcol = 'PTT',
        burst = T,
        legend = F)

# Comparaison des deux data sets - Visual inspection
# ----
wargos2 <- droplevels(wargos[!wargos$PTT %in% lost.birds,])
table(wargos$PTT)

wargos2.sp <- sf::st_as_sf(wargos2,
                          coords = c('Longitude', 'Latitude'),
                          crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# ----
RAW.argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_retrieve_RAW_data.txt",
                        sep = '\t',
                        h = T)
RAW.argos$Vessel <- as.factor(RAW.argos$Vessel)

RAW.argos %>%
  group_by(Vessel) %>%
  count()

RAW.sp <- sf::st_as_sf(RAW.argos,
                       coords = c('Longitude', 'Latitude'),
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0") # Spatial Points
# ----
pargos.sp.list <- split(RAW.sp,
                     RAW.argos$Vessel)
wargos.sp.list <- split(wargos2.sp,
                     wargos2$PTT)

plot.list <- lapply(wargos.sp.list, function(x){
  PTT <- unique(x$PTT)
  mapview(x, col.regions = 'darkgreen', alpha.regions = 0.3) + mapview(pargos.sp.list[[PTT]], col.regions = 'red', alpha.regions = 0.3) 
})

for(i in 1:length(plot.list)){
  print(plot.list[[i]])
}

#### **** INCOMPLETE DATA FROM WEIMERSKIRCH FOR LONG PATHS **** ####