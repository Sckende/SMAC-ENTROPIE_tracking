rm(list = ls())
library(sf)
library(mapview)
library(lubridate)
library(leafpop)
library(leaflet)

# --------- #
# ZEE data #
# ------- #

# Worldwide

ZEE <- st_read('C:/Users/ccjuhasz/Downloads/EEZ_land_union_v3_202003/EEZ_Land_v3_202030.shp')

mapview(ZEE)
st_crs(ZEE)

# Seychelles
terr <- unique(ZEE$TERRITORY1)
terr <- terr[order(terr)]

ZEE_Seych <- ZEE[ZEE$TERRITORY1 == 'Seychelles',]
mapview(ZEE_Seych)


# ------------------------------------------------------------------ #
#### Loading and treatment of data 2 - RAW cleaned localisations ####
# ---------------------------------------------------------------- #
argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.txt",
                    h = T,
                    sep = '\t',
                    dec = '.')
head(argos)
names(argos)[1] <- 'PTT'


# ---- Date class
argos$Date <- as.POSIXct(argos$Date,
                         format = "%Y-%m-%d %H:%M") # Date format
argos$deploy <- as.POSIXct(argos$deploy,
                           format = "%Y-%m-%d %H:%M") # Date format

# Conversion in sf Spatial Object
projLatLon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# UTM 43S corresponding to the center of the Indian ocean
# UTM 43 => 32743 
projUTM <- '+init=epsg:32743'

# Non projected spatial object
argos.sf <- st_as_sf(argos,
                     coords = c('Longitude', 'Latitude'),
                     crs = projLatLon)
class(argos.sf)

# Projected spatial object
argos.sf.UTM <- st_transform(argos.sf,
                             crs = 32743)

mapview(ZEE_Seych) + mapview(argos.sf,
                       cex = 1,
                       col.regions = 'black')

# Find points in ZEE polygons
birds_in_ZEE <- st_join(argos.sf,
                        ZEE,
                        join = st_within)
summary(birds_in_ZEE)
table(birds_in_ZEE$PTT, useNA = 'always')

# Birds in Seychelles ZEE
birds_Seych <- birds_in_ZEE[birds_in_ZEE$TERRITORY1 == 'Seychelles',]
birds_Seych <- birds_Seych[!is.na(birds_Seych$PTT),]
table(birds_Seych$PTT, useNA = 'always')

birds_Seych$PTT_fact <- as.factor(birds_Seych$PTT)

summary(birds_Seych)

birds_Seych$popup_info <- paste0("<b>PTT</b> ",
                                 birds_Seych$PTT,
                                 "<br/>",
                                 "<b>Relocation date</b>",
                                 birds_Seych$Date,
                                 "<br/>")

mapview(birds_Seych[, c(41, 40, 2, 6, 42)],
        zcol = 'PTT_fact',
        popup = popupTable(birds_Seych[, c(41, 40, 2, 6, 42)],
                           zcol = 'popup_info',
                           feature.id = FALSE,
                           row.numbers = FALSE,
                           className = 'mapview-popup'),
        # popup = birds_Seych$popup_info,
        # popup = popupTable(birds_Seych,
        #                    zcol = 'popup_info'),
        burst = TRUE) + mapview(ZEE_Seych,
                             col.regions = 'darkgrey',
                             popup = F,
                             layer.name = 'Seychelles_EEZ')
# Summary of juvenile PTEBAR in the Seychelles ZEE

birds_Seych_list <- split(birds_Seych, birds_Seych$PTT)

sum_juv_Seych <- lapply(birds_Seych_list, function(x){
  
  ID <- unique(x$PTT)
  species <- 'PTEBAR'
  status <- 'JUV'
  device <- 'ARGOS'
  
  y <- year(unique(x$deploy))
  min.d <- min(x$Date)
  max.d <- max(x$Date)
  
  r <- c(ID = as.character(ID), species = species, status = status, device = device, year = y, input = as.character(min.d), output = as.character(max.d))

}) 
sum_juv_Seych_df <- do.call('rbind', sum_juv_Seych)





















leaflet(birds_Seych)
