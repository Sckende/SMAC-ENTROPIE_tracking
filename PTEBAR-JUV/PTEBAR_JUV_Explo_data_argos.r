# Exploration des données Argos déployées sur des juvéniles pétrel de Barau en avril 2017 et 2018
# Data à l'origine du papier de Weimerskirch et al 2016 - Wettability of juvenile plumage as a major cause of mortality threatens endangered Barau’s petrel

rm(list = ls())

library('mapview')
library('sf')
library('lubridate')
library('dplyr')

# Loading and treatment of data 1
infos_argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Infos_deploiement.txt",
                        h = T,
                        sep = "\t")
names(infos_argos)

infos_argos$deploy <- as.POSIXct(infos_argos$deploy,
                                 format = "%d/%m/%Y %H:%M") # Date format
infos_argos$start <- as.POSIXct(infos_argos$start,
                                 format = "%d/%m/%Y %H:%M") # Date format
summary(infos_argos)


# Loading and treatment of data 2
argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_TRACK_argos.txt",
                    h = T,
                    sep = '\t',
                    dec = '.')

head(argos)

argos$Date <- as.POSIXct(argos$Date,
                         format = "%d/%m/%Y %H:%M") # Date format

argos$DATE_1 <- as.POSIXct(argos$DATE_1,
                         format = "%d/%m/%Y %H:%M") # Date format

class(argos$Date)
summary(argos$Date)
barplot(table(argos$Date))
year(head(argos$Date))
table(year(argos$Date))

summary(argos)
all(is.na(argos$Latitude)) # No NA for Latitude
all(is.na(argos$Longitude)) # No NA for Longitude

y19 <- argos[year(argos$Date) == 2019,]
dim(y19)

# Summary of device recordings
head(argos)

arg_bil <- argos %>% group_by(Vessel) %>% 
  summarise(n_loc = n(),
            min_date = min(Date),
            max_date = max(Date),
            max_lat = max(Latitude),
            min_lat = min(Latitude),
            max_lon = max(Longitude),
            min_lon = min(Longitude),
            max_speed = max(Speed, na.rm = T))

no_speed <- arg_bil$Vessel[arg_bil$max_speed == '-Inf']
argos$Speed[argos$Vessel %in% no_speed]

# Device id check up - infos_argos vs. argos_location
unique(argos$Vessel) %in% infos_argos$device
vessel_no_data <- setdiff(infos_argos$device, unique(argos$Vessel)) # 3 devices with no raw data obtained
infos_argos[infos_argos$device %in% vessel_no_data,]

# Implementation of the summary dataframe
arg_bil2 <- left_join(arg_bil, infos_argos, by = c('Vessel' = 'device'))
View(arg_bil2)
names(arg_bil2)

# Dates check up - all deployment dates have to be greater than dates of device start
all(arg_bil2$deploy > arg_bil2$start) # ok
arg_bil2$deploy - arg_bil2$start # Gap in hours between the device start and the deployment

# Dates check up - device start < min date of trajectories
# **** WARNINGS **** #
all(arg_bil2$min_date > arg_bil2$start) # problem here with 3 devices displaying min_date of trajectories < start date of the device
pb_argos <- arg_bil2[arg_bil2$min_date < arg_bil2$start,]
View(pb_argos)

# Details case by case
# First device - 162072
pb_162072 <- argos[argos$Vessel == 162072,]
head(pb_162072) # Weird first row 
#---#
pb_162072$Date > arg_bil2$start[arg_bil2$Vessel == 162072] # => Only the first row is concerned

# Second device - 166566
pb_166566 <- argos[argos$Vessel == 166566,]
head(pb_166566)
#---#
pb_166566$Date > arg_bil2$start[arg_bil2$Vessel == 166566] # => Only the two first rows are concerned

# Third device - 166568
pb_166568 <- argos[argos$Vessel == 166568,]
head(pb_166568)
#---#
pb_166568$Date > arg_bil2$start[arg_bil2$Vessel == 166568] # => Only the first rows is concerned

# For each trajectories, deletion of location before the date of the device deployment
dim(argos)
argos2 <- left_join(argos, arg_bil2[, c('Vessel', 'deploy')], by = 'Vessel')
dim(argos2)

argos3 <- argos2[argos2$Date >= argos2$deploy,]
dim(argos3) # Deletion of 22 rows

# ----------------------------------------------- #
#### Rapid visual exploration of trajectories ####
# --------------------------------------------- #

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
argos_sp <- sf::st_as_sf(argos3,
                     coords = c('Longitude', 'Latitude'),
                     crs = projcrs)

argos_sp$Vessel <- as.factor(argos_sp$Vessel)

track_lines <- argos_sp %>%
    group_by(Vessel) %>% 
    summarize(do_union = FALSE) %>%
    st_cast("LINESTRING") # Creation of SF LINESTRINGS



mapview(argos_sp,
        zcol = 'Vessel',
        burst = T,
        homebutton = F) 
# + 
  mapview(track_lines,
          zcol = 'Vessel',
          burst = T,
          homebutton = F)

# ------------------------------------------------------- #
#### Rapid visual exploration of trajectories on land ####
# ----------------------------------------------------- #
  run <- st_read("C:/Users/ccjuhasz/Desktop/SMAC/SPATIAL_data_RUN/Admin/REU_adm0.shp")

  in_run <- st_intersection(argos_sp, run)
  
  mapview(in_run,
          zcol = 'Vessel',
          burst = T,
          homebutton = F)
  # mapview(track_lines[track_lines$Vessel == '166568',]) # Back and forth from Reunion Island before to go toward Tanzania
  