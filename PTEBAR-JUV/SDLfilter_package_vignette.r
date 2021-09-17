#####################################################################
# https://www.rdocumentation.org/packages/SDLfilter/versions/2.1.1 #
###################################################################
library(SDLfilter)
library(sf)
library(mapview)
library(lubridate)

# Loading data 
data(turtle)
names(turtle)
head(turtle)
class(turtle$DateTime)

# Visualization
turtle_sp <- st_as_sf(turtle,
                      coords = c('lon', 'lat'))
mapview(turtle_sp,
        zcol = 'id',
        burst = T)
# Remove temporal and spatial duplicates
dim(turtle)
dup <- turtle[duplicated(turtle$DateTime),]
turtle[turtle$DateTime == dup$DateTime[1],]

turtle.dup <- dupfilter(turtle)
dim(turtle.dup)

# Calculate the maximum linear speed between two consecutive locations
V <- vmax(turtle.dup)  

# Calculate the maximum one-way linear speed of a loop trip
VLP <- vmaxlp(turtle.dup) 

## Run ddfilter
turtle.dd <- ddfilter(turtle.dup,
                      vmax = V,
                      vmaxlp = VLP)

#### ---------- Application on PTEBAR data ---------- ####
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

# Name modifications for functions in package SDLfilter
names(argos)[1:5] <- c('id', 'DateTime', 'lat', 'lon', 'qi')

# Remove temporal and spatial duplicates
argos.dup <- dupfilter(argos)
dim(argos.dup)

projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
argos.dup.sp <- st_as_sf(argos.dup,
                         coords = c('lon', 'lat'),
                         crs = projcrs)
argos.dup.sp$id <- as.factor(argos.dup.sp$id)

mapview(argos.dup.sp,
        zcol = 'id',
        burst = T)

# Calculate the maximum linear speed between two consecutive locations
V <- vmax(argos.dup)  

# Calculate the maximum one-way linear speed of a loop trip
VLP <- vmaxlp(argos.dup) 

## Run ddfilter
turtle.dd <- ddfilter(turtle.dup,
                      vmax = V,
                      vmaxlp = VLP)