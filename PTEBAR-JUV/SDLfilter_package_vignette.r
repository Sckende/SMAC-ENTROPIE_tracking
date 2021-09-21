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

table(argos.dup$id)
table(argos$id)

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

###########################################################
# --------- adehabitatHR package ----------------------- #
#########################################################
rm(list=ls())
library(adehabitatHR)
library(sp)
argos <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_TRACK_argos.txt",
                    h = T,
                    sep = '\t',
                    dec = '.')

head(argos)

argos$Date <- as.POSIXct(argos$Date,
                         format = "%d/%m/%Y %H:%M") # Date format

argos$DATE_1 <- as.POSIXct(argos$DATE_1,
                           format = "%d/%m/%Y %H:%M") # Date format

# Data quality filer
unique(argos$Class)
argos.qual <- argos[argos$Class %in% c('0', '1', '2', '3', 'A', 'B'),]

# Duplicated date filter
dup_argos.qual <- argos.qual[duplicated(paste(argos.qual$Vessel, argos.qual$DateTime)),]

argos.qual.sing <- argos.qual[!duplicated(argos.qual[c(1, 2)]),] # Remove duplicated rows based on ID and DateTime
table(argos.qual.sing$Vessel)
summary(argos.qual.sing)

# Spatial Dataframe conversion
projcrs <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
argos.sp <- st_as_sf(argos.qual.sing,
                     coords = c('Longitude', 'Latitude'),
                     crs = projcrs) # step 1
class(argos.sp)


coords <- SpatialPoints(argos.qual.sing[, c('Longitude', 'Latitude')])
argos.sp2 <- SpatialPointsDataFrame(coords, argos.qual.sing)

class(argos.sp2)

# ------------------------------------------------- #
# adehabitatHR - The Minimum Convex Polygon Method #
# ----------------------------------------------- #

cp <- mcp(argos.sp2[,1], percent = 95)
class(cp)

# plot(cp)
# plot(argos.sp2, add = T)

mapview::mapview(cp,
                 zcol = 'id',
                 burst = T)

as.data.frame(cp)

# Computation of home range size for various choices of the number of extreme relocations to be excluded
x11(); mcp.area(argos.sp2[, 1],
                percent = seq(50, 100, by = 5),
                unin = 'km', # unit of original data -> c('m', 'km')
                unout = 'km2', # unit of the results -> c('ha', 'km2', 'm2')
                plotit = T)
# ------------------------------------------------------------- #
# adehabitatHR - Kernel estimatipon & utilization distribution #
# ----------------------------------------------------------- #

# Estimation of UD in each pixel of a grid superposed to the relocations
kud <- kernelUD(argos.sp2[, 1],
                h = 'href') # based on the 'reference bandwidth'
kud

x11(); image(kud)

kud1 <- kernelUD(argos.sp2[, 1],
                 h = 'LSCV') # based on the 'Least Square Cross Validation'

# WARNING!! No convergence in LSCV for:
#   [1] "162070" "162072" "162073" "166561" "166563" "166564" "166565" "166566" "166568" "166569"
# [11] "166571" "166572" "166573"
# Consider a new fit of UD using the ad hoc method for h.
kud1

x11(); image(kud1)
x11(); plotLSCV(kud1)
