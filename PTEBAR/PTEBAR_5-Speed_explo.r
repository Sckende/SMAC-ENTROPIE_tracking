# ------------------------------------------------------------------- #
# OBJ : computation of speed flight for adult PTEBAR based on GPS data
# DATA : GPS data deployed in 2018 - see : https://sckende.github.io/SMAC-ENTROPIE_tracking/page1.html
# DATA SOURCE : 'PTEBAR_4-Dist_ta_colony.r'
# ------------------------------------------------------------------ #

# Clean environment & load packages ####
# ----------------------------------- #
rm(list = ls())
require(sf)
require(dplyr)
require(lubridate)
require(mapview)

# Load & prepare data ####
# --------------------- #

gps <- read.table('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/4-PTEBAR_GPS/DATA/PTEBAR_GPS_bursts.txt',
                  h = T,
                  dec = '.',
                  sep = '\t')
head(gps)
summary(gps)

##### Time conversion
gps$time <- as.POSIXct(gps$time)
gps$date <- as.POSIXct(gps$date)

# Exploration of lag between locations for each burst ####
# ----------------------------------------------------- #

##### Selection of bursts with at least more than two rows
gps2 <- gps %>% group_by(burst) %>%
  filter(length(burst) > 2) # Only keep burst with more than two rows

##### Check
table(gps2$burst)

##### Summary per burst : number of points, third quartile for 'diff'
gps2 %>% group_by(burst) %>% 
  summarise(n = length(burst),
            min.diff = min(diff),
            quart3 = quantile(diff)[3]) %>% 
  View()

# Computation of speed ####
# ---------------------- #

##### Step 1 - Computation of distance between each locations

########### Conversion in sf object - LONLAT
gps2.sf.LONLAT <- st_as_sf(gps2,
                           coords = c('Longitude', 'Latitude'),
                           crs = 4326)
# mapview(gps2.sf.LONLAT)

########## Conversion in sf object - UTM
gps2.sf.UTM <- st_transform(gps2.sf.LONLAT,
                            crs = 32743)
# mapview(gps2.sf.UTM)

##### Step 2 - Apply on a single burst
test <- gps2.sf.UTM[gps2.sf.UTM$burst == 'PAC03-3',]

########## Distance computation
m.dist <- st_distance(test)
dia.m.dist <- diag(m.dist[, -1])

########## Speed computation
speed.m.min <- dia.m.dist/test$diff[-length(test$diff)] # m/min
speed.km.h <- speed.m.min * 0.001 * 60 # km/h

##### Step 3 - Apply on the complete data

########## Computation
gps2.sf.UTM.l <- split(gps2.sf.UTM, gps2.sf.UTM$burst)

speed.burst <- lapply(gps2.sf.UTM.l, function(x){
  # Distance
  m.dist <- st_distance(x)
  dia.m.dist <- diag(m.dist[, -1])
  # Speed
  speed.m.min <- dia.m.dist/x$diff[-length(x$diff)] # m/min
  speed.km.h <- speed.m.min * 0.001 * 60 # km/h
  # Output
  df <- data.frame(dist.m = c(as.numeric(dia.m.dist), NA), # Addition of 1 NA to have the same row number of initial db
                   speed.km.h = c(as.numeric(speed.km.h), NA), # Addition of 1 NA to have the same row number of initial db
                   burst = unique(x$burst))
  df
})

########## Merge with the initial data set

speed.l <- lapply(gps2.sf.UTM.l, function(x){
  
  burst <- unique(x$burst)
  x <- cbind(x, speed.burst[[burst]][, -3])
  x
})

########## Exploration
lapply(speed.l, function(x){
  summary(x$speed.km.h)})

########## Merge speed infos with initial db & obtain more details for PAC06-3/PAC15-4 with max speed > 100 km/h & PAC11-1/PAC15-1/PAC16-1 with max speed < 1.5 km/h

# PAC06-3
# ----- #
p063 <- speed.l[['PAC06-3']]
hist(p063$speed.km.h)
# Extract rows before and after speed > 100
out.rank <- which(p063$speed.km.h > 100) # location of values < 100
out.rank.extra <- sort(c(out.rank, out.rank - 1, out.rank + 1))

p063.out <- p063[out.rank.extra,]
p063.out


# All the outliers
# -------------- #
out.l.max <- speed.l[c('PAC06-3', 'PAC15-4')]
out.l.min <-  speed.l[c('PAC11-1', 'PAC15-1', 'PAC16-1')]

out.l.extra <- lapply(out.l.max, function(x){

  # Extract rows before and after speed > 100
  out.rank <- which(x$speed.km.h > 100) # location of values < 100
  out.rank.extra <- unique(sort(c(out.rank, out.rank - 1, out.rank + 1)))
  
  p.out <- x[out.rank.extra,]
  p.out
  
})
