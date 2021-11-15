# --------------------------------------------------------------- #
# For reminder --> point.group correspond to burst of point separated  by at least 10 consecutive hours - based on the setting of ARGOS device
# script where computation of point.group is done --> PTEBAR_JUV_Explo_data_argos_3.r
# --------------------------------------------------------------- #
require(mapview)
require(lubridate)
require(sf)

argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.rds")
head(argos)
summary(argos[['166566']])

# COMPUTATION exploration on one device first ####

dev <- argos[['166566']]
summary(dev)
head(dev)
dev$point.group.fact <- as.factor(dev$point.group)

del <- dev[dev$delay <= -10,]

require(mapview)
### Visualization of point bursts ####
mapview(dev,
        zcol = 'point.group.fact',
        burst = T,
        popup = 'Date')

### Point count per group ####
barplot(table(dev$point.group.fact),
        xlab = 'Burst number',
        ylab = 'Number of relocs',
        main = 'ARGOS 166566')
burst.count <- as.data.frame(table(dev$point.group.fact))
names(burst.count) <- c('burst', 'count')


### Frequence in delay <= - 10 hours ####
td <- table(round(del$delay)*-1)
barplot(td,
        xlab = 'Delay between burst of points (hour)',
        ylab = 'Occurence')

### Speed between each relocs for each burst ####
dev.list <- split(dev, dev$point.group.fact)

dev.list <- lapply(dev.list, function(x){
  # Distance matrice
  matr <- st_distance(x)
  time <- difftime(c(x$Date[-1], NA),
                   x$Date,
                   units = 'secs')
  x$reloc.del.sec <- c(NA, time[-length(time)])
  x
  
  if(length(x$Vessel) > 2){
    
    x$dist.m <- c(0, diag(matr[, -1]))
    x
    
  } else if(length(x$Vessel) == 2){
    
    x$dist.m <- matr[1,]
    x
    
  } else {
    
    x$dist.m <- NA
    x
  }
  
  x$speed.m.sec <- x$dist.m/x$reloc.del.sec
  x
})
dev.list[[1]]

dev <- do.call('rbind', dev.list)
summary(dev$speed.m.sec)

# Application on the complete data

argos2 <- lapply(argos, function(y){
  
  y.list <- split(y, y$point.group)
  
  y.list <- lapply(y.list, function(x){
    # Distance matrice
    matr <- st_distance(x)
    time <- difftime(c(x$Date[-1], NA),
                     x$Date,
                     units = 'secs')
    x$reloc.del.sec <- c(NA, time[-length(time)])
    x
    
    if(length(x$Vessel) > 2){
      
      x$dist.m <- c(0, diag(matr[, -1]))
      x
      
    } else if(length(x$Vessel) == 2){
      
      x$dist.m <- matr[1,]
      x
      
    } else {
      
      x$dist.m <- NA
      x
    }
    
    x$speed.m.sec <- x$dist.m/x$reloc.del.sec
    x
  })
y <- do.call('rbind', y.list)
y
  
})
