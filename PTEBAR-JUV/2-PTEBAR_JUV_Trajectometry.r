# --------------------------- #
# OBJECTIVE - Trajectometry analysis
# INITIAL DATA - "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED_speed.rds"
# PREVIOUS SCRIPT - 1-PTEBAR_JUV_Vitesse.r
# LITTERATURE - 2016_de Grissac_et_al_Contrasting movement strategies among juvenile albatrosses and petrels
#             - 2006_The package “adehabitat” for the R software: A tool for the analysis of space and habitat use by animals
#             - 2009_Calenge_et_al_The concept of animals' trajectories from a data analysis perspective
# Orientation , daily distance travelled, sinuosity and range
#            - 2015_Calenge_Analysis of animal movement in R: the adehabitatLT package 

# --------------------------- #
rm(list = ls())

# ---- PACKAGES ----
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

# ---- DATA ----
argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED_speed.rds")

str(argos)

# ---- WORKING on two devices ----
# ar <- do.call('rbind', argos[1:2])
ar <- argos[[1]]
# hist(table(ar$point.group))

#### Retrieve the UTM coordinates from sf object
coords <- st_coordinates(ar)
coords.DF <- as.data.frame(coords)
names(coords.DF) <- c('X', 'Y')

#### Complete coordinates in dataframe
ar <- cbind(as.data.frame(ar)[, - length(names(ar))], coords.DF)

#### Specific to the work on a single device ####
# Deletion of unused factor level
ar <- droplevels(ar)

# Burst number as factor to split the trajectory in a next step
ar$burst <- as.factor(as.character(ar$point.group))
ar$burst <- factor(as.character(ar$burst),
                   levels = 1:max(ar$point.group))
summary(ar)

#### Conversion to ltraj object ####
ar.traj <- as.ltraj(xy = ar[, c('X', 'Y')], # UTM coordinates for meter unitin distance computation
               date = ar$Date,
               id = ar$burst) # Trajectory split based on the burst of points

ar.traj # Note that dx,dy,dist are expressed in the units of the coordinates x,y (here, Degrees, Minutes and Seconds), abs.angle,rel.angle are expressed in radians and dt is in seconds

#### Trajectory parameters ####
 ar.traj[[1]]
 ar.traj[[2]]
 ar.traj[[3]]
 ar.traj[[4]]
 plot(ar.traj[1:12])
 plot(ar.traj[13:24]) 
 plot(ar.traj[25:36]) 
 plot(ar.traj[37:48]) 
 plot(ar.traj[49:60]) 
 plot(ar.traj[61:69]) 

 t <- do.call('rbind', ar.traj) 
head(t) 

#### Speed conversion ####
t$speed <- t$dist / t$dt # Speed in m/s
t$speed.km.h <- t$speed * 0.001 / (1/3600)
par(mfrow = c(1, 2)); barplot(t$speed); barplot(ar$speed.m.sec)
summary(t$speed); summary(t$speed.km.h)

#### Check for speed outliers ####

out <- t[t$speed.km.h > 20,]
out <- out[!is.na(out$speed.km.h),]

# ---- TEST with cutltraj function ----
ar.traj2 <- as.ltraj(xy = ar[, c('X', 'Y')], # UTM coordinates for meter unitin distance computation
                    date = ar$Date,
                    id = ar$Vessel) 

ar.traj2

#### Look at dt according to the date ####
plotltr(ar.traj2, 'dt/3600/24')

#### Function for detecting gap between relocs > 10h ####
gap <- function(dt){
  return(dt > 10*3600)
} # return TRUE if the delay btw 2 relocs is greater than 10 hours
gap(ar.traj2[[1]]$dt)

#### cutltraj use to cut burst into several segments ####

bursts <- cutltraj(ar.traj2, 'gap(dt)', nextr = TRUE) # Resultats similaires avec ma propre création de burst de points sauf que adehabitatLT retire les groupe de points inférieur à 3

bursts <- lapply(bursts, function(x){
  x$speed.m.s <- x$dist/x$dt # distance and time between i and i+1
  x$speed.km.h <- x$speed.m.s * 0.001 / (1/3600)
  x
})

bursts.out <- do.call('rbind', bursts)
bursts.out <- bursts.out[!is.na(bursts.out$speed.km.h),]
barplot(bursts.out$speed.km.h)

out2 <- bursts.out[bursts.out$speed.km.h > 20,]
barplot(bursts.out$speed.km.h[bursts.out$speed.km.h <= 20])

# ---- WORKING on all devices ----
#### Creation of ltraj object ####

argos.ltraj <- lapply(argos, function(x){ # List of list object - First level = device, second level = bursts
  
  coords <- st_coordinates(x)
  coords.DF <- as.data.frame(coords)
  names(coords.DF) <- c('X', 'Y')
  
  # Complete coordinates in dataframe
  x <- cbind(as.data.frame(x)[, - length(names(x))], coords.DF)

  # Specific to the work on a single device 
  # Deletion of unused factor level
  x <- droplevels(x)
  
  # Burst number as factor to split the trajectory in a next step
  x$burst <- as.factor(as.character(x$point.group))
  x$burst <- factor(as.character(x$burst),
                     levels = 1:max(x$point.group))
  # Conversion to ltraj object
  ar.traj <- as.ltraj(xy = x[, c('X', 'Y')], # UTM coordinates for meter unitin distance computation
                      date = x$Date,
                      id = x$burst,
                      infolocs = x[, c('Vessel', 'Class')]) # Trajectory split based on the burst of points
  
  ar.traj
  
})

#### Creation & exploration of speed variable ####

# Speed computation for each burst for each device

# 1 - Save the infoLoc part of the ltraj object
infosLocs <- lapply(argos.ltraj, function(x){
  x <- infolocs(x)
  x <- do.call('rbind', x)
})

# 2 - speed computation
argos.ltraj.speed <- lapply(argos.ltraj, function(x){
  
  x <- lapply(x, function(y){
    
    if(length(y) == 1){
      y$speed <- NA
    } else {
      y$speed.m.s <- round(y$dist / y$dt)
      y$speed.m.s[1] <- NA
      y$speed.km.h <- round(y$speed.m.s * 0.001 / (1/3600))
      y
    }
    
  })
  
  x <- do.call('rbind', x)
  x
  
})


par(mfrow = c(1, 2))
for(i in 1:length(argos)){
  barplot(argos.ltraj.speed[[i]]$speed.m.s)
  barplot(argos[[i]]$speed.m.sec)
  
}


# Creation of list with speed outliers for each device
argos.speed.out2 <- lapply(argos.ltraj.speed, function(x){
  
  # x <- do.call('rbind', x)
  x <- x[x$speed.m.s > 20,] # Revoir la valeur seuil
  x <- x[!is.na(x$speed.m.s),]
  x
  
})

speed.outl <- do.call('rbind', argos.speed.out2)
speed.outl$PTT <- stringr::str_sub(row.names(speed.outl), 1,6)
 
speed.outl %>%
  group_by(PTT) %>% 
  summarise(min(speed.m.s), max(speed.m.s))

# Fusion of speed DF and infosLocs
transi <- cbind(do.call('rbind', argos.ltraj.speed), do.call('rbind', infosLocs))

argos.ltraj.speed2 <- split(transi, transi$Vessel)

# Visualization of bird speed - barplot & histogram
argos.ltraj.speed2

barplot.speed <- lapply(argos.ltraj.speed2, function(x){
  
  speed1 <- x
#   argos1 <- argos[[unique(x$Vessel)]]
# })
# 
# 
# 
# argos11 <- left_join(argos1, speed1[, -c(1, 2)], by = c('Date' = 'date'))

speed1$col_class[speed1$Class %in% c('A', 'B')] <- '#FDE725FF'
speed1$col_class[speed1$Class == '0'] <- '#7AD151FF'
speed1$col_class[speed1$Class == '1'] <- '#22A884FF'
speed1$col_class[speed1$Class == '2'] <- '#2A788EFF'
speed1$col_class[speed1$Class == '3'] <- '#414487FF'
speed1$col_class[speed1$Class == 'U'] <- 'grey'

# hist(speed1$speed.m.s, breaks = dim(speed1)[1])
# barplot(speed1$speed.m.s, col = speed1$col_class) 

library(plotly)
fig <- plot_ly(
  
  x = as.character(speed1$date),
  
  y = speed1$speed.km.h,
  
  name = unique(speed1$Vessel),
  
  type = "bar",
  marker = list(color = speed1$col_class)
  
) %>%
  layout(title = unique(speed1$Vessel),
         xaxis = list(title = 'Records',
                      showticklabels = FALSE),
         yaxis = list(title = 'Speed (km/h)'))


fig
})

histo.speed <- lapply(argos.ltraj.speed2, function(x){
  
  fig <- plot_ly(
    
    x = x$speed.km.h,
    nbinsx = max(x$speed.km.h[!is.na(x$speed.km.h)]),
    
    name = unique(x$Vessel),
    
    type = "histogram"
    
  ) %>%
    layout(title = unique(x$Vessel),
           xaxis = list(title = 'Speed (km/h)'),
           yaxis = list(title = 'Occurence'))
  
  
  fig
})
# saveRDS(barplot.speed,
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_barplot_speed_list.rds')
# saveRDS(histo.speed,
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_histo_speed_list.rds')
# saveRDS(argos.ltraj.speed2,
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_argos_&_speed_ltraj.rds')


# 3 - Mean speed per PTT
ind.speed <- lapply(argos.ltraj.speed2, function(x){
  
  raw.mean <- mean(x$speed.km.h, na.rm = T)
  raw.min <- min(x$speed.km.h, na.rm = T)
  raw.max <- max(x$speed.km.h, na.rm = T)
  mean.72max <- mean(x$speed.km.h[x$speed.km.h <= 72], na.rm = T)
  
  y <- c(as.character(unique(x$Vessel)), raw.mean, raw.min, raw.max, mean.20max)
  y
})

ind.speed.range <- as.data.frame(do.call('rbind', ind.speed))
names(ind.speed.range) <- c('PTT', 'rawMean', 'rawMin', 'rawMax', 'mean.72max')

# saveRDS(ind.speed.range,
#         'C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_ind_speed_range.rds')
