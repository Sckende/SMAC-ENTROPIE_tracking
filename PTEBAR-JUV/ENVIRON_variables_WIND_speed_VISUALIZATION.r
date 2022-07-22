# ----------------------------------------------------------------------- #
# OBJECTIFS - Superposition des données de vitesse et de direction (basée sur les composantes zonales & méridionales) sur des cartes divisé par périodes de dispersion des juvéniles #
# --------------------------------------------------------------------- #

rm(list = ls())
# -------------------- #
# Packages loading ####
# -------------------- #

source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r')

# Data loading ####
argos <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED.rds')

argos.track <- readRDS('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/RMD/PTEBAR_JUV_Spatial_tracks_UTM_ARGOS.rds')

## YEAR 1 - 2017 ####
speed1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652088629_YEAR1_SPEED.nc')

zon1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637651769964_YEAR1_ZONAL.nc')
mer1 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637651880863_YEAR1_MERIDIONAL.nc')

# Check if same number and names of layers
all(names(zon1) == names(mer1))
all(names(zon1) == names(speed1))

layers1 <- names(speed1)

# vec1 <- 180/pi * atan2(zon1, mer1) + 180
# hist(values(vec1[[1]]))
# vectorplot(mean(vec1[[1:10]]))
# vec1.4 <- atan2(zon1, mer1) + 180
# hist(values(vec1.4[[1]]))
# vec1.2 <- 180/pi * atan2(zon1, mer1)
# vec1.3 <- atan2(zon1, mer1)
# 
# vectorplot(mean(vec1.4[[1:10]]), scaleSlope=T)

mean.mer1 <- mean(mer1)
mean.zon1 <- mean(zon1)
mean.speed1 <- mean(speed1)
x11();vectorplot(stack(mean.mer1, mean.zon1),
                 isField = 'dXY',
                 region = mean.speed1,
                 narrows = 300,
                 lwd.arrows = 1)

x11();levelplot(mean.speed1)

# par(mfrow = c(2, 2))
# hist(values(vec1[[1]]))
# hist(values(vec1.2[[1]]))
# hist(values(vec1.3[[1]]))
# hist(values(vec1.4[[1]]))
# 
# par(mfrow = c(1, 2))
# vectorplot(vec1)
# vectorplot(vec1.4)

argos1 <- argos[year(argos$deploy) == 2017,]


## YEAR 2 - 2018 ####

speed2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/year1_year2/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637653002349_YEAR2_SPEED.nc')

zon2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/year1_year2/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652399617_YEAR2_ZONAL.nc')
mer2 <- stack('C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/AUDREY/Env_Variables/WIND/year1_year2/CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE_1637652499961_YEAR2_MERIDIONAL.nc')

# Check if same number and names of layers
all(names(zon2) == names(mer2))
all(names(zon2) == names(speed2))

layers2 <- names(speed2)


mean.mer2 <- mean(mer2)
mean.zon2 <- mean(zon2)
mean.speed2 <- mean(speed2)
x11();vectorplot(stack(mean.mer2, mean.zon2),
           isField = 'dXY',
           region = mean.speed2,
           # narrows = 100,
           lwd.arrows = 1)

# vec2 <- 180/pi * atan2(zon2, mer2) + 180
# hist(values(vec2))

argos2 <- argos[year(argos$deploy) == 2018,]


levelplot(mean.speed2,
          at = my.at,
          col.regions = my.cols) 

# --- Global levelplot --- #
nlev <- 100
my.at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my.cols <- viridis_pal(begin = 1,
                       end = 0,
                       alpha = 0.9,
                       option = "A")(nlev)
my.cols.grey <- rev(gray(seq(0.25, 1, length.out = nlev+1)))
levelplot(mean.speed1,
          at = my.at,
          col.regions = my.cols) 

x11(); levelplot(mean.speed1,
                 at = my.at,
                 # col.regions = my.cols,
                 col.regions = my.cols.grey,
                 main = 'Wind 2017')
x11(); levelplot(mean.speed2,
                 at = my.at,
                 col.regions = my.cols,
                 main = 'Wind 2018')

# --- Global vectorplot --- #
argos.track.sp <- as(st_transform(argos.track, 4326), 'Spatial')

# png("C:/Users/ccjuhasz/Desktop/test/2017_global_Tracks_&_Wind.png",
#     res=300,
#     width=50,
#     height=30,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

# x11()
vectorplot(stack(mean.mer1, mean.zon1),
                  isField = 'dXY',
                  region =  mean.speed1,
                  at = my.at,
                  lwd.arrows = 1,
                  aspX = 0.4,
                  narrows = 500,
                  col.regions = my.cols,
                  # col.regions = my.cols.grey,
                  main = 'Tracks & Wind 2017') +
  layer(sp.lines(argos.track.sp[argos.track.sp$year == 2017,], col = viridis(7), lwd = 3))

dev.off()

# png("C:/Users/ccjuhasz/Desktop/test/2018_global_Tracks_&_Wind.png",
#     res=300,
#     width=50,
#     height=30,
#     pointsize=12,
#     unit="cm",
#     bg="transparent")

# x11()
vectorplot(stack(mean.mer2, mean.zon2),
                 isField = 'dXY',
                 region = mean.speed2,
                 at = my.at,
                 lwd.arrows = 1,
                 aspX = 0.4,
                 narrows = 500,
                 col.regions = my.cols,
                 main = 'Tracks & Wind 2018') +
  layer(sp.lines(argos.track.sp[argos.track.sp$year == 2018,], col = viridis(8), lwd = 3))

dev.off()
# --------------------- #
# AVRIL 2017 & 2018 ####
# -------------------- #

## Locations points ####
argos.list <- split(argos, year(argos$deploy))

arg.envol <- lapply(argos.list, function(x){
  
  x <- x[month(x$Date) == 4,]
})

arg.env.period <- lapply(arg.envol, function(y){
  
  y$period[day(y$Date) < 8] <- 1
  y$period[day(y$Date) %in% 8:14] <- 2
  y$period[day(y$Date) %in% 15:21] <- 3
  y$period[day(y$Date) %in% 22:28] <- 4
  y$period[day(y$Date) > 28] <- 5
  y
})

lapply(arg.env.period, function(x){
  table(x$period)
  # sum(table(x$period))
  
})
arg.env.period <- lapply(arg.env.period, function(x){
  x <- split(x, x$period)
  x
})

## WIND speed & trajectory per period ####
max(values(speed1), na.rm = T)

# period 1 - from 1 to 7 APRIL
which(layers1 == 'X2017.04.07.18.00.00')#8

sp1Per1 <- mean(speed1[[1:8]])
zon1Per1 <- mean(zon1[[1:8]])
mer1Per1 <- mean(mer1[[1:8]])

nlev <- 100
my.at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my.cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)

y1period1 <- vectorplot(stack(zon1Per1, mer1Per1),
                        isField = 'dXY',
                        narrows = 200,
                        region =  sp1Per1,
                        at = my.at,
                        col.regions = my.cols,
                        main = 'Wind 6 - 7 April 2017') 

y1period1

# ---- #

which(layers2 == 'X2018.04.07.18.00.00')#20

sp2Per1 <- mean(speed2[[1:20]])
zon2Per1 <- mean(zon2[[1:20]])
mer2Per1 <- mean(mer2[[1:20]])

y2period1 <- vectorplot(stack(zon2Per1, mer2Per1),
                        isField = 'dXY',
                        narrows = 200,
                        region =  sp2Per1,
                        at = my.at,
                        col.regions = my.cols,
                        main = 'Wind 3 - 7 April 2018')
y2period1

# period 2 - from 8 to 14 APRIL

which(layers1 == 'X2017.04.14.18.00.00')#36

sp1Per2 <- mean(speed1[[9:36]])
zon1Per2 <- mean(zon1[[9:36]])
mer1Per2 <- mean(mer1[[9:36]])

y1period2 <- vectorplot(stack(zon1Per2, mer1Per2),
                        isField = 'dXY',
                        narrows = 200,
                        region =  sp1Per2,
                        at = my.at,
                        col.regions = my.cols,
                        main = 'Wind 8 - 14 April 2017') 
y1period2
# ---- #

which(layers2 == 'X2018.04.14.18.00.00')#48

sp2Per2 <- mean(speed2[[21:48]])
zon2Per2 <- mean(zon2[[21:48]])
mer2Per2 <- mean(mer2[[21:48]])

y2period2 <- vectorplot(stack(zon2Per2, mer2Per2),
                        isField = 'dXY',
                        narrows = 200,
                        region =  sp2Per2,
                        at = my.at,
                        col.regions = my.cols,
                        main = 'Wind 8 - 14 April 2018')
y2period2

# period 3 - from 15 to 21 APRIL

which(layers1 == 'X2017.04.21.18.00.00')#64

sp1Per3 <- mean(speed1[[37:64]])
zon1Per3 <- mean(zon1[[37:64]])
mer1Per3 <- mean(mer1[[37:64]])

y1period3 <- vectorplot(stack(zon1Per3, mer1Per3),
                        isField = 'dXY',
                        narrows = 200,
                        region =  sp1Per3,
                        at = my.at,
                        col.regions = my.cols,
                        main = 'Wind 9 - 21 April 2017')
y1period3

# ---- #

which(layers2 == 'X2018.04.21.18.00.00')#76

sp2Per3 <- mean(speed2[[49:76]])
zon2Per3 <- mean(zon2[[49:76]])
mer2Per3 <- mean(mer2[[49:76]])

y2period3 <- vectorplot(stack(zon2Per3, mer2Per3),
                        isField = 'dXY',
                        narrows = 200,
                        region =  sp2Per3,
                        at = my.at,
                        col.regions = my.cols,
                        main = 'Wind 9 - 21 April 2018')
y2period3

# period 4 - from 22 to 28 APRIL

which(layers1 == 'X2017.04.28.18.00.00')#92

sp1Per4 <- mean(speed1[[65:92]])
zon1Per4 <- mean(zon1[[65:92]])
mer1Per4 <- mean(mer1[[65:92]])

y1period4 <- vectorplot(stack(zon1Per4, mer1Per4),
                        isField = 'dXY',
                        narrows = 200,
                        region =  sp1Per4,
                        at = my.at,
                        col.regions = my.cols,
                        main = 'Wind 22 - 28 April 2017')

y1period4

# ---- #

which(layers2 == 'X2018.04.28.18.00.00')#104

sp2Per4 <- mean(speed2[[77:104]])
zon2Per4 <- mean(zon2[[77:104]])
mer2Per4 <- mean(mer2[[77:104]])

y2period4 <- vectorplot(stack(zon2Per4, mer2Per4),
                        isField = 'dXY',
                        narrows = 200,
                        region =  sp2Per4,
                        at = my.at,
                        col.regions = my.cols,
                        main = 'Wind 22 - 28 April 2018')

y2period4

# period 5 - from 29 to 30 APRIL

which(layers1 == 'X2017.04.30.18.00.00')#100

sp1Per5 <- mean(speed1[[93:100]])
zon1Per5 <- mean(zon1[[93:100]])
mer1Per5 <- mean(mer1[[93:100]])

y1period5 <- vectorplot(stack(zon1Per5, mer1Per5),
                        isField = 'dXY',
                        narrows = 200,
                        region =  sp1Per5,
                        at = my.at,
                        col.regions = my.cols,
                        main = 'Wind 29 - 30 April 2017')

y1period5
# ---- #

which(layers2 == 'X2018.04.30.18.00.00')#112

sp2Per5 <- mean(speed2[[105:112]])
zon2Per5 <- mean(zon2[[105:112]])
mer2Per5 <- mean(mer2[[105:112]])

y2period5 <- vectorplot(stack(zon2Per5, mer2Per5),
                        isField = 'dXY',
                        narrows = 200,
                        region =  sp2Per5,
                        at = my.at,
                        col.regions = my.cols,
                        main = 'Wind 29 - 30 April 2018')

y2period5

# ---- #
par(mfrow = c(1,2
              ))
y1period1; y1period2

boby.sp <- as(st_transform(arg.env.period[[1]][[1]], 4326), 'Spatial')
class(boby.sp)

proj4string(boby.sp)
proj4string(vec1Per1)
 # Dispersion avril 2017
y1period1 +
  layer(sp.points(boby.sp))

y1period2 +
  layer(sp.points(as(st_transform(arg.env.period[[1]][[2]], 4326), 'Spatial')))

y1period3 +
  layer(sp.points(as(st_transform(arg.env.period[[1]][[3]], 4326), 'Spatial')))

y1period4 +
  layer(sp.points(as(st_transform(arg.env.period[[1]][[4]], 4326), 'Spatial')))

y1period5 +
  layer(sp.points(as(st_transform(arg.env.period[[1]][[5]], 4326), 'Spatial')))


# Dispersion avril 2018
y2period1 +
  layer(sp.points(as(st_transform(arg.env.period[[2]][[1]], 4326), 'Spatial')))

y2period2 +
  layer(sp.points(as(st_transform(arg.env.period[[2]][[2]], 4326), 'Spatial')))

y2period3 +
  layer(sp.points(as(st_transform(arg.env.period[[2]][[3]], 4326), 'Spatial')))

y2period4 +
  layer(sp.points(as(st_transform(arg.env.period[[2]][[4]], 4326), 'Spatial')))

# y2period5 +
#   layer(sp.points(as(st_transform(arg.env.period[[2]][[5]], 4326), 'Spatial')))

# ------------------------------------- #
# From APRIL to AUGUST by quinzaine ####
# ----------------------------------- #

# period 1 - 1-15 APRIL
# period 2 - 16-30 APRIL
# period 3 - 1-15 MAY
# period 4 - 16-31 MAY
# period 5 - 1-15 JUNE
# period 6 - 16-30 JUNE
# period 7 - 1-15 JULY
# period 8 - 16-31 JULY
# period 9 - 1-15 AUGUST
# period 10 - 16-31 AUGUST

## Locations points ####
argos.list <- split(argos, year(argos$deploy))

arg.envol <- lapply(argos.list, function(x){
  
  x <- x[month(x$Date) %in% 4:8,]
})

lapply(arg.envol, function(x){
  table(month(x$Date))
})

arg.env.period <- lapply(arg.envol, function(y){
  
  y <- split(y, month(y$Date))
  
  y <- lapply(y, function(z){
    
    z$period[day(z$Date) %in% 1:15] <- 1
    z$period[day(z$Date) %in% 16:31] <- 2
    z
    
  })
  y <- do.call('rbind', y)
  y
})


lapply(arg.env.period, function(x){
  table(x$period)
  # sum(table(x$period))
  
})



arg.env.period <- lapply(arg.env.period, function(x){
  x <- split(x, list(x$period, month(x$Date)))
  x
})

# First try on 2017 only
# ---- test ---- #
test2017 <- arg.env.period[[1]]

init <- 1
for(i in  1:length(test2017)){

  test2017[[i]]$period <- init

  init <- init + 1
}

tt2017 <- do.call('rbind', test2017)
table(tt2017$period)

# ---- #

test2018 <- arg.env.period[[2]]

init <- 1
for(i in  1:length(test2018)){
  
  test2018[[i]]$period <- init
  
  init <- init + 1
}

tt2018 <- do.call('rbind', test2018)
table(tt2018$period)

# Layers manipulations
stringr::str_which(layers2, '.04.30.') # give the indexes of strings that contain a pattern match
cut.lay1 <- stringr::str_which(layers1, 'X2017.04.16.00.00.00|X2017.05.01.00.00.00|X2017.05.16.00.00.00|X2017.06.01.00.00.00|X2017.06.16.00.00.00|X2017.07.01.00.00.00|X2017.07.16.00.00.00|X2017.08.01.00.00.00|X2017.08.16.00.00.00|X2017.09.01.00.00.00')
cut.lay1 <- c(1, cut.lay1)


cut.lay2 <- stringr::str_which(layers2, 'X2018.04.16.00.00.00|X2018.05.01.00.00.00|X2018.05.16.00.00.00|X2018.06.01.00.00.00|X2018.06.16.00.00.00|X2018.07.01.00.00.00|X2018.07.16.00.00.00|X2018.08.01.00.00.00|X2018.08.16.00.00.00|X2018.09.01.00.00.00')
cut.lay2 <- c(1, cut.lay2)

# layers2[stringr::str_detect(layers2, 'X2018.04.16.|X2018.05.01.|X2018.05.16.|X2018.06.01.|X2018.06.16.|X2018.07.01.|X2018.07.16.|X2018.08.01.|X2018.08.16.|X2018.09.01.')]

# ---- 2017 ---- #
for(i in 1:(length(cut.lay1)-1)){
  

  
  begin <- cut.lay1[i]
  end <- cut.lay1[i + 1] - 1
  layer.name1 <- str_sub(names(speed1[[begin]]), 2, 11)
  layer.name2 <- str_sub(names(speed1[[end]]), 2, 11)
  
  print(end - begin)
  # ---- #
  meanSpeed <- mean(speed1[[begin:end]])
  meanZon <- mean(zon1[[begin:end]])
  meanMer <- mean(mer1[[begin:end]])

  # ---- #
  
  png(paste("C:/Users/ccjuhasz/Desktop/test/2017/", layer.name2, ".png", sep = ''),
    res=300,
    width=50,
    height=30,
    pointsize=12,
    unit="cm",
    bg="transparent")

if(i == 1){
  print(vectorplot(stack(meanZon, meanMer),
                   isField = 'dXY',
                   narrows = 200,
                   region =  meanSpeed,
                   at = my.at,
                   lwd.arrows = 1,
                   aspX = 0.4,
                   col.regions = my.cols,
                   main = paste('From ', layer.name1, ' to ', layer.name2, sep = '')) +
          layer(sp.points(as(st_transform(test2017[[i]], 4326), 'Spatial'), col = 'white', cex = 2)))
}else{
  print(vectorplot(stack(meanZon, meanMer),
                   isField = 'dXY',
                   narrows = 200,
                   region =  meanSpeed,
                   at = my.at,
                   lwd.arrows = 1,
                   aspX = 0.4,
                   col.regions = my.cols,
                   main = paste('From ', layer.name1, ' to ', layer.name2, sep = '')) +
          layer(c(sp.points(as(st_transform(test2017[[i]], 4326), 'Spatial'), col = 'white', cex = 2), sp.points(as(st_transform(test2017[[i-1]], 4326), 'Spatial'), col = 'grey', cex = 1))))
}
  


  dev.off()
  
}

# ---- 2018 ---- #

for(i in 1:(length(cut.lay2)-1)){
  
  
  
  begin <- cut.lay2[i]
  end <- cut.lay2[i + 1] - 1
  layer.name1 <- str_sub(names(speed2[[begin]]), 2, 11)
  layer.name2 <- str_sub(names(speed2[[end]]), 2, 11)
  
  print(end - begin)
  # ---- #
  meanSpeed <- mean(speed2[[begin:end]])
  meanZon <- mean(zon2[[begin:end]])
  meanMer <- mean(mer2[[begin:end]])
  
  # ---- #
  
  png(paste("C:/Users/ccjuhasz/Desktop/test/2018/", layer.name2, ".png", sep = ''),
      res=300,
      width=50,
      height=30,
      pointsize=12,
      unit="cm",
      bg="transparent")
  
  if(i == 1){
    print(vectorplot(stack(meanZon, meanMer),
                     isField = 'dXY',
                     narrows = 300,
                     region =  meanSpeed,
                     at = my.at,
                     lwd.arrows = 1,
                     aspX = 0.4,
                     col.regions = my.cols,
                     main = paste('From ', layer.name1, ' to ', layer.name2, sep = '')) +
            layer(sp.points(as(st_transform(test2018[[i]], 4326), 'Spatial'), col = 'white', cex = 2)))
  }else{
    print(vectorplot(stack(meanZon, meanMer),
                     isField = 'dXY',
                     narrows = 300,
                     region =  meanSpeed,
                     at = my.at,
                     lwd.arrows = 1,
                     aspX = 0.4,
                     col.regions = my.cols,
                     main = paste('From ', layer.name1, ' to ', layer.name2, sep = '')) +
            layer(c(sp.points(as(st_transform(test2018[[i]], 4326), 'Spatial'), col = 'white', cex = 2), sp.points(as(st_transform(test2018[[i-1]], 4326), 'Spatial'), col = 'grey', cex = 1))))
  }
  
  
  
  dev.off()
  
}

# ---------------------------------- #
# From APRIL to AUGUST - monthly ####
# -------------------------------- #
argos.sp <- as(st_transform(argos, 4326), 'Spatial')
proj4string(argos.sp)

## Locations points ####
argos.list <- split(argos.sp, year(argos.sp$deploy))

argos.sp.2017 <- argos.list[[1]]
argos.sp.2018 <- argos.list[[2]]
# arg.month.list <- lapply(argos.list, function(x){
#   
#   x <- split(x, month(x$Date))
# })

## Layers manipulations ####
# extraire les mois pour chaque année de suivi et moyenner les rasterstacks dont les noms fit avec le mois en question
# layers1 = from X2017.04.06.00.00.00 to X2017.09.16.00.00.00
cut.lay1 <- stringr::str_which(layers1, 'X2017.05.01.00.00.00|X2017.06.01.00.00.00|X2017.07.01.00.00.00|X2017.08.01.00.00.00|X2017.09.01.00.00.00')
cut.lay1 <- c(1, cut.lay1)

## 2017 ####

nlev <- 100
my.at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my.cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)

for(i in 1:(length(cut.lay1))){
  
  if(i == length(cut.lay1)){
    begin <- cut.lay1[i]
    end <- length(names(speed1))
    layer.name1 <- str_sub(names(speed1[[begin]]), 2, 11)
    layer.name2 <- str_sub(names(speed1[[end]]), 2, 11)
    month <- as.numeric(str_sub(layer.name1, 6,7))
    
    print(paste('Layer number: ', end - begin))
    print(paste('Month: ', month))
    # ---- #
    meanSpeed <- mean(speed1[[begin:end]])
    meanZon <- mean(zon1[[begin:end]])
    meanMer <- mean(mer1[[begin:end]])
  }else{
    begin <- cut.lay1[i]
    end <- cut.lay1[i + 1] - 1
    layer.name1 <- str_sub(names(speed1[[begin]]), 2, 11)
    layer.name2 <- str_sub(names(speed1[[end]]), 2, 11)
    month <- as.numeric(str_sub(layer.name1, 6,7))
    
    print(paste('Layer number: ', end - begin))
    print(paste('Month: ', month))
    # ---- #
    meanSpeed <- mean(speed1[[begin:end]])
    meanZon <- mean(zon1[[begin:end]])
    meanMer <- mean(mer1[[begin:end]])
  }
  

  
  # ---- #
  
  png(paste("C:/Users/ccjuhasz/Desktop/test/2017/monthly/", layer.name2, ".png", sep = ''),
      res=300,
      width=30,
      height=30,
      pointsize=12,
      unit="cm",
      bg="transparent")
  
  if(i == 1){
    print(vectorplot(stack(meanZon, meanMer),
                     isField = 'dXY',
                     narrows = 300,
                     region =  meanSpeed,
                     at = my.at,
                     lwd.arrows = 1,
                     aspX = 0.4,
                     col.regions = my.cols,
                     main = paste('From ', layer.name1, ' to ', layer.name2, sep = '')) +
            layer(sp.points(argos.sp.2017[month(argos.sp.2017$Date) == month,], col = 'white', cex = 2))
            # layer(sp.points(as(st_transform(arg.month.list[['2017']][[month]], 4326), 'Spatial'), col = 'white', cex = 2))
    )
  }else{
    print(vectorplot(stack(meanZon, meanMer),
                     isField = 'dXY',
                     narrows = 300,
                     region =  meanSpeed,
                     at = my.at,
                     lwd.arrows = 1,
                     aspX = 0.4,
                     col.regions = my.cols,
                     main = paste('From ', layer.name1, ' to ', layer.name2, sep = '')) +
            layer(c(sp.points(argos.sp.2017[month(argos.sp.2017$Date) == month,], col = 'white', cex = 2), sp.points(argos.sp.2017[month(argos.sp.2017$Date) == (month-1),], col = 'grey', cex = 1))))
            # layer(c(sp.points(as(st_transform(arg.month.list[['2017']][[month]], 4326), 'Spatial'), col = 'white', cex = 2), sp.points(as(st_transform(arg.month.list[['2017']][[month-1]], 4326), 'Spatial'), col = 'grey', cex = 1))))
  }
  
  
  
  dev.off()
  
}

## 2018 ####
cut.lay2 <- stringr::str_which(layers2, 'X2018.05.01.00.00.00|X2018.06.01.00.00.00|X2018.07.01.00.00.00|X2018.08.01.00.00.00|X2018.09.01.00.00.00|X2018.10.01.00.00.00|X2018.11.01.00.00.00|X2018.12.01.00.00.00')
cut.lay2 <- c(1, cut.lay2)



nlev <- 100
my.at <- seq(from = 0,
             to = 20,
             length.out = nlev+1)
my.cols <- viridis_pal(begin = 1,
                       end = 0,
                       option = "A")(nlev)

for(i in 1:(length(cut.lay2))){
  
  if(i == length(cut.lay2)){
    begin <- cut.lay2[i]
    end <- length(names(speed2))
    layer.name1 <- str_sub(names(speed2[[begin]]), 2, 11)
    layer.name2 <- str_sub(names(speed2[[end]]), 2, 11)
    month <- as.numeric(str_sub(layer.name1, 6,7))
    
    print(paste('Layer number: ', end - begin))
    print(paste('Month: ', month))
    # ---- #
    meanSpeed <- mean(speed2[[begin:end]])
    meanZon <- mean(zon2[[begin:end]])
    meanMer <- mean(mer2[[begin:end]])
  }else{
    begin <- cut.lay2[i]
    end <- cut.lay2[i + 1] - 1
    layer.name1 <- str_sub(names(speed2[[begin]]), 2, 11)
    layer.name2 <- str_sub(names(speed2[[end]]), 2, 11)
    month <- as.numeric(str_sub(layer.name1, 6,7))
    
    print(paste('Layer number: ', end - begin))
    print(paste('Month: ', month))
    # ---- #
    meanSpeed <- mean(speed2[[begin:end]])
    meanZon <- mean(zon2[[begin:end]])
    meanMer <- mean(mer2[[begin:end]])
  }
  
  
  
  # ---- #
  
  png(paste("C:/Users/ccjuhasz/Desktop/test/2018/monthly/", layer.name2, ".png", sep = ''),
      res=300,
      width=30,
      height=30,
      pointsize=12,
      unit="cm",
      bg="transparent")
  
  if(i == 1){
    print(vectorplot(stack(meanZon, meanMer),
                     isField = 'dXY',
                     narrows = 300,
                     region =  meanSpeed,
                     at = my.at,
                     lwd.arrows = 1,
                     aspX = 0.4,
                     col.regions = my.cols,
                     main = paste('From ', layer.name1, ' to ', layer.name2, sep = '')) +
            layer(sp.points(argos.sp.2018[month(argos.sp.2018$Date) == month,], col = 'white', cex = 2))
          # layer(sp.points(as(st_transform(arg.month.list[['2017']][[month]], 4326), 'Spatial'), col = 'white', cex = 2))
    )
  }else{
    print(vectorplot(stack(meanZon, meanMer),
                     isField = 'dXY',
                     narrows = 300,
                     region =  meanSpeed,
                     at = my.at,
                     lwd.arrows = 1,
                     aspX = 0.4,
                     col.regions = my.cols,
                     main = paste('From ', layer.name1, ' to ', layer.name2, sep = '')) +
            layer(c(sp.points(argos.sp.2018[month(argos.sp.2018$Date) == month,], col = 'white', cex = 2), sp.points(argos.sp.2018[month(argos.sp.2018$Date) == (month-1),], col = 'grey', cex = 1))))
    # layer(c(sp.points(as(st_transform(arg.month.list[['2017']][[month]], 4326), 'Spatial'), col = 'white', cex = 2), sp.points(as(st_transform(arg.month.list[['2017']][[month-1]], 4326), 'Spatial'), col = 'grey', cex = 1))))
  }
  
  
  
  dev.off()
  
}
