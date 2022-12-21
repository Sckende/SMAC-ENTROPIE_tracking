# --------------------------------------------------------------- #
# OBJ --> explorer la variation de vitesse des individus 
#    --> trouver une façon de visualiser l'information
# --------------------------------------------------------------- #
rm(list = ls())
# ---- Load packages ------ #
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
head(argos)

#### Exploration des valeurs de vitesse ####
summary(argos$delay)
hist(argos$delay,
     nclass = 100)
# retrait des delay inf à 1h et supérieur à 12h
ar <- argos[abs(argos$delay) >= 1 & abs(argos$delay) <= 8, ]
summary(ar$speed.km.h)
summary(ar$delay)

ar[ar$speed.km.h == 2866, ]

x11()
hist(ar$speed.km.h,
     nclass = 10000)

#### Recalcul des vitesses à partir de la distance entre deux points et du délai ####
t1 <- argos[argos$Vessel == "166566", ]
head(t1)
hist(t1$delay, nclass = 100)
table(round(t1$delay))

projLatLon <- "+proj=latlon +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
t1_sf <- st_as_sf(x = t1,
                  coords = c("Longitude", "Latitude"),
                  crs = projLatLon)
mapview(t1_sf)

names(t1_sf)
new_t1 <- data.frame(Vessel = t1$Vessel,
                     Date = t1$Date,
                     Delay_h= t1$delay,
                     Rounded_delay_h = round(t1$delay),
                     Wind_spd = t1$wind_speed)
dim(new_t1)
head(new_t1)

dist_mat <- st_distance(t1_sf)[, -1]
dist_loc <- diag(dist_mat)

new_t1$dist_m <- c(0, dist_loc)
new_t1$spd_km_h <- (new_t1$dist_m / 1000) / abs(new_t1$Delay_h)

nt1 <- new_t1[abs(new_t1$Rounded_delay_h) <= 8, ]
hist(nt1$spd_km_h, nclass = 1000)

nt2 <- nt1[nt1$spd_km_h <= 100, ]
hist(nt2$spd_km_h, nclass = 1000)
x11()
barplot(nt2$Wind_spd, col = "darkgreen")
par(new = T)
barplot(nt2$spd_km_h)

lm1 <- lm(nt2$spd_km_h ~ nt2$Wind_spd)

summary(lm1)
summary(nt2$spd_km_h)

lm2 <- lm(argos$speed.km.h[year(argos$deploy) == 2017] ~ argos$wind_speed[year(argos$deploy) == 2017])
summary(lm2)

lm3 <- lm(argos$speed.km.h[year(argos$deploy) == 2018] ~ argos$wind_speed[year(argos$deploy) == 2018])
summary(lm3)


lm4 <- lm(argos$speed.km.h[year(argos$deploy) == 2017 & month(argos$Date) == 4] ~ argos$wind_speed[year(argos$deploy) == 2017 & month(argos$Date) == 4])
summary(lm4)

lm5 <- lm(argos$speed.km.h[year(argos$deploy) == 2018 & month(argos$Date) == 4] ~ argos$wind_speed[year(argos$deploy) == 2018 & month(argos$Date) == 4])
summary(lm5)

#### TEST 1 - vitesse moyenne hebdomadaire ####

# test sur un individu
ind <- argos[argos$Vessel == "166561", ]
ind_week <- split(ind,
                  ind$week_numb)
length(ind_week)
ind_week <- ind_week[-1]
ind_week_speed <- lapply(ind_week,
                         function(x) {
                             mean(x$speed.km.h,
                                  na.rm = T)
                         })
spd_df <- do.call("rbind",
                  ind_week_speed)
barplot(spd_df[, 1])

# test sur tous les individus
ind_l <- split(argos,
               argos$Vessel)

ind_spd_l <- lapply(ind_l,
                    function(x) {
                        x <- x[!x$week_numb <= 14, ]
                        l <- split(x,
                                   x$week_numb)
                        spd <- lapply(l,
                                      function(y) {
                                          mean_spd <- mean(y$speed.km.h, na.rm = T)
                                          sd_spd <- sd(y$speed.km.h, na.rm = T)
                                          data.frame(year_depl = year(x$Date[1]),
                                                     Vessel = unique(y$Vessel),
                                                     mean_spd = mean_spd,
                                                     sd_spd = sd_spd)
                                          
                                      })
                        df <- do.call("rbind",
                                      spd)
                        df$week_num <- row.names(df)
                        df
                        
                    })

ind_spd_df <- do.call("rbind",
                      ind_spd_l)

# visualisation
table(ind_spd_df$year_depl,
      ind_spd_df$Vessel) #7 ind en 2017 & 8 ind en 2018

# 2017
ind2017 <- ind_spd_df[ind_spd_df$year_depl == 2017, ]
ind2017_l <- split(ind2017,
                   ind2017$Vessel)

x11()
par(mfrow = c(3, 3))
lapply(ind2017_l,
       function(x) {
           barplot(x$mean_spd,
                   names.arg = x$week_num,
                   main = unique(x$Vessel))
       })

# 2018
ind2018 <- ind_spd_df[ind_spd_df$year_depl == 2018, ]
ind2018 <- droplevels(ind2018)
ind2018_l <- split(ind2018,
                   ind2018$Vessel)

x11()
par(mfrow = c(3, 3))
lapply(ind2018_l,
       function(x) {
           x <- x[x$week_num <= 30, ]
           barplot(x$mean_spd,
                   names.arg = x$week_num,
                   main = unique(x$Vessel))
       })

#### TEST 2 - vitesse moyenne journalière ####
tt <- argos[argos$Vessel == "162073", ]
tt <- tt[!tt$week_numb <= 14, ]
table(tt$week_numb)

tt_day <- split(tt,
                date(tt$Date))
length(tt_day)
lapply(tt_day,
       nrow)
spd <- lapply(tt_day,
              function(x) {
                  mean(x$speed.km.h, na.rm = T)
                  })
spd <- do.call("rbind", spd)
barplot(spd[1:30, 1])

