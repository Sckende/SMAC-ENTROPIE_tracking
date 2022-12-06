# --------------------------------------------------------------- #
# OBJ --> explorer la variation de vitesse des individus 
#    --> trouver une façon de visualiser l'information
# --------------------------------------------------------------- #
rm(list = ls())
# ---- Load packages ------ #
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
head(argos)

#### TEST 1 - vitesse myenne hebdomadaire ####

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

