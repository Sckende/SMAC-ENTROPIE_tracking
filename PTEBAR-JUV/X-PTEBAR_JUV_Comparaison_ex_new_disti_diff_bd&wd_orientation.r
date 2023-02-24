# ---- FIRST DF TO COMPARE ---- OLD FIGURES NO SSM ---- #### 
comp <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
names(comp)

comp2 <- comp[comp$Vessel %in% c(166569, 
                                166572,
                                166564,
                                166565,
                                162070,
                                162072,
                                162073,
                                166561,
                                166563),]
comp2 <- droplevels(comp2)
unique(comp2$Vessel)

comp2$diff_dir <- (comp2$bird_0_360_METEO_TOWARD - comp2$wind_dir_0_360) %% 360

# ---- Visualisation GLOBALE
x11()
hist(comp2$diff_dir,
     freq = FALSE,
     breaks = 36,
     main = "diff direct° bd-wd - no SSM - GLOBAL",
     xlab = "angle (°)")
lines(density(comp2$diff_dir,
              na.rm = T,
              from = 0,
              to = 360))

# ---- Visualisation GLOBALE MENSUELLE

comp_month <- split(comp2,
                    month(comp2$Date))[2:10] # retrait du mois de janvier
x11(); par(mfrow = c(3, 3))
lapply(comp_month,
       function(x) {
           hist(x$diff_dir,
                freq = FALSE,
                breaks = 36,
                main = paste("diff direct° bd-wd - no SSM - GLOBAL",
                             unique(month(x$Date, label = TRUE, abbr = FALSE))),
                xlab = "angle (°)")
           lines(density(x$diff_dir,
                         na.rm = T,
                         from = 0,
                         to = 360))
       }) # VALIDATED 

#### ---- SECOND DF TO COMPARE ---- NEW FIGURES WITH SSM ---- ####
loc <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_aniMotum_fitted_data_env_param_bird_dir_110max.rds")
names(loc)
head(loc$wind_meteo_dir_loc)
# --------------- #
loc$dir_bird_deg0_360 <- ifelse(loc$dir_bird_deg >= 0,
                                loc$dir_bird_deg,
                                360 + loc$dir_bird_deg)

loc$wind_meteo_dir0_360_loc <- ifelse(loc$wind_meteo_dir_loc >= 0,
                                  loc$wind_meteo_dir_loc,
                                  360 + loc$wind_meteo_dir_loc)

loc$wind_meteo_dir0_360_200km <- ifelse(loc$wind_meteo_dir_200km >= 0,
                                  loc$wind_meteo_dir_200km,
                                  360 + loc$wind_meteo_dir_200km)

# --------------- #
loc$diff_wind_bird_loc <- (loc$dir_bird_deg0_360 - loc$wind_meteo_dir0_360_loc) %% 360
loc$diff_wind_bird_200km <- (loc$dir_bird_deg0_360 - loc$wind_meteo_dir0_360_200km) %% 360

# ---- Visualisation GLOBALE
x11()
hist(loc$diff_wind_bird_loc,
     freq = FALSE,
     breaks = 36,
     main = "diff direct° bd-wd - WITH SSM - GLOBAL",
     xlab = "angle (°)")
lines(density(loc$diff_wind_bird_loc,
              na.rm = T,
              from = 0,
              to = 360))

# ---- Visualisation GLOBALE MENSUELLE

loc_month <- split(loc,
                    month(loc$date))[2:10] # retrait du mois de janvier
x11(); par(mfrow = c(3, 3))
lapply(loc_month,
       function(x) {
           hist(x$diff_wind_bird_loc,
                freq = FALSE,
                breaks = 36,
                main = paste("diff direct° bd-wd - WITH SSM - GLOBAL",
                             unique(month(x$date, label = TRUE, abbr = FALSE))),
                xlab = "angle (°)")
           lines(density(x$diff_wind_bird_loc,
                         na.rm = T,
                         from = 0,
                         to = 360))
       })


#### ---- Comparaison ---- ####
# GLOBAL

hist_comp <- hist(comp2$diff_dir, plot = FALSE)
hist_SSM <- hist(loc$diff_wind_bird_loc, plot = FALSE)

x11()
hist(comp2$diff_dir,
     freq = FALSE,
     breaks = 36,
     col = "grey",
     main = "diff direct° bird-wind",
     xlab = "angle (°)",
     ylim = c(0, max(c(hist_comp$density, hist_SSM$density)) + 0.001))
lines(density(comp2$diff_dir,
              na.rm = T,
              from = 0,
              to = 360))
# ---- #
hist(loc$diff_wind_bird_loc,
     freq = FALSE,
     breaks = 36,
     add = TRUE,
     col = "#0baea0aa")
lines(density(loc$diff_wind_bird_loc,
              na.rm = T,
              from = 0,
              to = 360),
      col = "#034d3c")
legend("topright",
       legend = c("NO SSM", "WITH SSM"),
       fill = c("grey", "#0baea0aa"),
       border = c("grey", "#0baea0aa"),
       bty = "n")

# GLOBAL MONTHLY

x11(); par(mfrow = c(3, 3))

for(i in 1:length(comp_month)) {
    
    hist_comp <- hist(comp_month[[i]]$diff_dir, plot = FALSE)
    hist_SSM <- hist(loc_month[[i]]$diff_wind_bird_loc, plot = FALSE)
    
    hist(comp_month[[i]]$diff_dir,
     freq = FALSE,
     breaks = 36,
     col = "grey",
     main = paste("diff direct° bird-wind", unique(month(comp_month[[i]]$Date, label = TRUE, abbr = FALSE))),
     xlab = "angle (°)",
     ylim = c(0, 0.008))
lines(density(comp_month[[i]]$diff_dir,
              na.rm = T,
              from = 0,
              to = 360))
# ---- #
hist(loc_month[[i]]$diff_wind_bird_loc,
     freq = FALSE,
     breaks = 36,
     add = TRUE,
     col = "#0baea0aa")
lines(density(loc_month[[i]]$diff_wind_bird_loc,
              na.rm = T,
              from = 0,
              to = 360),
      col = "#034d3c")
legend("topleft",
       legend = c("NO SSM", "WITH SSM"),
       fill = c("grey", "#0baea0aa"),
       border = c("grey", "#0baea0aa"),
       bty = "n")
}