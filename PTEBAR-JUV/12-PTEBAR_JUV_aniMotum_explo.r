#### Package install ####
# install.packages("aniMotum", 
#                  repos = c("https://cloud.r-project.org",
#                            "https://ianjonsen.r-universe.dev"),
#                  dependencies = "Suggests")

library(aniMotum)

fit <- fit_ssm(sese,
               vmax = 4,
               model = "mp",
               time.step = 24,
               control = ssm_control(verbose = 0))
x11()
plot(fit, type = 3, pages = 1, ncol = 2)

x11()
map(fit,
    what = "predicted",
    crs = "+proj=stere +lon_0=68 +units=km +datum=WGS84")

#### TEST ####
argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
head(argos)
dim(argos)
table(argos$Vessel)
argos <- argos[argos$Class != "U", ]

ind1 <- argos[argos$Vessel %in% c("162073") & lubridate::month(argos$Date) <= 5, ]
ind1 <- droplevels(ind1)
dim(ind1)
summary(ind1)
head(ind1)
plot(ind1$Longitude,
     ind1$Latitude,
     type = "b")

ind11 <- ind1[, c("Vessel", "Date", "Class", "Longitude", "Latitude")]
names(ind11) <- c("id", "date", "lc", "lon", "lat")
head(ind11)

rw_ind1 <- fit_ssm(ind11,
                    vmax = 28, #max speed in m.s
                    model = "rw", #a simple random walk 
                    time.step = 6,
                    control = ssm_control(verbose = 0))
crw_ind1 <- fit_ssm(ind11,
                    vmax = 28, #max speed in m.s
                    model = "crw", #a correlated random walk
                    time.step = 6,
                    control = ssm_control(verbose = 0))
mp_ind1 <- fit_ssm(ind11,
                    vmax = 28, #max speed in m.s
                    model = "mp", #a time-varying move persistence model 
                    time.step = 6,
                    control = ssm_control(verbose = 0))
x11(); par(mfraow = c(2, 2))
plot(rw_ind1, what = "predicted", type = 1)
plot(crw_ind1, what = "predicted", type = 1)
plot(mp_ind1, what = "predicted", type = 1)

# Predictions for each model types
pred_rw_ind1 <- grab(rw_ind1,
                     what = "predicted") 
pred_crw_ind1 <- grab(crw_ind1,
                     what = "predicted") 
pred_mp_ind1 <- grab(mp_ind1,
                     what = "predicted") 


# Vizualisation
x11(); par(mfrow = c(2, 2))
plot(ind1$Longitude,
     ind1$Latitude,
     type = "b",
     col = "grey")

points(pred_rw_ind1$lon,
     pred_rw_ind1$lat,
     # type = "b",
     col = "darkorange")
points(pred_crw_ind1$lon,
       pred_crw_ind1$lat,
       # type = "b",
       col = "darkred")
points(pred_mp_ind1$lon,
       pred_mp_ind1$lat,
       # type = "b",
       col = "darkgreen")



fit_ind11 <- fit_ssm(ind11,
                    vmax = 28, #max speed in m.s
                    model = "mp", #a time-varying move persistence model 
                    time.step = 12,
                    control = ssm_control(verbose = 0))
fit_ind111 <- fit_ssm(ind11,
                    vmax = 28, #max speed in m.s
                    model = "mp", #a time-varying move persistence model 
                    time.step = 6,
                    control = ssm_control(verbose = 0))
summary(fit_ind1)
# Predictions - 24h
x11(); plot(fit_ind1, what = "predicted", type = 1, pages = 1, ncol = 1)
x11(); plot(fit_ind1, what = "predicted",  type = 2, pages = 1, ncol = 1)
x11(); plot(fit_ind1, what = "predicted",  type = 3, pages = 1, ncol = 1)

# Predictions - 12h
x11(); plot(fit_ind11, what = "predicted", type = 1, pages = 1, ncol = 1)
x11(); plot(fit_ind11, what = "predicted", type = 2, pages = 1, ncol = 1)
x11(); plot(fit_ind11, what = "predicted", type = 3, pages = 1, ncol = 1)# sorties fit_ind1 & fit_ind11 assez similaires

# Predictions - 6h
x11(); plot(fit_ind111, what = "predicted", type = 1, pages = 1, ncol = 1)
x11(); plot(fit_ind111, what = "predicted", type = 2, pages = 1, ncol = 1)
x11(); plot(fit_ind111, what = "predicted", type = 3, pages = 1, ncol = 1)# sorties fit_ind1 & fit_ind11 assez similaires

# test de calcul de vitesse avec localisations prédites au 24h
t_vit24 <- fit_ind1$ssm$`162073`$predicted
head(t_vit24)
mapview::mapview(t_vit24)

diag_dist24 <- diag(sf::st_distance(t_vit24)[, -1])
vitesse24 <- diag_dist24/24
max(vitesse24)
hist(vitesse24, main = "24hours", nclass = as.numeric(round(max(vitesse))))
plot(t_vit24$date[2:31],
     vitesse24[1:30],
     main = "24hours",
     type = "b")

# test de calcul de vitesse avec localisations prédites au 12h
t_vit12 <- fit_ind11$ssm$`162073`$predicted
head(t_vit12)
mapview::mapview(t_vit12)

diag_dist12 <- diag(sf::st_distance(t_vit12)[, -1])
vitesse12 <- diag_dist12/12
max(vitesse12)
hist(vitesse12, main = "12hours", nclass = as.numeric(round(max(vitesse12))))
plot(t_vit12$date[2:61],
     vitesse12[1:60],
     main = "12hours",
     type = "b")

# test de calcul de vitesse avec localisations prédites au 6h
t_vit6 <- fit_ind111$ssm$`162073`$predicted
head(t_vit6)
mapview::mapview(t_vit6)

diag_dist6 <- diag(sf::st_distance(t_vit6)[, -1])
vitesse6 <- diag_dist6/6
max(vitesse6)
hist(vitesse6, main = "6hours", nclass = as.numeric(round(max(vitesse6))))
plot(t_vit6$date[2:121],
     vitesse6[1:120],
     main = "6hours",
     type = "b")



####
x11(); map(fit_ind1)

fmp1 <- fit_mpm(fit_ind1,
               what = "predicted",
               model = "mpm",
               control = ssm_control(vebose = 0))
fmp11 <- fit_mpm(fit_ind11,
                what = "predicted",
                model = "mpm",
                control = ssm_control(vebose = 0))
fmp111 <- fit_mpm(fit_ind111,
                what = "predicted",
                model = "mpm",
                control = ssm_control(vebose = 0))
x11(); plot(fmp1, ask = F)
x11(); plot(fmp11, ask = F)
x11(); plot(fmp111, ask = F)

#### APPLICATION ON ALL DATA ####

# Remove too short tracks
# 2017: 166570, 166571, 166573
# 2018: 162071

argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
head(argos)
dim(argos)
table(argos$Vessel)
argos <- argos[argos$Class != "U", ]

ind <- argos[!(argos$Vessel %in% c("166570",
                                   "166571",
                                   "166573",
                                   "162071")), ]
inds <- droplevels(ind)
dim(inds)
table(inds$Vessel)

indss <- inds[, c("Vessel", "Date", "Class", "Longitude", "Latitude")]
names(indss) <- c("id", "date", "lc", "lon", "lat")
head(indss)


fit_indss <- fit_ssm(indss,
                     vmax = 28,
                     model = "mp",
                     time.step = 12,
                     control = ssm_control(verbose = 0))

fit_indss
summary(fit_indss)

x11(); plot(fit_indss, type = 3, pages = 1, ncol = 4)

fmp_indss <- fit_mpm(fit_indss,
                     what = "predicted",
                     model = "mpm",
                     control = ssm_control(vebose = 0))

x11(); plot(fmp_indss, type = 3, pages = 1, ncol = 4)
