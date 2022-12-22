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

##################################
#### APPLICATION ON ALL DATA ####
################################
# Model fitting for quality control of locations is comprised of 3 steps: a data formatting step, a pre-filtering step, and the actual model fitting step. A number of checks are made on the input data during the formatting and pre-filtering steps, including applying the trip::sda filter to identify extreme observations (see ?fit_ssm for details). The pre-filter step is fully automated, although various arguments can be used to modify it’s actions, and called via the fit_ssm function. These are the minimum arguments required: the input data, the model (rw, crw, or mp) and the time.step (in h) to which locations are predicted. Additional control can be exerted over the data formatting and pre-filtering steps, via the id, date, lc, coord, epar and sderr variable name arguments, and via the vmax, ang, distlim, min.dt, and spdf pre-filtering arguments (see ?fit_ssm for details). The defaults for these arguments are quite conservative (for non-flying species), usually leading to relative few observations being flagged to be ignored by the SSM. Additional control over the optimization can also be exerted via the control = ssm_control() argument, see vignette('SSM_fitting') and ?ssm_control for more details.
# fit_ssm usually returns two sets of estimated locations in the model fit object: fitted values and predicted values. The fitted values occur at the times of the observations to which the SSM was fit (i.e., the observations that passed the pre-filter step). The predicted values occur at the regular time intervals specified by the time.step argument. If time.step = NA, then no predicted values are estimated or returned in the model fit object.

# Users can obtain the fitted or predicted locations as a data.frame by using grab().

source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")
require(aniMotum)

# Remove too short tracks
# 2017: 166570, 166571, 166573
# 2018: 162071

argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")
head(argos)
dim(argos)
table(argos$Vessel)

ind <- argos[!(argos$Vessel %in% c("166570",
                                   "166571",
                                   "166573",
                                   "162071")), ]
inds <- droplevels(ind)
dim(inds)
table(inds$Vessel)
table(inds$Vessel, year(inds$deploy))

inds$Class[inds$Class == "U"] <- 3
table(inds$Class)

# Formatage pour SSM analyses
indss <- inds[, c("Vessel", "Date", "Class", "Longitude", "Latitude")]
names(indss) <- c("id", "date", "lc", "lon", "lat")
head(indss)

#### move persistence model ####
fit_mp <- fit_ssm(indss,
                  vmax = 28,
                  model = "mp",
                  time.step = NA, # to obtain only the fitted values
                  control = ssm_control(verbose = 0))

fit_mp # "converged" = The converged column indicates whether each model fit converged successfully. / "pdHess" =  The Hessian matrix was positive-definite and could be solved to obtain parameter standard errors./ In some cases, the optimizer will converge but the Hessian matrix is not positive-definite, which typically indicates the optimizer converged on a local minimum. In this case, some standard errors may be calculated but not all. One possible solution is to try specifying a longer time.step or set time.step = NA to turn off predictions and return only fitted values (location estimates at the pre-filtered observation times). If pdHess = FALSE persists then careful inspection of the supplied data is warranted to determine if suspect observations not identified by prefilter are present. 
summary(fit_mp)

x11(); plot(fit_mp,
            what = "fitted",
            type = 1,
            pages = 1,
            ncol = 4) # see ?plot.ssm_df
x11(); plot(fit_mp,
            type = 2,
            pages = 1,
            ncol = 4)
x11(); plot(fit_mp,
            type = 3,
            pages = 1,
            ncol = 4)
x11(); map(fit_mp)

for(i in fit_mp$id) {
     ind <- dplyr::filter(fit_mp, id == i)
     x11()
     print(map(ind))
     }

#### Random Walk model ####
fit_rw <- fit_ssm(indss,
                  vmax = 28,
                  model = "rw",
                  time.step = NA,
                  control = ssm_control(verbose = 0))

fit_rw
summary(fit_rw)

x11(); plot(fit_rw,
            type = 1,
            pages = 1,
            ncol = 4)
x11(); plot(fit_rw,
            type = 2,
            pages = 1,
            ncol = 4)
map(fit_rw)

# Separate models for estimating move persistence
fmp_rw <- fit_mpm(fit_rw,
                  what = "fitted",
                  model = "jmpm",
                  control = ssm_control(vebose = 0))

x11(); plot(fmp_rw,
            pages = 1,
            ncol = 4)
for(i in fmp_rw$id) {
     ind <- dplyr::filter(fmp_rw, id == i)
     x11()
     print(map(ind))
     }

#### SSM validation ####
# use patchwork package to arrange plot.osar options
require(patchwork)
# calculate & plot residuals

class(fit_rw)

for(i in fit_rw$id) {
     ind <- dplyr::filter(fit_rw, id == i)
     res.rw <- osar(ind)
# x11()
png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/Figures/aniMotum_SSM_validation/SSM_rw_FITTED_",
              i,
              ".png",
              sep = ""),
    res = 300,
    width = 30,
    height = 30,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")
print((plot(res.rw, type = "ts") | plot(res.rw, type = "qq")) / 
  (plot(res.rw, type = "acf") | plot_spacer()))
dev.off()
}

for(i in fit_mp$id) {
     ind <- dplyr::filter(fit_mp, id == i)
     res.rw <- osar(ind)
# x11()
png(paste("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/Figures/aniMotum_SSM_validation/SSM_mp_FITTED_",
              i,
              ".png",
              sep = ""),
    res = 300,
    width = 30,
    height = 30,
    pointsize = 12,
    unit = "cm",
    bg = "transparent")
print((plot(res.rw, type = "ts") | plot(res.rw, type = "qq")) / 
  (plot(res.rw, type = "acf") | plot_spacer()))
dev.off()
}
