rm(list = ls())
#### ---- Load packages ------ ####
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")
require(aniMotum)
require(patchwork)


#### ---- Load data ---- ####
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

#### ---- move persistence model ---- ####

# max 100 km/h
fit_mp <- fit_ssm(indss,
                  vmax = 28, # env. 100 km/h
                  ang = NA,
                  model = "mp",
                  time.step = NA, # to obtain only the fitted values
                  control = ssm_control(verbose = 0))

fit_mp
summary(fit_mp)
data_mp <- grab(fit_mp,
                what = "fitted")
x11(); plot(fit_mp, type = 2)

#### ---- Production des figures ---- ####
# ---- inspectios visuelles des modèles
for(i in fit_mp$id) {
     ind <- dplyr::filter(fit_mp, id == i)
     res.rw <- osar(ind)
# x11()
png(paste("G:/Mon Drive/Projet_Publis/TRACKING_PTEBAR_JUV/MS/Analyses/Figures/SSM_MP_FITTED_",
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

####
# max 60 km/h
fit_mp1 <- fit_ssm(indss,
                  vmax = 17, # gannet speed = 61 km/h
                  ang = NA,
                  model = "mp",
                  time.step = NA, # to obtain only the fitted values
                  control = ssm_control(verbose = 0))

fit_mp1
summary(fit_mp1)
data_mp1 <- grab(fit_mp1,
                 what = "fitted")

#### ---- Calcul des vitesses à partir de BD à 100km/h max ---- ####
summary(data_mp)
class(data_mp)
x11(); map(fit_mp, what = "fitted")

# conversion en sf object
data_mp_sf <- st_as_sf(data_mp,
                       coords = c("lon", "lat"),
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
mapview(data_mp_sf, zcol = "id")

# calcul des vitesses
data_ls <- split(data_mp_sf,
                 data_mp_sf$id)
length(data_ls)
speed_ls <- lapply(data_ls,
                   function(x) {
                       diag <- diag(st_distance(x)[, -1])
                       delay <- diff(x$date)
                       vit_m.min <- as.numeric(diag)/as.numeric(delay)
                       vit_km.h <- vit_m.min * 0.001 * 60
                       
                       df <- data.frame(id = x$id,
                                        date = x$date,
                                        dist_m = c(as.numeric(diag), NA),
                                        delay_min = c(as.numeric(delay), NA),
                                        speed_km.h = c(vit_km.h, NA))
                       df
                       })

lapply(speed_ls,
       function(x) {
           summary(x$delay/60)
       })

lapply(speed_ls,
       function(x) {
           x11()
           barplot(x$speed_km.h[x$delay_min <= 120],
                   main = unique(x$id))
       })

# compilation des données
speed_df <- do.call("rbind",
                    speed_ls)
data_mp_sf2 <- left_join(data_mp_sf,
                         speed_df,
                         by = c("id", "date"))
data_mp_sf2[data_mp_sf2$speed_km.h > 110, ]

#### ---- Calcul des vitesses à partir de BD à 60km/h max ---- ####
# conversion en sf object
data_mp1 <- grab(fit_mp1,
                 what = "fitted")

data_mp_sf1 <- st_as_sf(data_mp1,
                       coords = c("lon", "lat"),
                       crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
mapview(data_mp_sf1, zcol = "id")

# calcul des vitesses
data_ls1 <- split(data_mp_sf1,
                 data_mp_sf1$id)
length(data_ls1)
speed_ls1 <- lapply(data_ls1,
                   function(x) {
                       diag <- diag(st_distance(x)[, -1])
                       delay <- diff(x$date)
                       vit_m.min <- as.numeric(diag)/as.numeric(delay)
                       vit_km.h <- vit_m.min * 0.001 * 60
                       
                       df <- data.frame(id = x$id,
                                        date = x$date,
                                        dist_m = c(as.numeric(diag), NA),
                                        delay_min = c(as.numeric(delay), NA),
                                        speed_km.h = c(vit_km.h, NA))
                       df
                       })

lapply(speed_ls1,
       function(x) {
           summary(x$delay/60)
       })

lapply(speed_ls1,
       function(x) {
           x11()
        #    barplot(x$speed_km.h[x$delay_min <= 120],
        #            main = unique(x$id))
           barplot(x$speed_km.h,
                   main = unique(x$id))
       })

# compilation des données
speed_df1 <- do.call("rbind",
                    speed_ls1)
data_mp_sf1.2 <- left_join(data_mp_sf1,
                         speed_df1,
                         by = c("id", "date"))
max(data_mp_sf1.2$speed_km.h, na.rm = T)
