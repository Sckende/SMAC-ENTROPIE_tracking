#############################################################################################################################################################
#                                                                                                                                                      #
#                                                  SCRIPT 1 : Téléchargement des données environementales                                              #
#                                                               BASED ON: PNB 2021, Romain FERNANDEZ                                                             #
#                                                                                                                                                      #
#############################################################################################################################################################


############################################################## DONNEES POUR LA PARTIE 1 DU SCRIPT ######################################################
rm(list = ls())
# Packages nécessaires
# install.packages("ecmwfr") # Connexion à Copernicus et téléchargement des variables

library(ecmwfr)

# Stocker le chemin de l'espace de travail
# wd <- getwd()
wd <- 'C:/Users/ccjuhasz/Desktop'


# Chemin de stockage des fichiers exportés
dossier_vars <- paste(wd, "Variables", sep = "/")

########## Paramètres de téléchargement des variables issues de Copernicus ####
years <- list("2017", "2018", "2019")
monthss <- list(c("04", "05", "06", "07", "08", "09"),
                c("04", "05", "06", "07", "08", "09", "10", "11", "12"),
                "01")
days <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", 
          "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31")
times <- c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", 
          "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00")
area <- c(31, 10, -55, 155)

########## Création de la liste de requêtes en fonction de la période d'intérêt par années ####

chloro_a <- list()
for(i in 1:length(years)){
  chloro_a[[i]] <- list(
  format = "zip",
  variable = "mass_concentration_of_chlorophyll_a",
  projection = "regular_latitude_longitude_grid",
  year = years[[i]],
  month = monthss[[i]],
  day = days,
  version = "5.0",
  dataset_short_name = "satellite-ocean-colour",
  target = paste(years[[i]], "Concentration_chlorophylle_a.zip", sep = "-")
)}

SST <- list()
for(i in 1:length(years)){
  SST[[i]] <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = "sea_surface_temperature",
  year = years[[i]],
  month = monthss[[i]],
  day = days,
  time = times,
  area = area,
  dataset_short_name = "reanalysis-era5-single-levels",
  target = paste(years[[i]], "Sea_surface_temperature.nc", sep = '-')
)}

bathymetry <- list()
for(i in 1:length(years)){
  bathymetry[[i]] <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = "model_bathymetry",
  year = years[[i]],
  month = monthss[[i]],
  day = days,
  time = times,
  area = area,
  dataset_short_name = "reanalysis-era5-single-levels",
  target = paste(years[[i]], "Bathymetry.nc", sep = '-')
)}

SSH <- list()
for(i in 1:length(years)){
  SSH[[i]] <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = "significant_height_of_wind_waves",
  year = years[[i]],
  month = monthss[[i]],
  day = days,
  time = times,
  area = area,
  dataset_short_name = "reanalysis-era5-single-levels",
  target = paste(years[[i]], "Significant_height_of_winf_waves.nc", sep = '-')
)}

east_wind <- list()
for(i in 1:length(years)){
  east_wind[[i]] <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = "10m_u_component_of_neutral_wind",
  year = years[[i]],
  month = monthss[[i]],
  day = days,
  time = times,
  area = area,
  dataset_short_name = "reanalysis-era5-single-levels",
  target = paste(years[[i]], "Wind_neutral_eastward.nc", sep = '-')
)}

north_wind <- list()
for(i in 1:length(years)){
  north_wind[[i]] <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = "10m_v_component_of_neutral_wind",
  year = years[[i]],
  month = monthss[[i]],
  day = days,
  time = times,
  area = area,
  dataset_short_name = "reanalysis-era5-single-levels",
  target = paste(years[[i]], "Wind_neutral_northward.nc", sep = '-')
)}

geostrophic_current <- list()
for(i in 1:length(years)){
 geostrophic_current[[i]] <- list(
  variable = "all",
  format = "zip",
  year = years[[i]],
  month = monthss[[i]],
  day = days,
  dataset_short_name = "satellite-sea-level-global",
  version = "vDT2021",
  target = paste(years[[i]], "Geostrophic_current_and_others.zip", sep = '-')
)}

########## Connexion au serveur Copernicus ####
wf_set_key(user = "72932", # UID sur profile Copernicus
           key = "1a607273-1ccb-45ff-8a01-795e5ac682ab", # Sur profil Copernicus
           service = "cds") # webapi quand user est un email

########## Téléchargement des variables SST
# for(i in 1:length(years)){
# # Formatage de la requête
#   SST <- list(
#     product_type = "reanalysis",
#     format = "netcdf",
#     variable = "sea_surface_temperature",
#     year = years[[i]],
#     month = monthss[[i]],
#     day = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", 
#             "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31"),
#     time = c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", 
#              "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"),
#     area = c(31, 10, -55, 155), # LON EAST, LAT NORTH, LAT SOUTH, LON WEST limits
#     dataset_short_name = "reanalysis-era5-single-levels",
#     target = paste(years[[i]], "Sea_surface_temperature.nc", sep = '-')
#   )
  

  
######## Téléchargement des variables ####
  request <- c(
               # SSH, # doesn't work - internal error - retry after Python installation - OK
               # SST, # already done
               # chloro_a, # doesn't work - internal error - retry after Python installation - "Error: Client has not agreed to the required terms and conditions" -
               # bathymetry, # OK
               # east_wind#, # doesn't work - internal error - retry after Python installation - OK
               # north_wind, # OK
               # geostrophic_current # doesn't work - request not valid cause the version (vDT2021) was missing
               )
  
for (i in 1:length(request)){
  
  file <- wf_request(user = "72932", 
                     request  = request[[i]],
                     transfer = TRUE, #download the file
                     path     = dossier_vars, #store datas
                     verbose = TRUE) 
  
  print(paste("Téléchargement réalisé pour", i, "variable sur", length(request), seq = ""))
  
}
  

