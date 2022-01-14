#############################################################################################################################################################
#                                                                                                                                                      #
#                                                  SCRIPT 1 : Téléchargement des données environementales                                              #
#                                                               BASED ON: PNB 2021, Romain FERNANDEZ                                                             #
#                                                                                                                                                      #
#############################################################################################################################################################



############################################################# DONNEES POUR LA PARTIE 2 DU SCRIPT #######################################################

# Installer python 3.6 ou 3.7 (https://www.python.org/downloads/) et motuclient 1.8.4 en lancant dans le terminale python la commande : python -m pip install motuclient 
install.packages("devtools") # Pour permettre l'installation du package RCMEMS via Github 
# Devtools utilise Rtools qui doit être installé et ajouté aux path du système : https://cran.r-project.org/bin/windows/Rtools/. 
# Attention aux compatibilités des versions Rtools et de R (Cf. lien ci-dessus)

library(devtools)

# Installer le package développé sur Github, soumis sur CRAN en mars 2020 mais pas encore sortie
# devtools::install_github("markpayneatwork/RCMEMS", force = TRUE) 
library(RCMEMS)
# Stocker le chemin de l'espace de travail
wd <- getwd()

# Loging à la plateforme Copernicus marine
source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/Copernicus_id.r')
user_name <- id
mdp <- pwd

# Chemin de stockage des fichiers exportés
dossier_vars = "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R"

# Paramètres d'emprise des variables à télécharger
long_min = "10"         # longitude minimale de la zone à télécharger
long_max = "155"        # longitude maximale de la zone à télécharger
lat_min = "-55"         # latitude minimale de la zone à télécharger
lat_max = "31"          # latitude maximale de la zone à télécharger

# Préparer l'import du tableau de paramètrage des variables, qui a comme colonnes :
### "Variable" : Nom donné à la variable. Si même variable numérotée en plusieurs fichiers séparer nom avriable et numéro par un "_" (Ex : [..]__ZOOC_1.nc, [..]__ZOOC_2.nc...)
### "Motu" : A récupérer sur Copernicus (code Python)
### "Variable_code" : A récupérer sur Copernicus (code Python)
### "Product_id" : A récupérer sur Copernicus (code Python)
### "Service_id" : A récupérer sur Copernicus (code Python)
### "Date_min" : Format jour/mois/année (format Français)
### "Date_max" : Format jour/mois/année  (format Français)
### "Depth_min" et "Depth_max": Intervalle de profondeur à télécharger (points et pas des virgules dans le fichier), laisser vide si pas de valeurs  

# Informations variables
# link_tab_parm <- "F:/PNB_ENTROPIE/Input_R"     # Chemin du tableau contenant les paramètres des variables
link_tab_parm <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Input_R"
nom_fichier_tab <- "Parametres_variables_test1.csv"   # Nom du fichier contenant le tableau avec les paramètres des variables (.csv, séparateur point-virgule)


####################### DEBUT PARTIE 2 DU SCRIPT : Téléchargement variables sur Copernicus Marine Environment Monitoring Service #######################

########## Créer les vecteurs issus des différentes colonnes du tableau de paramètrage des variables
tab_parm <- read.csv(paste(link_tab_parm, nom_fichier_tab, sep = "/"), head = TRUE, na.strings = "" ,  sep = ";")

tab_parm$Motu <- as.character(tab_parm$Motu)
lien_motu <- c(tab_parm$Motu)

tab_parm$Service_id <- as.character(tab_parm$Service_id)
ID_service <- c(tab_parm$Service_id)

tab_parm$Product_id <- as.character(tab_parm$Product_id)
ID_produit <- c(tab_parm$Product_id)

tab_parm$Date_min <- as.Date(tab_parm$Date_min, format = "%d/%m/%Y")
date_min <- as.character(tab_parm$Date_min)
date_min <- c(date_min)

tab_parm$Date_max <- as.Date(tab_parm$Date_max, format = "%d/%m/%Y")
date_max <- as.character(tab_parm$Date_max)
date_max <- c(date_max)

tab_parm$Variable_code <- as.character(tab_parm$Variable_code)
vars <- c(tab_parm$Variable_code) 

tab_parm$Variable <- as.character(tab_parm$Variable)
names_vars <- c(tab_parm$Variable)

tab_parm$Depth_min <- as.character(tab_parm$Depth_min)
depth_min <- c(tab_parm$Depth_min)

tab_parm$Depth_max <- as.character(tab_parm$Depth_max)
depth_max <- c(tab_parm$Depth_max)

########## Télécharger les variables
for (i in 1:length(vars)){
  
  if (is.na(depth_max[i]) == TRUE){
    parm_vars <- RCMEMS::CMEMS.config(
      motu = lien_motu[i],
      python = "C:/Users/ccjuhasz/AppData/Local/Programs/Python/Python310/python.exe",
      script = "C:/Users/ccjuhasz/AppData/Local/Programs/Python/Python310/Scripts/motuclient-script.py",
      user = user_name,
      pwd = mdp,
      auth.mode = "cas",
      longitude.min = long_min,
      longitude.max = long_max,
      latitude.min = lat_min,
      latitude.max = lat_max,
      service.id = ID_service[i],
      product.id = ID_produit[i],
      date.min = date_min[i],
      date.max = date_max[i],
      variable = vars[i], 
      out.dir = dossier_vars,
      out.name = paste(ID_service[i], "__", names_vars[i], ".nc", sep ="")
    )
    
    RCMEMS::CMEMS.download(parm_vars)
    
    print(paste("Nom de la variable traitée : ", names_vars[i]))
    print(paste("Variable traitées :", i, "sur", length(vars), seq = ""))
  }
  
  if (is.na(depth_max[i]) == FALSE){
    parm_vars <- RCMEMS::CMEMS.config(
      motu = lien_motu[i],
      python = "C:/Users/ccjuhasz/AppData/Local/Programs/Python/Python310/python.exe",
      script = "C:/Users/ccjuhasz/AppData/Local/Programs/Python/Python310/Scripts/motuclient-script.py",
      user = user_name,
      pwd = mdp,
      auth.mode = "cas",
      longitude.min = long_min,
      longitude.max = long_max,
      latitude.min = lat_min,
      latitude.max = lat_max,
      service.id = ID_service[i],
      product.id = ID_produit[i],
      date.min = date_min[i],
      date.max = date_max[i],
      variable = vars[i],
      depth.min = depth_min[i],
      depth.max = depth_max[i],
      out.dir = dossier_vars,
      out.name = paste(ID_service[i], "__", names_vars[i], ".nc", sep ="")
    )
    
    RCMEMS::CMEMS.download(parm_vars)
    
    print(paste("Nom de la variable traitée : ", names_vars[i]))
    print(paste("Variable traitées :", i, "sur", length(vars), seq = ""))
  }
}























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
               chloro_a#, # doesn't work - internal error - retry after Python installation - "Error: Client has not agreed to the required terms and conditions" -
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
  

