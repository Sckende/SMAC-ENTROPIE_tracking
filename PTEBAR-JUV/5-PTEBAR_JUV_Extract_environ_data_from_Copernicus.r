#############################################################################################################################################################
#                                                                                                                                                      #
#                                                  SCRIPT 1 : Téléchargement des données environementales                                              #
#                                                               BASED ON: PNB 2021, Romain FERNANDEZ                                                             #
#                                                                                                                                                      #
#############################################################################################################################################################



############################################################# DONNEES POUR LA PARTIE 2 DU SCRIPT #######################################################

# Installer python 3.6 ou 3.7 (https://www.python.org/downloads/) et motuclient 1.8.4 en lancant dans le terminale python la commande : python -m pip install motuclient 
# install.packages("devtools") # Pour permettre l'installation du package RCMEMS via Github 
# Devtools utilise Rtools qui doit être installé et ajouté aux path du système : https://cran.r-project.org/bin/windows/Rtools/. 
# Attention aux compatibilités des versions Rtools et de R (Cf. lien ci-dessus)
rm(list = ls())
# library(devtools)

# Installer le package développé sur Github, soumis sur CRAN en mars 2020 mais pas encore sortie
# devtools::install_github("markpayneatwork/RCMEMS", force = TRUE) 
library(RCMEMS)
library(lubridate)
# Stocker le chemin de l'espace de travail
# wd <- getwd()

# Loging à la plateforme Copernicus marine
source('C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/Copernicus_id.r')
user_name <- id
mdp <- pwd

# Chemin de stockage des fichiers exportés
# dossier_vars = "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R"
dossier_vars = "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/ENV_DATA_Romain/Output_R/SST_2018-2019/"


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

# Informations variables
link_tab_parm <- "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/5-PTEBAR_argos_JUV/ENV_DATA_Romain/Input_R" # Chemin du tableau contenant les paramètres des variables
# nom_fichier_tab <- "Parametres_5_variables.csv"   # Nom du fichier contenant le tableau avec les paramètres des variables (.csv, séparateur point-virgule)
# nom_fichier_tab <- "Parametres_WIND_ORIENTATION.csv"
# nom_fichier_tab <- "Parametres_chlo_JANFEVMAR_2018.csv"
# nom_fichier_tab <- "Parametres_chlo_JANFEVMAR_OCTNOVDEC_2017.csv"
# nom_fichier_tab <- "Parametres_WIND_2008-2018_2.csv"
nom_fichier_tab <- "Parametres_SST_PNB_ISOSCAPE.csv"


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


########## Télécharger les variables
for (i in 1:length(vars)){
  
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


######################## Prolongation du script pour télécharger les layers manquantes en fin de chaque mois pour les variables enregistrées au 6h #######################
# WIND_SPEED, WIND_NORTH, WIND_EAST

########## Créer les vecteurs issus des différentes colonnes du tableau de paramètrage des variables
tab_parm <- read.csv(paste(link_tab_parm, nom_fichier_tab, sep = "/"), head = TRUE, na.strings = "" ,  sep = ";")

# selection des variables d'intérêt
tab_parm2 <- tab_parm[tab_parm$Variable_code %in% c("wind_speed", "northward_wind", "eastward_wind"),]
dim(tab_parm2)
# modification des noms de fichiers
tab_parm2$Variable <- paste(tab_parm2$Variable, '-suite', sep = '')

# Modification des dates
tab_parm2$Date_min <- tab_parm2$Date_max
tab_parm2$Date_min <- as.Date(tab_parm2$Date_min, format = "%d/%m/%Y")
tab_parm2$Date_max <- date(tab_parm2$Date_min) + 1

tab_parm2$Motu <- as.character(tab_parm2$Motu)
lien_motu <- c(tab_parm2$Motu)

tab_parm2$Service_id <- as.character(tab_parm2$Service_id)
ID_service <- c(tab_parm2$Service_id)

tab_parm2$Product_id <- as.character(tab_parm2$Product_id)
ID_produit <- c(tab_parm2$Product_id)

# tab_parm2$Date_min <- as.POSIXct(tab_parm2$Date_min, format = "%d/%m/%Y %H:%M:%S")
# tab_parm2$Date_min <- as.Date(tab_parm2$Date_min, format = "%d/%m/%Y")
date_min <- as.character(tab_parm2$Date_min)
date_min <- c(date_min)

# tab_parm2$Date_max <- as.POSIXct(tab_parm2$Date_max, format = "%d/%m/%Y %H:%M:%S")
# tab_parm2$Date_max <- as.Date(tab_parm2$Date_max, format = "%d/%m/%Y")
date_max <- as.character(tab_parm2$Date_max)
date_max <- c(date_max)

tab_parm2$Variable_code <- as.character(tab_parm2$Variable_code)
vars <- c(tab_parm2$Variable_code) 

tab_parm2$Variable <- as.character(tab_parm2$Variable)
names_vars <- c(tab_parm2$Variable)


########## Télécharger les variables
# --> cf première partie du script

####################### EXTRA PART : Creation du fichier "Parametres_WIND_2008-2018.csv" #######################

day <- "01"
month_min <- rep(c("01", "05", "09"), 11)
month_max <- rep(c("05", "09", "01"), 11)
year_min <- rep(2008:2018, each = 3)
year_max <- c(year_min[-1], 2019)
variable <- paste(rep(c("eastward_wind",
                        "northward_wind",
                        "wind_speed"),
                      each = 33),
                  rep(year, 3),
                  rep(1:3, 3),
                  sep = "-")

date_min <- paste(day, month_min, year_min, sep = "/")
date_max <- paste(day, month_max, year_max, sep = "/")

param <- data.frame(Variable = variable,
                    Motu = "https://my.cmems-du.eu/motu-web/Motu",
                    Variable_code = rep(c("eastward_wind",
                                          "northward_wind",
                                          "wind_speed"),
                                        each = 33),
                    Product_id = "CERSAT-GLO-BLENDED_WIND_L4_REP-V6-OBS_FULL_TIME_SERIE",
                    Service_id = "WIND_GLO_WIND_L4_REP_OBSERVATIONS_012_006-TDS",
                    Date_min = date_min,
                    Date_max = date_max,
                    CRS = 4326)

# write.table(param,
#           "C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/ENV_DATA_Romain/Input_R/Parametres_WIND_2008-2018.csv",
#           sep = ";")