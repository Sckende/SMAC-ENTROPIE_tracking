# ------------------------------------------------------------------------ #
#### First steps for the trajecory characterization of juvenile PTEBAR ####
# ---------------------------------------------------------------------- #

rm(list = ls())

library('mapview')
library('leaflet')
library('leafpop')
library('sf')
library('lubridate')
library('dplyr')
library('adehabitatHR')

#### Loading and treatment of data #### 
arg <- read.table("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_TRACK_argos.txt",
                 h = T,
                 sep = '\t',
                 dec = '.')[, -c(2, 6:9, 14:17)]
names(arg)
summary(arg)

arg$Vessel <- as.factor(arg$Vessel)
arg$DATE_1 <- as.POSIXct(arg$DATE_1,
                           format = "%d/%m/%Y %H:%M") # Date format
class(arg$DATE_1)

# --- Deletion of low quality data
table(arg$Class)
arg1 <- arg[arg$Class %in% c('0', '1', '2', '3', 'A', 'B'),]

# --- Check for duplicated data based on Vessel and DATE_1
arg.list <- split(arg1, arg1$Vessel)

lapply(arg.list, function(x){
  summary(as.numeric(diff(x$DATE_1)))
})

# Spatial point for each hour excepted for 162072, 166561, 166563, 166569, 166572
# To check case by case
# ---- 162072
head(arg1[arg1$Vessel == '162072',]) # First date impossible -> TO DELETE

