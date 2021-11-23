rm(list = ls())
source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

argos <- do.call('rbind', readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_Pinet_data_CLEANED_speed.rds"))
head(argos)

# ----- #
# Computation of location buffer #
# ----- #

# 3 - < 250 m
# 2 - 250 - 500 m => 375
# 1 - 500 - 1500 m => 1000
# 0 - > 1500 m
# U, A & B - NA

argos$buffer <- NA

argos.list <- split(argos, argos$Class)

cl <- names(argos.list)
buf <- c(1500, 1000, 375, 250, 2000, 2000, NA)

argos.list2 <- lapply(argos.list, function(x){
  
  class <- unique(x$Class)
  i <- which(cl == class)
  
  x$buffer <- buf[i]
  
  x
})

tt <- do.call('rbind', argos.list2)
table(tt$buffer, tt$Class, useNA = 'always')
