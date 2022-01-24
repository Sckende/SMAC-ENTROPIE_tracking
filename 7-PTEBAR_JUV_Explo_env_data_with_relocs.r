argos <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA.rds")
summary(argos)
class(argos$Date)
names(argos)
dim(argos)

argos_list <- split(argos3, argos$Vessel)
lapply(argos_list, function(x){
    plot(x$wind_speed)
})

plot(argos_list[[1]]$wind_speed, type = 'l')
plot(argos_list[[1]]$CHLO, col = 'darkgreen', type = 'l')
plot(argos_list[[1]]$SST, col = 'darkorange', type = 'l')
View()
