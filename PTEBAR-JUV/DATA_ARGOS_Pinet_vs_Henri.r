# Comparaison des deux jeux de données pour identifier l'origine des gaps 

source("C:/Users/ccjuhasz/Desktop/SMAC/GITHUB/SMAC-ENTROPIE_tracking/PTEBAR-JUV/packages_list.r")

# ----- Load & treatment of data ----- ####
henri <- read.table("C:/Users/ccjuhasz/Desktop/PTEBAR_juv_argos_Henri.txt",
                    sep = "\t",
                    h = T)
names(henri)
henri_sf <- st_as_sf(henri,
                      coords = c("Longitude", "Latitude"),
                      crs = 4326)
henri_sf$Vessel <- as.factor(henri_sf$Vessel)
henri_sf$Date <- as.POSIXct(henri_sf$Date,
                            format = '%d/%m/%Y %H:%M')
henri_ls <- split(henri_sf, henri_sf$Vessel)

pinet <- readRDS("C:/Users/ccjuhasz/Desktop/SMAC/Projet_publi/X-PTEBAR_argos_JUV/DATA/PTEBAR_JUV_argos_with_env_DATA_wind_dirs_n_abs_speed_2.rds")

pinet_sf <- st_as_sf(pinet,
                     coords = c("Longitude", "Latitude"),
                     crs = 4326)
pinet_sf$Vessel <- as.factor(pinet_sf$Vessel)
pinet_sf$Date <- as.POSIXct(pinet_sf$Date,
                            format = "%Y-%m-%d %H:%M:%S")
pinet_ls <- split(pinet_sf, pinet_sf$Vessel)

# ----- #
dim(pinet_sf)
dim(henri_sf)

# ----- 162070 ----- #
henri_070 <- henri_ls[["162070"]]
max(henri_070$Date, na.rm = TRUE)

pinet_070 <- pinet_ls[["162070"]]
pinet_070 <- pinet_070[pinet_070$Date <= max(henri_070$Date, na.rm = TRUE), ]

dim(henri_070)
dim(pinet_070)

mapview(henri_070) + mapview(pinet_070, color = "red")

# mapview(argos_sf,
#         zcol = 'Vessel',
#         burst = T)


loc_delay <- lapply(pinet_ls, function(x) {
    x <- x[order(x$Date),]
    x$loc_delay <- difftime(x$Date,
             c(x$Date[-1], NA),
             units = "hours")*-1
    x
})
lapply(loc_delay, function(x) {
    table(round(x$loc_delay))
})
table(round(loc_delay[["162070"]]$loc_delay))

lapply(loc_delay, function(x) {
    print(unique(x$Vessel))
    print(table(round(x$loc_delay)))
})

# ----- 162072 ----- #
delay_072 <- pinet_ls[["162072"]]
delay_072[delay_072$delay < -62, ]

d1 <- delay_072[delay_072$Date < as.POSIXct("2018-04-09 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d1)
d2 <- delay_072[delay_072$Date < as.POSIXct("2018-05-04 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d2)
d3 <- delay_072[delay_072$Date < as.POSIXct("2018-05-21 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d3)
d4 <- delay_072[delay_072$Date < as.POSIXct("2018-08-08 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d4)

# ----- 162073 ----- #
delay_073 <- pinet_ls[["162073"]]
delay_073[delay_073$delay < -58, ]

d1 <- delay_073[delay_073$Date < as.POSIXct("2018-04-11 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d1)
d2 <- delay_073[delay_073$Date < as.POSIXct("2018-05-05 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d2)
d3 <- delay_073[delay_073$Date < as.POSIXct("2018-08-25 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d3)

# ----- 166561 ----- #
delay_561 <- pinet_ls[["166561"]]
delay_561[delay_561$delay < -68, ]

d1 <- delay_561[delay_561$Date < as.POSIXct("2018-04-12 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d1)
d2 <- delay_561[delay_561$Date < as.POSIXct("2018-05-05 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d2)
d3 <- delay_561[delay_561$Date < as.POSIXct("2018-11-13 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d3)
d4 <- delay_561[delay_561$Date < as.POSIXct("2018-11-16 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d4)
d5 <- delay_561[delay_561$Date < as.POSIXct("2018-12-05 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d5)
d6 <- delay_561[delay_561$Date < as.POSIXct("2018-12-13 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d6)

# ----- 166563 ----- #
delay_563 <- pinet_ls[["166563"]]
delay_563[delay_563$delay < -59, ]

d1 <- delay_563[delay_563$Date < as.POSIXct("2018-04-08 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d1)
d2 <- delay_563[delay_563$Date < as.POSIXct("2018-05-05 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d2)
d3 <- delay_563[delay_563$Date < as.POSIXct("2018-07-07 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d3)
d4 <- delay_563[delay_563$Date < as.POSIXct("2018-08-25 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d4)

# ----- 166564 ----- #
delay_564 <- pinet_ls[["166564"]]
delay_564[delay_564$delay < -30, ]

d1 <- delay_564[delay_564$Date < as.POSIXct("2018-04-13 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d1)
d2 <- delay_564[delay_564$Date < as.POSIXct("2018-05-05 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d2)

# ----- 166565 ----- #
delay_565 <- pinet_ls[["166565"]]
delay_565[delay_565$delay < -57, ]

d1 <- delay_565[delay_565$Date < as.POSIXct("2018-04-10 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d1)
d2 <- delay_565[delay_565$Date < as.POSIXct("2018-05-05 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d2)
d3 <- delay_565[delay_565$Date < as.POSIXct("2018-08-25 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d3)
d4 <- delay_565[delay_565$Date < as.POSIXct("2018-11-05 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d4)
d5 <- delay_565[delay_565$Date < as.POSIXct("2018-11-12 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d5)
d6 <- delay_565[delay_565$Date < as.POSIXct("2018-11-16 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d6)

# ----- 166568 ----- #
delay_568 <- pinet_ls[["166568"]]
delay_568[delay_568$delay < -34, ]

d1 <- delay_568[delay_568$Date < as.POSIXct("2018-04-18 00:00:00",
                                       format = "%Y-%m-%d %H:%M:%S"), ]
tail(d1)
