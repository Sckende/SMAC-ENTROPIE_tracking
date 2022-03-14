#### Production de gifs pour présentatioon à Henry ####

library(gifski)

# -----> Perturbation SUD AVRIL 2018 ####
png_files <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/MAPS/FAKIR_6hours/ZONE_SUD/",
                        pattern = ".*png$",
                        full.names = TRUE)
gifski(png_files,
       gif_file = "C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/Meeting_Henri_MARS_2022/ZONE_SUD.gif",
       width = 4724,
       height = 5905,
       delay = 0.5)

# -----> Période de FAKIR 20-24 AVRIL 2018 ####
# EXTRA LONG !
png_files_TRACK <- list.files("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/Meeting_Henri_MARS_2022/FAKIR",
                        # pattern = ".*png$",
                        full.names = FALSE)
for (i in png_files_TRACK) {
    png_files <- list.files(paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/Meeting_Henri_MARS_2022/FAKIR/",
                                  i,
                                  "/",
                                  sep = ""),
                        pattern = ".*png$",
                        full.names = TRUE)
    gifski(png_files,
       gif_file = paste("C:/Users/ccjuhasz/Desktop/Meeting_H_Weimerskirch/Meeting_Henri_MARS_2022/FAKIR-",
                        i,
                        ".gif",
                        sep = ""),
       width = 4724,
       height = 5905,
       delay = 0.5)
}

