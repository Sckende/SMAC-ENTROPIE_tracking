# ------------------------------------- #
#### GPS fixes frequencies function ####
# ----------------------------------- #
# allows the computation of number of GPS fixes per day
# x = df with a 'date' variable

# For first PETBAR GPS data, date range =  
# test <- function(x){
#   x$date <- as.Date(strftime(x$time, "%Y-%m-%d"))
#   sequen <- data.frame(date = seq.Date(from = min(x$date), to = max(x$date), by = 1), n = 0)
#   
#   mm <- dplyr::count(x, date)
#   mm <- rbind(mm, sequen[!(sequen$date %in% mm$date),])
#   mm <- mm[order(mm$date, decreasing = F),]
#   mm <- cbind(Logger_ID = unique(x$Logger_ID), mm)
#   return(mm)
# }

# For first PETBAR GPS data
# x = df with a 'date' variable, date_min & date_max with a "%Y-%m-%d" format
# test_V2 <- function(x, date_min, date_max){
#   x$date <- as.Date(strftime(x$time, "%Y-%m-%d"))
#   sequen <- data.frame(date = seq.Date(from = as.Date(strftime(date_min, "%Y-%m-%d")), to = as.Date(strftime(date_max, "%Y-%m-%d")), by = 1), n = 0)
#   
#   xx <- x[x$date %in% sequen$date,]
#   mm <- dplyr::count(xx, date)
#   mm <- rbind(mm, sequen[!(sequen$date %in% mm$date),])
#   mm <- mm[order(mm$date, decreasing = F),]
#   mm <- cbind(Logger_ID = unique(x$Logger_ID), mm)
#   return(mm)
# }

# Last version of function with NA integration
# x is dataframe with a minimum of 'Logger_ID' and 'time' variables
fix_freq <- function(x, date_min, date_max, NA_use = TRUE){
  
  if(!lubridate::is.POSIXct(x$time)){
    stop('Time has to correspond to a POSIXct class')
  }
  if(!lubridate::is.POSIXct(date_min)){
    stop('Date_min has to correspond to a POSIXct class')
  }
  if(!lubridate::is.POSIXct(date_max)){
    stop('Date_max has to correspond to a POSIXct class')
  }
  
  sequen <- data.frame(date = seq.Date(from = lubridate::date(date_min),
                                       to = lubridate::date(date_max),
                                       by = 1),
                       n = 0,
                       row.names = NULL)
  # For all fixes 
  xx <- x[lubridate::date(x$time) %in% sequen$date,]
  mm <- dplyr::count(xx, lubridate::date(xx$time))
  names(mm)[1] <- 'date'
  mm1 <- rbind(mm, sequen[!(sequen$date %in% mm$date),])
  
  # For fixes with NA in Lon/Lat
  x_NA <- x[is.na(x$Latitude),]
  xx_NA <- x_NA[lubridate::date(x_NA$time) %in% sequen$date,]
  mm_NA <- dplyr::count(xx_NA, lubridate::date(xx_NA$time))
  names(mm_NA)[1] <- 'date'
  mm_NA1 <- rbind(mm_NA, sequen[!(sequen$date %in% mm_NA$date),])
  names(mm_NA1)[2] <- 'n_NA'
  
  if(NA_use == TRUE){
    df <- cbind(unique(x$Logger_ID), merge(mm1, mm_NA1))
    names(df)[1] <- 'Logger_ID'
    return(df)
  }else{
    df <- cbind(unique(x$Logger_ID),mm1)
    names(df)[1] <- 'Logger_ID'
    return()
  }
  
}


#### Color function for life cycle periods ####

# Function which selects the color associated to the life cycle period of the PETBAR and based on the date
# x = vector of dates

col_choice <- function(x){
  # WARNING - periods selected here are extra specific to the first GPS data of PETBAR
  # post_dates <- seq.Date(as.Date(strftime('2018-05-01', "%Y-%m-%d")),
  #                        as.Date(strftime('2018-08-30', "%Y-%m-%d")),
  #                        by = 1)
  # prospec_dates <- seq.Date(as.Date(strftime('2018-09-01', "%Y-%m-%d")),
  #                           as.Date(strftime('2018-10-31', "%Y-%m-%d")),
  #                           by = 1)
  # inc_dates <- seq.Date(as.Date(strftime('2018-11-01', "%Y-%m-%d")),
  #                       as.Date(strftime('2018-12-31', "%Y-%m-%d")),
  #                       by = 1)
  # rear_dates <- seq.Date(as.Date(strftime('2019-01-01', "%Y-%m-%d")),
  #                        as.Date(strftime('2019-04-30', "%Y-%m-%d")),
  #                        by = 1)
  pal <- viridis::viridis(4) # 1 color per period i.e., winter, prosp, incub, rearing
  
  colors <- NULL
  
  for(i in 1:length(x)){
    
    if(lubridate::month(x[i]) %in% 5:8){
      col <- pal[1]
    } else if(lubridate::month(x[i]) %in% 9:10){
      col <- pal[2]
    } else if(lubridate::month(x[i]) %in% 11:12){
      col <- pal[3]
    } else {
      col <- pal[4]
    }
    colors <- c(colors, col)
  }
  return(colors)
}

# Function test 
# dates <- seq.Date(as.Date(strftime('2018-05-01', "%Y-%m-%d")),
#                   as.Date(strftime('2019-04-30', "%Y-%m-%d")),
#                   by = 1)
# dates
# df <- data.frame(date = dates, n = sample(1:1000, length(dates)))
# barplot(df$n,
#         col = col_choice(df$date))

# --------------------------------------- #
#### Barplot of gps fixes frequencies ####
# ------------------------------------- #

# x = df with variables 'date', 'n', 'Logger_ID' and 'n_NA'
# use_NA = TRUE if we want to see the frequence of NA for Lon/Lat
barp_fix_freq <- function(x, use_NA = FALSE){
  require(plotly)
  pal <- viridis::viridis(4) # only for the legend
  par(oma = c(0,0,0,0)) # Set right margin
  
  if(use_NA == FALSE) print('Sure do not want to see NAs ?')
  
  # barplot(x$n,
  #         names.arg = x$date,
  #         ylim = c(0, max(x$n) + 10),
  #         main = unique(x$Logger_ID),
  #         las = 2,
  #         cex.names = 0.8,
  #         col = col_choice(x$date),
  #         ylab = 'GPS fixes number')
  
  if(use_NA == TRUE){
    
    # barplot(x$n_NA,
    #         col = 'red',
    #         axes = F,
    #         add = T)
    # 
    # legend('top',
    #        legend = c('non-breed', 'prosp', 'incub', 'rear', 'NA'),
    #        fill = c(pal, '#FF0000'),
    #        bty = 'n',
    #        ncol = 2) 
    fig <- plot_ly(x, x = ~date, y = ~n_NA, type = 'bar', name = 'Fixes with NA', marker = list(color = 'red'))
    fig <- fig %>% add_trace(y = x$n - x$n_NA, name = '', marker = list(color = col_choice(x$date)))
    fig <- fig %>% layout(yaxis = list(title = 'Count'),
                          barmode = 'stack',
                          showlegend = T)
    fig <- fig %>% layout(legend = list(title = list(text = '<b> Fixes types </b>')),
                          title = list(text = unique(x$Logger_ID)))
    fig
    
  } else {
    
    # legend('top',
    #        legend = c('non-breed', 'prosp', 'incub', 'rear'),
    #        fill = pal,
    #        bty = 'n',
    #        ncol = 2) 
    fig <- plot_ly(x, x = ~date, y = x$n, name = '', type = 'bar', marker = list(color = col_choice(x$date)))
    fig <- fig %>% layout(yaxis = list(title = 'Count'),
                          showlegend = F)
    fig <- fig %>% layout(legend = list(title = list(text = '<b> Fixes types </b>')),
                          title = list(text = unique(x$Logger_ID)))
    fig
  }
}


#### Conversion of POINT geometry variable from sf dataframe object to couple of coordinates ####

recup_coord <- function(geom_var){
  c <- as.character(geom_var) # geom_var correspond à la colonne geom du df de classe sf
  cc <- NULL
  for (i in 1:length(c)){
    cut <- stringr::str_sub(c[i], 3, stringr::str_length(c[i])-1)
    cutcut <- as.numeric(stringr::str_split(cut, ',', simplify = T))
    cc <- rbind(cc, cutcut)
  }
  cc <- as.data.frame(cc, row.names = F)
  names(cc) <- c('Longitude', 'Latitude')
  
  return(cc)
}