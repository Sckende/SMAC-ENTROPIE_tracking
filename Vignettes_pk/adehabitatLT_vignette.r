#### adehabitatLT vignettes ####
# see 'Analysis if animal movement in R: the adehabitatLT package' - Calenge 2015
require(adehabitatLT)

data("puechabonsp") # trajectories of 4 boars
locs <- puechabonsp$relocs
locs <- as.data.frame(locs)

head(locs)

# Conversion of dates
dat <- as.character(locs$Date)
head(dat)

dat <- as.POSIXct(strptime(dat, '%y%m%d'))
head(dat)

# Then it's possible to create an object of class ltraj to store movements
puech <- as.ltraj(xy = locs[, c('X', 'Y')],
                  date = dat,
                  id = locs$Name)
puech # list of class ltraj with 4 bursts of relocations corresponding to 4 animals. Trajectories are of type II and irregular, without missing value. dx, dy, dist in meters and abs.angle, rel.angle in radians are automatically computed

plot(puech[[1]])
plot(puech)

head(puech[[1]])

#### Managing objects of class ltraj ####
#### ---- Cutting a burst into several segments ---- 
is.regular(puech) # non-regular object

# Causes of non-regularity ?
# Visualization of dt (time between two relocations)
# Because dt is in seconds and that no more than one reloc is coellected every day, we convert this time lag into days
plotltr(puech, 'dt/3600/24') # ind 3 was monitored during two succcessive summers and we want to cut its burst into 2 sub-bursts with the function cutltraj() 
    # FIRST, definition of function returning TRUE when time between two relocations is greater than 100 days

foo <- function(dt){
  return(dt/3600/24 > 100)
}

puech2 <- cutltraj(puech, 'foo(dt)', nextr = T)
puech2

x11();plotltr(puech2)
burst(puech2) # give the names of bursts
burst(puech2)[3:4] <- c('Chou.1992', 'Chou.1993')

#### ---- Playing with bursts ----

# subset of ltraj object

puech2b <- puech2[c(1,2,5)]

# Selection of bursts satisfying conditions
bu <- which.ltraj(puech2, 'dist>2000') # bursts with distance > 2000m between two relocs, at least once - Df contains ID, burst ID and relocation NUMBERS satisfying the specified criterion.

puech2[burst(puech2) %in% bu$burst] # extraction of the burst satisfying the criterion

#### ---- Placing the missing values in trajectories ----
# In this example, relocations have been collected daily excepted when it was not possible. We need to add missing values to define a regular trajectory. 

# FIRST, definition of a reference date
refdat <- strptime('00:00', '%H:%M')
refdat

# This reference date will be used to check that each date in the object of class ltraj is separated from this reference by an integer multiple of the theoretical dt (here, one day), and place the missing values at the times when relocations should theoretically have been collected.

puech3 <- setNA(puech2,
                refdat,
                1,
                units = 'day')
puech3 # Trajectories are now regular but with NAs

#### ---- Rounding the timing of the trajectories to define a regular trajectory ----
# In some cases, despite the fact that the relocations were expected to be collected to return a regular trajectory, a minor delay is sometimes observed in this timing (e.g. the GPS collar needs some time to relocate).

data(ibexraw)
ibexraw
class(ibexraw)
plot(ibexraw)

# Looking at the time lag between successive relocations
plotltr(ibexraw, 'dt/3600') #The relocations should have been collected every 4 hours, but there are some missing values. Use the function setNA to place the missing values, as in the section 3.3. We define a reference date and place the missing values

refdat <- strptime('2003-06-01 00:00',
                   '%Y-%m-%d %H:%M',
                   tz = 'Europe/Paris')
ib2 <- setNA(ibexraw,
             refdat,
             4,
             units = "hour")
ib2 

plotltr(ib2, 'dt/3600')# even when filling the gaps with NAs, the trajectory is still not regular. We can see that the time lag is only slightly dfferent from 4 hour. The function sett0 can be used to \round" the timing of the coordinates.

ib3 <- sett0(ib2,
             refdat,
             4,
             units = 'hour')
plotltr(ib3, 'dt/3600') # regular trajectories !
 # ***WARNING*** - The functions setNA and sett0 are to be used to define a regular trajectory from a nearly regular trajectory. It is NOT intended to transform an irregular trajectory into a regular one (many users of adehabitat asked this question).

#### ---- Special type of trajectory: same duration ----
# In some cases, an object of class ltraj contains several regular bursts of the same duration characterized by relocations collected at the same time (same time lags between successive relocations, same number of relocations).
# To check if an object is like that

is.sd(ib3) # sd for 'same distance' - This object is not of the type sd (same duration). However, theoretically, all the trajectories should have been sampled at the same time points. It is regular, but there are mismatches between the time of the relocations. Because there are missing values at the beginning/end of the monitoring
#We can use the function set.limits to dene the time of beginning and ending of the trajectories. This function adds NAs to the beginning and ending of the monitoring when required.

ib4 <- set.limits(ib3,
                  begin = '2003-06-01 00:00',
                  dur = 14,
                  units = 'day',
                  pattern = '%Y-%m-%d %H:%M',
                  tz = 'Europe/Paris')
ib4 # all trajectories are now covering the same period
is.sd(ib4)

#  then it's possible to store some parameters of sd objects into a data frame with one reloc per row and one burst per column
di <- sd2df(ib4, 'dist')
head(di)

sddt <- sd2df(ib4, 'rel.angle')
head(sddt)

sdr2n <- sd2df(ib4, 'R2n')
head(sdr2n)

#### ---- Metadata on the trajectories ----
# it's possible to stock extra information about relocations in the ltraj object

data(capreochiz)
head(capreochiz)

capreo <- as.ltraj(xy = capreochiz[, c('x', 'y')],
                   date = capreochiz$date,
                   id = 'Roe.Deer',
                   infolocs = capreochiz[, 4:8])
capreo

inf <- infolocs(capreo)
head(inf[[1]]) # The function removeinfo can be used to set the attribute infolocs of all bursts to NULL.

plotltr(capreo, 'log(Dop)')


#### Analyzing the trajectories ####
