#####################################################################
# https://www.rdocumentation.org/packages/SDLfilter/versions/2.1.1 #
###################################################################
library(SDLfilter)

# Loading data 
data(turtle)
names(turtle)
head(turtle)
class(turtle$DateTime)

# Remove temporal and spatial duplicates
dim(turtle)
dup <- turtle[duplicated(turtle$DateTime),]
turtle[turtle$DateTime == dup$DateTime[1],]

turtle.dup <- dupfilter(turtle)
dim(turtle.dup)

# Calculate the maximum linear speed between two consecutive locations
V <- vmax(turtle.dup)  

# Calculate the maximum one-way linear speed of a loop trip
VLP <- vmaxlp(turtle.dup) 

## Run ddfilter
turtle.dd <- ddfilter(turtle.dup,
                      vmax = V,
                      vmaxlp = VLP)
