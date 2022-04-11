library(dismo)
library(rgeos)
setwd("C:\\Users\\searc\\Desktop\\Demo\\Species_Distribution_Modeling\\SDM\\bioclim") #setting working directory
lantana=read.csv("lantana_camara.csv")

head(lantana)
lantana1= lantana[,-1] #we do not need the first column, that’s why we use -1
ext = extent(99, 105, 1.2, 6.7) ##geographic extent of study area i.e. longitude 1, longitude 2, latitude 1, latitude 2
library(rgdal)
all.worldclim = raster::getData("worldclim", res = 10, var = 'bio')
msia.worldclim = crop(all.worldclim, extent(99, 105, 1.2, 6.7))
h.extent = extent(min(lantana1$long -1), ## defining the map's bounding box (+1/-1 degrees)
max(lantana1$long + 1),
min(lantana1$lat - 1),
max(lantana1$lat + 1))
h.map = gmap(h.extent, type = 'satellite', latlon = TRUE)
plot(h.map)
points(Mercator(lantana1[,c('long','lat')]), pch = 16, col = 'red')
h.bc = bioclim(msia.worldclim, lantana1[,c('long','lat')]) ## the bioclim function takes the climate layers and the species long and lat columns 
par(mfrow = c(4,4))
response(h.bc)
par(mfrow = c(1,1))
lantana.d<- bioclim(msia.worldclim, lantana1[,c('long','lat')])
par(mfrow = c(4,4))
response(lantana.d)
par(mfrow = c(1,1))
lantana.d.pred<- predict(object = lantana.d, msia.worldclim)
plot(lantana.d.pred, main = 'sdm predictions using climate layers')





head(lantana1)
plot(msia.worldclim) ## environmental predictors
lantana.d = bioclim(msia.worldclim, lantana1[,c('long','lat')]) ## bioclimatic model
ext = extent(99, 105, 1.2, 6.7)
backg = randomPoints(msia.worldclim, n=1000,ext=ext, extf = 1.25) #background/pseudo-absence data
e = evaluate(lantana1, backg, lantana.d, msia.worldclim) #four arguments namely presence, background, model and predictors.
#presence, background, model, predictors
e
plot(e, 'ROC')
