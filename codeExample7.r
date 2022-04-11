library(sp)
library(raster)
library(dismo)   # package to run the model
library(maptools)
datafiles = Sys.glob("*.tif") # file extension for our predictor files
resultingStack = stack() #empty stack for raster
for(i in 1:NROW(datafiles)){
tempraster = raster(datafiles[i])
stck = stack(stck,tempraster)
}
stck#raster predictors as a stackplot
plot(stck,2)
lantana=read.csv("lantana_camara.csv") # presence data
head(lantana)
lantana1= lantana[,-1] #first column not needed
ext = extent(99, 105, 1.2, 6.7) #study are extent
points(lantana1, col='blue') 
group = kfold(horn1, 5) #kfold cross-validation for model performance evaluation
pres_train = lantana1[group != 1, ]
pres_test = lantana1[group == 1, ]
backg = randomPoints(stck, n=1000,ext=ext, extf = 1.25)
colnames(backg) = c('lon', 'lat')
group = kfold(backg, 5)
backg_train = backg[group != 1, ]
backg_test = backg[group == 1, ]

r = raster(stck, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(ext, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-',  cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')
require(dismo)
dm =domain(stck, pres_train) #domain model- presence data only 
e = evaluate(pres_test, backg_test, dm, stck)
e
pd = predict(stck, dm, ext=ext, progress='') #predictive mapping
par(mfrow=c(1,2))
plot(pd, main='Domain, raw values')


