library(raster)
library(rgdal)
library(dismo)
setwd("C:\\Users\\searc\\Desktop\\Demo\\Species_Distribution_Modeling\\SDM\\bioclim")#setting working directory
datafiles = Sys.glob("*.tif") # file extension of your raster predictor files
datafiles #list of predictors
stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
tempraster = raster(datafiles[i])
stck = stack(stck,tempraster)
}
stck #raster predictors as a stackplot
plot(stck,1)
lantana=read.csv("lantana_camara.csv") ## presence data
head(lantana)
lantana1= lantana[,-1] ##first column not needed
points(lantana1, col='blue') 
group = kfold(lantana1, 5) #split the data into 5 portions, build and test models on all splits
pres_train = lantana1[group != 1, ]
pres_test = lantana1[group == 1, ]
xm = maxent(stck, pres_train) ##implement maxent on the presence-only data
plot(xm) ## variable importance
ext = extent(99, 105, 1.2, 6.7) ## Study Area Malaysia
backg = randomPoints(stck, n=1000,ext=ext, extf = 1.25) ##1000 background data points
colnames(backg) = c('lon', 'lat')
group = kfold(backg, 5)
##pseudo-absences for training model performances
backg_train = backg[group != 1, ]
backg_test = backg[group == 1, ]
e = evaluate(pres_test, backg_test, xm, stck)
e
p= predict(stck, xm, ext=ext, progress='')
## 0–1 scale where 1 indicates the most suitable habitat and 0 as least suitable habitat 
par(mfrow=c(1,2))
plot(p, main='Maxent, raw values')


