setwd("C:\\Users\\searc\\Desktop\\Demo\\Species_Distribution_Modeling\\SDM \\Elevation")#setting working directory
library(raster)
library(rgdal)
elev1=raster("Elev_srtm_A.tif")
plot(elev1)
elev2=raster("Elev_srtm_B.tif")
plot(elev2)
par(mfrow=c(1,2))
plot(elev1)
plot(elev2)
mosaicAB = mosaic(elev1, elev2, fun=mean)
plot(mosaicAB)
writeRaster(mosaicAB,"Elev_AB.tif")
mosaicAB##lat long
ref = "+proj=utm +zone=50 +ellps=GRS80 +datum=NAD83 +units=m +no_defs" #utm projection for study area 
library(raster)
projected_raster = projectRaster(mosaicAB, crs = ref)
projected_raster
slp=terrain(projected_raster, opt='slope', unit='radians', neighbors=8, filename='slp2.tif') # compute slope for given parameters
plot(slp)
aspect=terrain(projected_raster, opt='aspect') # compute aspect
hillshd = hillShade(slp, aspect, angle=45, direction=270)  # create hillshade from slope & aspect
plot(hills)
