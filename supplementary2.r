library(raster)
adm0 = getData('GADM', country='MY', level=0) #country outline
adm1 = getData('GADM', country='MY', level=1) # states boundary
par(mfrow=c(1,2))
plot(adm0, main="Adm. Boundaries India Level 0")
plot(adm1, main="Adm. Boundaries India Level 1")
climate = getData('worldclim', var='bio', res=2.5) ## world climate data at resolution 2.5 
plot(climate$bio1, main="Annual Mean Temperature")
plot(climate$bio5, main="Maximum Temperature")
