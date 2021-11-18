library(spocc)
spp= c("Impatiens glanduliferaRoyle","Lantana camara ")
dat = occ(query = spp, from ='gbif', gbifopts = list(hasCoordinate=TRUE))
data = occ2df(dat) ##convert to data frame
head(data)
tail(data)
names(data)
