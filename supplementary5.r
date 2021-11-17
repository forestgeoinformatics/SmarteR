head(lantana1)
plot(msia.worldclim) ## environmental predictors
lantana.d = bioclim(msia.worldclim, lantana1[,c('long','lat')]) ## bioclimatic model
ext = extent(99, 105, 1.2, 6.7)
backg = randomPoints(msia.worldclim, n=1000,ext=ext, extf = 1.25) #background/pseudo-absence data
e = evaluate(lantana1, backg, lantana.d, msia.worldclim) #four arguments namely presence, background, model and predictors.
#presence, background, model, predictors
e
plot(e, 'ROC')
