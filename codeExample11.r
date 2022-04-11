setwd("C:\Users\searc\Desktop\Demo\Species_Distribution_Modeling\SDM\bioclim_land")#setting working directory
pa=read.csv("Pres_abs.csv")
#pa=na.omit(pa)
tail(pa)
summary(pa)
pb=as.factor(pa$pb) #1 stands for presence and 0 for absence
land=as.factor(pa$land) #land use categories are categorical
head(pa)
library(caret)
set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting
training = pa[trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing
head(training)
set.seed(456) #this is needed to garantee that every run will produce the same output
train_control = trainControl(method="cv", number=10) # define training control--> 10fold cv
mod_fit=train(pb~.,data=training,trControl=train_control,method="rf",importance=TRUE)
summary(mod_fit)
varImp(mod_fit) # importance of the different predictors
p1=predict(mod_fit, newdata=testing) #predict on the test data
library(pROC) #test model fit-auc
roc = pROC::roc(testing[,"pb"], p1) #compare testing data with predicted responses
auc= pROC::auc(roc)
plot(roc)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))
require(raster) # build an SDM with all predictors
datafiles = Sys.glob("*.tif") # files extension identifying the predictors
datafiles #list of predictors
stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
tempraster = raster(datafiles[i])
stck = stack(stck,tempraster)
}
p = predict(stck, mod_fit) #use predict to implement the RF model stored in mod_fit on the raster stack of our predictors
plot(p,main="RF Predictive Map")
# test the impact of individual predictors
require(randomForest) #randomForest package
m1 = randomForest(pb ~., data=training) 
partialPlot(m1, training, preciptn, pb) #class(m1) #rf model, training data, X, Y/response variable
p2 = predict(stck, m1) #use predict to implement the RF model stored in mod_fit on the raster stack of our predictors
plot(p2,main="RF Predictive Map")
m2 = randomForest(pb ~., data=pa) #all data
p3 = predict(stck, m2) #use predict to implement the RF model stored in mod_fit on the raster stack of our predictors
plot(p3,main="RF Predictive Map-All")
