
pa=read.csv("Pres_abs.csv")


head(pa)
set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting
training = pa[trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing
set.seed(456)
pb=as.factor(pa$pb) #1 stands for presence and 0 for absence
land=as.factor(pa$land) #land use categories are categorical
train_control = trainControl(method="cv", number=10)# define training control--> 10fold cv
mod_fit=train(pb~.,data=training,trControl=train_control,method="gbm")
summary(mod_fit) #will provide relative influence measure/ importance of the different predictors
varImp(mod_fit)
p1=predict(mod_fit, newdata=testing) #predict on the test data
library(pROC)#test model fit-auc
roc= pROC::roc(testing[,"pb"], p1) #compare testing data with predicted responses
auc= pROC::auc(roc)
auc
plot(roc)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))
require(raster)#raster predictors
datafiles = Sys.glob("*.tif") #Or whatever identifies your files
datafiles #list of predictors
stck = stack() #empty raster stack for storing raster layers
for(i in 1:NROW(datafiles)){
tempraster = raster(datafiles[i])
stck = stack(stck,tempraster)
}
p1 = predict(stck, mod_fit) #use predict to implement the GBM model stored
#in mod_fit on the raster stack of our predictors
plot(p1,main="GBM Predictive Map")
###### use the whole dataset
pb=as.factor(pa$pb) #pa is the unsplit dataframe
mod_fit2=train(pb~.,data=pa,trControl=train_control,method="gbm")
summary(mod_fit2)
p2 = predict(stck, mod_fit2) #use predict to implement the MARS model storedin mod_fit on the raster stack of our predictors
plot(p2,main="GBM Predictive Map")
#partial dependence plots
library(gbm)
boost = gbm(pb~. , data=training, 
             distribution = 'gaussian', 
n.trees = 5000, 
interaction.depth = 4)
summary(boost)
plot(boost, i='slope')
plot(boost, i='altitude')
plot(boost, i='roughness1')
