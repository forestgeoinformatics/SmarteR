pa=read.csv("Pres_abs.csv")

head(pa)
set.seed(1) #pseudo-repeatability
trainIndex = createDataPartition(pa$pb, p = .75, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting
training = pa[trainIndex,] #75% data for model training
testing= pa[-trainIndex,] #25% for model testing
set.seed(456)
pb=as.factor(training$pb) #1 stands for presence and 0 for absence
land=as.factor(training$land) #land use categories are categorical
## caret
train_control = trainControl(method="cv", number=10) # define training control with 10 fold cross validation
mod_fit=train(pb~.,data=training,trControl=train_control,method="knn")
summary(mod_fit)
varImp(mod_fit) # importance of the different predictors
p1=predict(mod_fit, newdata=testing) #predict on the test data
library(pROC)
roc.glmModel = pROC::roc(testing[,"pb"], p1) #compare testing datawith predicted responses
auc= pROC::auc(roc.glmModel)
auc#area under the curve
plot(roc.glmModel)
text(0.5,0.5,paste("AUC = ",format(auc, digits=5, scientific=FALSE)))
p1 = predict(stck, mod_fit) #use predict to implement the model stored in mod_fit on the raster stack of our predictors
plot(p1,main="kNN Predictive Map")
