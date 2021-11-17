library(caret)
pa=read.csv("Pres_abs.csv")


head(pa) #1 --> species occurence
set.seed(1) #pseudo-repeatability – so that we can re-run the same code 
trainIndex = createDataPartition(pa$pb, p = .80, 
                                 list = FALSE, 
                                 times = 1) #y as basis of splitting
training = pa[trainIndex,] #80% data for model training
testing= pa[-trainIndex,] #20% for model testing
set.seed(456)
pb=as.factor(training$pb) #pb is response variable. 1 stands for presence and 0 for absence
land=as.factor(training$land) #land use categories (categorical)  - used as predictor
m1 = glm(pb ~., data=training) #we can set glm as a base package
summary(m1)
