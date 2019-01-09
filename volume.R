#### pacman ####
library("pacman")
pacman::p_load("readr","caret","corrplot","dplyr","tidyverse","mlbench","ggplot2","rpart.plot","plotly","party","reshape")
setwd("C:/Users/raul9/Documents/Volume1")

#### read csv ####
existing_set <- read.csv("C:/Users/raul9/Documents/Volume1/Predict Volume/existingproductattributes2017.csv.csv")
new_set <- read.csv("C:/Users/raul9/Documents/Volume1/Predict Volume/newproductattributes2017.csv.csv")
existing_set$BestSellersRank <- NULL

summary(existing_set)
str(existing_set)

#### plotly ####
volume.Box <- plot_ly(existing_set, x = ~Volume, y= ~ProductType)
volume.Box

#### extended warranty ####
existing_set[34:41, "Price"] <- mean(existing_set[34:41, "Price"])
existing_set <- existing_set[-c(35:41),]

#### correlation and corrplot ####
excor <- subset(existing_set, select = -c(ProductType,x5StarReviews,x2StarReviews,x3StarReviews,NegativeServiceReview))

corrData<- cor(excor)
corrData

corrplot(
  corrData,
  method = "number",
  type = "lower")

#### decision tree ####
exdt <- subset(existing_set, select = -c(x5StarReviews,x2StarReviews,x3StarReviews,NegativeServiceReview))
set.seed(123)

decisionTree <- rpart(Volume~ .,
                      data = exdt,
                      control = list(maxdepth = 5))
decisionTree
rpart.plot(decisionTree)

varImp(decisionTree)

#### linear model ####
lm1<- lm(Volume~.,  + x4StarReviews + 0, data = exdt)
plot(lm1) 

varImp(lm1)
                    
#### selección de columnas ####
exset<- existing_set[,c("Volume","x4StarReviews", "PositiveServiceReview")]

#### outliers ####
exset <- subset(
  exset,
  Volume < 7000)

#### set seed 423 ####
set.seed(423)

#### cross validation ####
control<-trainControl(method = "repeatedcv", 
             number = 10,
             repeats = 2)

#### create data partition ####
trainingIndices<- (createDataPartition(exset$Volume,
                                       p= .75,
                                       list = FALSE))
training<- exset[trainingIndices,]
testing<- exset[-trainingIndices,]

#### distributions ####

ggplot(training, aes(x=x4StarReviews))+geom_histogram()
ggplot(testing, aes(x=x4StarReviews))+geom_histogram()
ggplot(training, aes(x=PositiveServiceReview))+geom_histogram()
ggplot(testing, aes(x=PositiveServiceReview))+geom_histogram()
ggplot(training, aes(x=Volume))+geom_histogram()
ggplot(testing, aes(x=Volume))+geom_histogram()  

    
#### compare models ####
models<- list(
  "rf",
  "svmLinear",
  "gbm",
  "knn")

compareModel<- c()
trainedModel<- list()

for (model in models) { 
  trainedModel<- train(Volume~.,data = training, method = model, trControl = control, tuneLength = 3, preProcess = c("center", "scale"))
  prediction <- predict(trainedModel, testing)
  predMetric <- postResample(prediction, testing$Volume)
  compareModel <- cbind(predMetric,compareModel)
  trainedModel[[model]] <- trainedModel
  }

colnames(compareModel) <- models
compareModel

compareMelt <- melt(compareModel)
compareMelt

colnames(compareMelt) <- c("Metric","Model","Value")
compareMelt

ggplot(compareMelt, aes(x=Model, y=Value))+
  geom_col()+facet_grid(Metric~., scales="free")

#### final prediction ####
modelknn<- train(Volume~.,data = training, method = "knn", trControl = control, tuneLength = 3, preProcess = c("center", "scale"))
prediction <- predict(modelknn, new_set)

prediction

#### create columns and csv ####
new_set$Volume <- prediction

new_set$Sales <- new_set$Price*new_set$ProfitMargin*new_set$Volume

write.csv(new_set, file = "C:/Users/raul9/Desktop/test.csv", row.names = TRUE)



