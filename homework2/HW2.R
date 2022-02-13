

# seyma cakir 2017402024 
# 13/672021
library(lubridate)
library(data.table)
library(dplyr)
library(ggplot2)
library(knitr)
library(tidyr)
library(tidyverse)
library(scales)
library(ggcorrplot)
library(forecast)
library(urca)
library(zoo)

library(GGally)
library(PerformanceAnalytics)
library("caTools")
library("tree")
library("rpart")
library("rpart.plot")
library(caret)
require(randomForest)


data  <- read.csv("C:/Users/seyma/Desktop/boun/IE 425/homework2/ToyotaCorolla.csv", header = TRUE,sep = ",")
data <- as.data.table(data)

str(data)
summary(data)
data$FuelType <- as.factor(data$FuelType)


#ggcorrplot(corr, hc.order = TRUE, type = "lower", lab = TRUE)




set.seed(425)

# create train set by usinng 0.70 of data
0.7*nrow(data)
sample = sample(1:nrow(data), 1006)
train <- data[sample]
# create test set by removing train set
test <- data[-sample]

mean(train$Price)
#  10766.66
mean(test$Price)
# 10647
str(train)



control=trainControl(method='repeatedcv',number=10,repeats=5)

ntree=  c(100,200,300,400,500)



# Random Forest

#rftrees <- vector(mode = "list", 5)
mtry <- c(1:(ncol(data)-1))


rftree1 <- train(Price~., data=train, method = "rf", metric = 'RMSE',trControl = control, ntree = 100,  tuneGrid = expand.grid(.mtry = mtry))
rftree1
rftree2 <- train(Price~., data=train, method = "rf", metric = 'RMSE',trControl = control, ntree = 200,  tuneGrid = expand.grid(.mtry = mtry))
rftree2
rftree3 <- train(Price~., data=train, method = "rf", metric = 'RMSE',trControl = control, ntree = 300,  tuneGrid = expand.grid(.mtry = mtry))
rftree3
rftree4 <- train(Price~., data=train, method = "rf", metric = 'RMSE',trControl = control, ntree = 400,  tuneGrid = expand.grid(.mtry = mtry))
rftree4
rftree5 <- train(Price~., data=train, method = "rf", metric = 'RMSE',trControl = control, ntree = 400,  tuneGrid = expand.grid(.mtry = mtry))
rftree5

# by analyzing th eoutcomes of trees above, it seIt can be said that mtry= 5 gives thesmaller RMSE whivh is the metrics used for evaluation of the models
# and secondly, the lowest RMSE which is 1084.982 gets by ntree= 400, therefore

set.seed(425)
rftree <- randomForest(Price~.,data=train,mtry=5, ntree= 400,importance=TRUE)
print( paste( "RMSE of randomForest Model" ,RMSE(train$Price,predict(rftree4))))
RMSE(predict(rftree4))
rfpredictions <- predict(rftree,test)
print( paste( "RMSE of randomForest Model" ,RMSE(test$Price, rfpredicitons)))

varImp(rftree)
varImpPlot(rftree)

# Overall
#Age       94.699008
# KM        24.969664
#FuelType   6.848736
#HP        17.734828
#MetColor   6.002885
#Automatic  4.142520
#CC        16.610693
#Doors     10.940617
#Weight    29.404268





# Linear Regression

lRegression <- train(Price~., data=train, method="lm", metric="RMSE",trControl = control)
summary(lRegression)



# tree by gbm 

gbmGrid=expand.grid(interaction.depth = c(1, 3, 5), 
                                        n.trees = c(75,100,125,150),
                                        shrinkage = c(0.1,0.2,0.3),
                                        n.minobsinnode = c(5,10,15))

gbmtree =train(Price~., data=train, method="gbm", metric="RMSE" ,trControl = control,tuneGrid = gbmGrid)
gbmtree
#RMSE was used to select the optimal model using the smallest value which is 
min(gbmtree$results["RMSE"])
#The final values used for the model were n.trees = 100, interaction.depth = 5, shrinkage = 0.1 and n.minobsinnode = 5.

#importance of variables 
summary(gbmtree)

