library(lubridate)
library(data.table)
library(dplyr)
library(forecast)
#install.packages("caret", dependencies = TRUE)
library(caret)
library(writexl)



train <- read.csv("C:/Users/seyma/Desktop/IE425project/kobe-train.csv", header = TRUE,sep = ",")
test <- read.csv("C:/Users/seyma/Desktop/IE425project/kobe-test.csv", header = TRUE,sep = ",")
submission <- read.csv("C:/Users/seyma/Desktop/IE425project/kobe-sample-submission.csv", header = TRUE,sep = ",")



summary(train)
str(train)
train$shot_made_flag  <- factor(train$shot_made_flag )
train$action_type <- factor(train$action_type)
train$combined_shot_type <-factor(train$combined_shot_type)
train$game_id <- factor(train$game_id)
train$game_event_id <- factor(train$game_event_id)
train$playoffs <- factor(train$playoffs)
train$shot_type  <- factor(train$shot_type)
train$shot_zone_area  <- factor(train$shot_zone_area)
train$shot_zone_basic  <- factor(train$shot_zone_basic)
train$shot_zone_range   <- factor(train$shot_zone_range)
train$team_id     <- factor(train$team_id)
train$team_name  <- factor(train$team_name) 
train$game_date   <- as.Date(train$game_date, format= "%d.%m.%Y") 
train$game_date <- as.numeric(train$game_date)
train$matchup    <-  factor(train$matchup)
train$opponent <- factor(train$opponent)

test$action_type <- factor(test$action_type)
test$combined_shot_type <-factor(test$combined_shot_type)
test$game_id <- factor(test$game_id)
test$game_event_id <- factor(test$game_event_id)
test$playoffs <- factor(test$playoffs)
test$shot_type  <- factor(test$shot_type)
test$shot_zone_area  <- factor(test$shot_zone_area)
test$shot_zone_basic  <- factor(test$shot_zone_basic)
test$shot_zone_range   <- factor(test$shot_zone_range)
test$team_id     <- factor(test$team_id)
test$team_name  <- factor(test$team_name) 
test$game_date   <- as.Date(test$game_date, format= "%d.%m.%Y") 
test$game_date <- as.numeric(test$game_date)
test$matchup    <-  factor(test$matchup)
test$opponent <- factor(test$opponent)




df <- train[,c("shot_made_flag","action_type","combined_shot_type", "loc_x", "loc_y","minutes_remaining","game_date", "period", "playoffs", "season", "seconds_remaining", "shot_distance", "opponent")]
test <- test[,c("X","action_type","combined_shot_type", "loc_x", "loc_y","minutes_remaining","game_date", "period", "playoffs", "season", "seconds_remaining", "shot_distance", "opponent")]
names(test) <- c("shot_made_flag","action_type","combined_shot_type", "loc_x", "loc_y","minutes_remaining","game_date", "period", "playoffs", "season", "seconds_remaining", "shot_distance", "opponent")


set.seed(425)


control=trainControl(method='repeatedcv',number=10,repeats=5)
glm <- train(shot_made_flag~., data=df, method="glm", metric="Accuracy",trControl = control)
summary(glm)
confusionMatrix(glm)
varImp(glm)


rpart_mod = train(
  shot_made_flag ~ .,
  data = df,
  method = "rpart",
  trControl = control,
  metric= "Accuracy",
  tuneGrid = expand.grid(cp = c(1:100)*0.01)
)

rpart_mod
confusionMatrix(rpart_mod)
varImp(rpart_mod)

control3 <- trainControl(method='cv',number=10)
mtry <- c(1:12)

rftree1 <- train(shot_made_flag~., data=df, method = "rf", metric = 'Accuracy',trControl = control3, ntree = 100,  tuneGrid = expand.grid(.mtry = mtry))
rftree1
confusionMatrix(rftree1)
varImp(rftree1)



rftree2 <- train(shot_made_flag~., data=df, method = "rf", metric = 'Accuracy',trControl = control3, ntree = 200,  tuneGrid = expand.grid(.mtry = mtry))
rftree2
confusionMatrix(rftree2)
varImp(rftree2)


rftree3 <- train(shot_made_flag~., data=df, method = "rf", metric = 'Accuracy',trControl = control3, ntree = 400,  tuneGrid = expand.grid(.mtry = mtry))
rftree3
confusionMatrix(rftree3)
varImp(rftree3)


control2 <- trainControl(method='repeatedcv',number=10,repeats=3)

gbmGrid=expand.grid(interaction.depth = c(1, 3, 5), 
                    n.trees = c(50,100,150),
                    shrinkage = c(0.1,0.2,0.3),
                    n.minobsinnode = c(5,10,15))

gbmtree =train(shot_made_flag~., data=df, method="gbm", metric="Accuracy", verbose = TRUE ,trControl = control2,tuneGrid = gbmGrid)
gbmtree
confusionMatrix(gbmtree)
summary(gbmtree)


gbmprediction <- predict(gbmtree,test,type= "prob")
submission$shot_made_flag <- gbmprediction[,2]
write_xlsx(submission,"C:/Users/seyma/Desktop/IE425project/submission.xlsx")

rfprediction <- predict(rftree3,test,type= "prob")
submission$shot_made_flag <- rfprediction[,2]
write_xlsx(submission,"C:/Users/seyma/Desktop/IE425project/submission2.xlsx")




