library(xgboost)
library(caret)
library(fastDummies)
library(ROCR)
# Read Data
train_data <- read.csv("../data/heart_2020_cleaned.csv", header = T)
# Data
coluniqe_Race <- unique(train_data$Race)
coluniqe_GenHealth <- unique(train_data$GenHealth)
coluniqe_AgeCategory <- unique(train_data$AgeCategory)
# Data Process
train_data <- distinct(train_data) # remove duplicate data
# Turn Yes/No to 1/0
train_data$HeartDisease <- ifelse(train_data$HeartDisease == "Yes", 1, 0)
train_data$Smoking <- ifelse(train_data$Smoking == "Yes", 1, 0)
train_data$AlcoholDrinking <- ifelse(train_data$AlcoholDrinking == "Yes", 1, 0)
train_data$Stroke <- ifelse(train_data$Stroke == "Yes", 1, 0)
train_data$DiffWalking <- ifelse(train_data$DiffWalking == "Yes", 1, 0)
train_data$Sex <- ifelse(train_data$Sex == "Male", 1, 0)
train_data$Diabetic <- ifelse(train_data$Diabetic == "Yes", 1, 0)
train_data$PhysicalActivity <- ifelse(train_data$PhysicalActivity == "Yes", 1, 0)
train_data$Asthma <- ifelse(train_data$Asthma == "Yes", 1, 0)
train_data$KidneyDisease <- ifelse(train_data$KidneyDisease == "Yes", 1, 0)
train_data$SkinCancer <- ifelse(train_data$SkinCancer == "Yes", 1, 0)
# Turn AgeCategory To Num
train_data$AgeCategory[train_data$AgeCategory=='18-24'] <- 21
train_data$AgeCategory[train_data$AgeCategory=='25-29'] <- 27
train_data$AgeCategory[train_data$AgeCategory=='30-34'] <- 32
train_data$AgeCategory[train_data$AgeCategory=='35-39'] <- 37
train_data$AgeCategory[train_data$AgeCategory=='40-44'] <- 42
train_data$AgeCategory[train_data$AgeCategory=='45-49'] <- 47
train_data$AgeCategory[train_data$AgeCategory=='50-54'] <- 52
train_data$AgeCategory[train_data$AgeCategory=='55-59'] <- 57
train_data$AgeCategory[train_data$AgeCategory=='60-64'] <- 62
train_data$AgeCategory[train_data$AgeCategory=='65-69'] <- 67
train_data$AgeCategory[train_data$AgeCategory=='70-74'] <- 72
train_data$AgeCategory[train_data$AgeCategory=='75-79'] <- 77
train_data$AgeCategory[train_data$AgeCategory=='80 or older'] <-80
train_data$AgeCategory<-as.numeric(train_data$AgeCategory)
train_data <- dummy_cols(train_data, select_columns = c('Race', 'Diabetic', 'GenHealth'), remove_selected_columns = TRUE)
# K fold
k <- 5
train_data <- train_data[sample(nrow(train_data)),]
folds <- cut(seq(1, nrow(train_data)), breaks = k, labels = FALSE)
test_set <- train_data[folds == 1,]
validation_set <- train_data[folds == 2,]
training_set <- train_data[!(folds %in% c(1, 2)), ]
dtrain <- xgb.DMatrix(data = as.matrix(training_set[,-1]), label = as.numeric(training_set$HeartDisease))
dvalidation <- xgb.DMatrix(data = as.matrix(validation_set[,-1]), label = as.numeric(validation_set$HeartDisease))
dtest <- xgb.DMatrix(data = as.matrix(test_set[,-1]), label = as.numeric(test_set$HeartDisease))
# XGboost Model
bst <- xgb.train(data=dtrain, max.depth=3
                 , eta=0.01, nthread = 2, nround=2000
                 , eval.metric = "error"
                 , eval.metric = "logloss"
                 , objective = "binary:logistic")
# Precision
print(xgb.importance(model = bst))
xgb.plot.importance(importance_matrix = xgb.importance(model = bst))
pred.xg<-predict(bst,dtest)
pr <- prediction(pred.xg, test_set$HeartDisease)
threshold <- 0.5
predicted_labels <- ifelse(pred.xg >= threshold, 1, 0)
# 計算準確率
accuracy <- sum(predicted_labels == test_set$HeartDisease) / length(test_set$HeartDisease)
print(paste("Accuracy:", accuracy))
# ROC
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
# AUC
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
