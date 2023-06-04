# --------------installed packages-----------------------
# install.packages('rpart.plot')
# install.packages('ggplot2')
# install.packages('png')
# install.packages('xgboost')
# install.packages('lightgbm')
# -------------------------------------------------------

# smote from raw data
#  Rscript.exe code/xgboost_111971005.R --input data/heart_2020_cleaned.csv 
# use already smote data and save your time
#  Rscript.exe code/xgboost_111971005.R --input data/heart_2020_smote.csv 
args <- commandArgs(trailingOnly = TRUE)
for (i in args){
  if (i == "--input") {
    input <- args[which(args == i) + 1]
  }
}

f_in_csv <- read.csv("data/heart_2020_cleaned.csv")
f_in_csv <- data.frame(f_in_csv)

# check rows 319795 rows & 18 cols
# message(nrow(f_in_csv))
# message(ncol(f_in_csv))
# head(f_in_csv)
# summary(f_in_csv)

# # check dataㄌ coolumn
# names(f_in_csv) 


# check columns category
# for(i in colnames(f_in_csv))
# {
#     if(i %in% c("BMI", "PhysicalHealth", "MentalHealth", "SleepTime")){next}
#     category <- unique(f_in_csv[[i]])
    
#     print(paste("col_name：", i))
#     print(category)

# }

# check for NA
sum(is.na(f_in_csv))
# No NA values
for(i in colnames(f_in_csv))
{
    na_nums <- 0   
    na_nums <- sum(is.na(f_in_csv[[i]]))
    
    if (na_nums > 0)
    {
       print(i)
       print(na_nums)
    }    
}

# check for value duplicate if duplicate selected the distinct values
library('dplyr')
message('raw data rows:')
nrow(f_in_csv)
message('rasw data cols:')
ncol(f_in_csv)

duplicate <- duplicated(f_in_csv)
if (sum(duplicate) > 0){
    # 18078 duplicated
    msg <- sprintf("has duplicated row %d", sum(duplicate))
    print(msg)
    f_in_csv <- distinct(f_in_csv)

}

# Age should be numerical, I will convert age variable into numeric variable taking mean age of each level of age.
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='18-24'] <- 21
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='25-29'] <- 27
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='30-34'] <- 32
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='35-39'] <- 37
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='40-44'] <- 42
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='45-49'] <- 47
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='50-54'] <- 52
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='55-59'] <- 57
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='60-64'] <- 62
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='65-69'] <- 67
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='70-74'] <- 72
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='75-79'] <- 77
f_in_csv$AgeCategory[f_in_csv$AgeCategory=='80 or older'] <-80
f_in_csv$AgeCategory<-as.numeric(f_in_csv$AgeCategory)

# install.packages("fastDummies")
#Encode categorical variables
#將Race Diabetic GenHealth 出現類別轉換為col names
# unique(f_in_csv$Race)
# unique(f_in_csv$Diabetic)
# unique(f_in_csv$GenHealth)
library('fastDummies')
f_in_csv$Smoking<- ifelse(f_in_csv$Smoking=='Yes',1,0)
f_in_csv$AlcoholDrinking<-ifelse(f_in_csv$AlcoholDrinking=='Yes',1,0)
f_in_csv$Stroke<-ifelse(f_in_csv$Stroke=='Yes',1,0)
f_in_csv$DiffWalking<-ifelse(f_in_csv$DiffWalking=='Yes',1,0)
f_in_csv$Sex<-ifelse(f_in_csv$Sex=='Male',1,0)
f_in_csv$PhysicalActivity<-ifelse(f_in_csv$PhysicalActivity=='Yes',1,0)
f_in_csv$Asthma<-ifelse(f_in_csv$Asthma=='Yes',1,0)
f_in_csv$KidneyDisease<-ifelse(f_in_csv$KidneyDisease=='Yes',1,0)
f_in_csv$SkinCancer<-ifelse(f_in_csv$SkinCancer=='Yes',1,0)
f_in_csv <- dummy_cols(f_in_csv, select_columns = c('Race', 'Diabetic', 'GenHealth'), remove_selected_columns = TRUE)

# change the col name 
f_in_csv<-rename(f_in_csv
    , Race_American_Indian_Alaskan_Native = 'Race_American Indian/Alaskan Native'
    ,Diabetic_No_borderline_diabetes ='Diabetic_No, borderline diabetes'
    , Diabetic_Yes_during_pregnancy ='Diabetic_Yes (during pregnancy)'
    ,GenHealth_Very_Good='GenHealth_Very good')

# 29 features 1 label
# names(f_in_csv)

# base_cols <- paste(colnames(f_in_csv)[-1], collapse="+")# "V3+V4+...+V5602"
set.seed(1234)

library(caret)
library(rpart.plot)
# set the training & testing data
data <- f_in_csv
ind_train <- createDataPartition(y = data$HeartDisease, p= 0.8, list = FALSE)
ind_test <- createDataPartition(y=data$HeartDisease[-ind_train], p=0.5, list = FALSE)
training <- data[ind_train,]
testing <- data[-ind_train,][ind_test, ]
validation <- data[-ind_train,][-ind_test, ]
message("training set:")
dim(training)
message("testing set:")
dim(testing)
message("validation set:")
dim(validation)
input='data/heart_2020_smote.csv'
# use smote to select sample
# SMOTE代表合成少数类过采样技术（Synthetic Minority Oversampling Technique）
# set the dataset into train, test, valid and use smote on training data.
# install.packages("performanceEstimation")
library('performanceEstimation')
if(input == "data/heart_2020_cleaned.csv"){
  training_smote <- smote(HeartDisease ~ ., data = training)
  message("finishing smote")
}else if (input == "data/heart_2020_smote.csv") {
  training_smote <- read.csv("data/heart_2020_smote.csv")
}
message("training data labes:")
table(training[1])
message("training data labels with smote:")
table(training_smote[1])

# 152663 rows 30 col
# NA 43618 after smote on HeartDiseases col
# select non na value
training_smote_NonNA <- na.omit(training_smote)

# 使用套件取得相關數值
get_model_cfmatrix <- function(model, testing, with_smote){
    predicted <- predict(model, newdata=testing)
    result_frame <- data.frame(predicted=predicted, result=testing$HeartDisease)
    result_frame$pred_acc <- result_frame$predicted == result_frame$result
    result_frame_count <- table(result_frame$pred_acc)
    sink(paste('results/', model[1], '-', with_smote, '.txt'))
    print('---------------Model Information--------------')
    print(model)
   
    print("---------------Predicted Result Counts:------------------")
    print(result_frame_count)
    print("----------------Testing Data Acc:------------------------")
    print(result_frame_count[2] / nrow(result_frame))
    # knn.k1 <- model$bestTune # keep this Initial k for testing with knn() function in next section
    cf <- confusionMatrix(factor(predicted), factor(testing$HeartDisease), positive='Yes')
    print("----------------Confusion Matrix:------------------------")
    print(cf)
    print(cf[3]) #overall col
    print(cf[4]) #byclass col
    sink()
}

library(pROC)
library(png)
# ROC Curve function
get_rpart_roc <- function(model, testing, model_name, with_smote){
  predicted <- predict(model, newdata = testing, type = "prob")
  scores <- predicted[, "Yes"]
  labels <- ifelse(testing$HeartDisease == "Yes", 1, 0)
  roc_obj <- roc(labels, scores)
  auc <- auc(roc_obj)
  plot(roc_obj, main = "ROC Curve")
  legend("bottomright", legend = paste("AUC =", round(auc, 2)), bty = "n")
  png(paste("results/", model_name, '-', with_smote, '.png', sep = ''), width = 600, height = 600)
  plot(roc_obj, main = "ROC Curve")
  legend("bottomright", legend = paste("AUC =", round(auc, 2)), bty = "n")
  dev.off()
  
  # pred <- prediction(scores, labels)
  # perf <- performance(pred, "tpr", "fpr")
  # auc  <- performance(pred, 'auc')
  # auc  <- unlist(slot(auc,"y.values"))
  # plot(perf, main = "ROC Curve")
  # legend("bottomright", legend = paste("AUC =", round(auc, 2)), bty = "n")
}

# Run algorithms using 5-fold cross validation
trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(3333)

# use rpart to train model with unsmote
message("************************")
model_select_message <- sprintf("Training model is: %s", 'rpart with unsmote')
print(model_select_message)
model_rpart_unsmote <- train(HeartDisease ~., data = training, method = 'rpart', trControl=trctrl)
# use rpart to test model
get_model_cfmatrix(model_rpart_unsmote, testing, with_smote='unsmote')
get_rpart_roc(model_rpart_unsmote, testing, model_name = 'rpart', with_smote = 'unsmote')

# use rpart to train model with smote
message("************************")
model_select_message <- sprintf("Training model is: %s", 'rpart with smote')
print(model_select_message)
model_rpart_smote <- train(HeartDisease ~., data = training_smote_NonNA, method = 'rpart', trControl=trctrl)
print(model_rpart_smote)
# use rpart to test model
get_model_cfmatrix(model_rpart_smote, testing, with_smote='smote')
get_rpart_roc(model_rpart_smote, testing, model_name = 'rpart', with_smote = 'smote')


#install.packages("xgboost")
# xgboost with unsmote
library(xgboost)
message("************************")
model_select_message <- sprintf("Training model is: %s", "xgboost")
print(model_select_message)

# 加載訓練數據和測試數據，並準備特徵矩陣和標籤
train_features_unsmote <- as.matrix(training[, -1])
train_labels_unsmote <- as.matrix(factor(training[, 1]))
train_labels_unsmote <- ifelse(train_labels_unsmote=="No", 0,1)
test_features <- as.matrix(testing[, -1])
test_labels <- as.matrix(testing[, 1])
test_labels <- ifelse(test_labels=='No', 0,1)

# 將數據轉換為DMatrix格式
train_matrix_unsmote <- xgb.DMatrix(data = train_features_unsmote, label = train_labels_unsmote)
test_matrix <- xgb.DMatrix(data = test_features)

params <- list(
  "objective" = "binary:logistic",
  "eval_metric" = "error",
  "max_depth" = 6,
  "eta" = 0.3,
  "nthread" = 4
)

model_xgboost_unsmote <- xgb.train(params = params, data = train_matrix_unsmote, nrounds = 100)
predicted_xgboost_unsmote <- predict(model_xgboost_unsmote, test_matrix)
binary_predictions_xg_unsmote <- ifelse(predicted_xgboost_unsmote > 0.5, 1, 0)
sink(paste('results/xgboost-unsmote.txt'))
print('------------------Model Information----------------------')
print(model_xgboost_unsmote)
print('------------------Confusion Matrix----------------------')
cf <- confusionMatrix(factor(binary_predictions_xg_unsmote), factor(test_labels), positive='1')
print(cf)
print(cf[3]) #overall col
print(cf[4]) #byclass col
sink()
roc_obj_xgboost_unsmote <- roc(factor(test_labels), predicted_xgboost_unsmote)
print("roc ok")
auc_xg_unsmote <- auc(roc_obj_xgboost_unsmote)
plot(roc_obj_xgboost_unsmote, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
text(0.5, 0.5, paste("AUC =", round(auc_xg_unsmote, 2)), adj = c(0.5, 0.5))
png("results/xgboost-unsmote.png", width = 600, height = 600)
plot(roc_obj_xgboost_unsmote, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
text(0.5, 0.5, paste("AUC =", round(auc_xg_unsmote, 2)), adj = c(0.5, 0.5))
dev.off()



# xgboost with smote
# 加載訓練數據和測試數據，並準備特徵矩陣和標籤
train_features_smote <- as.matrix(training_smote_NonNA[, -1])
train_labels_smote <- as.matrix(factor(training_smote_NonNA[, 1]))
train_labels_smote <- ifelse(train_labels_smote=='No', 0,1)
test_features <- as.matrix(testing[, -1])
test_labels <- as.matrix(testing[, 1])
test_labels <- ifelse(test_labels=='No', 0,1)
# 將數據轉換為DMatrix格式
train_matrix_smote <- xgb.DMatrix(data = train_features_smote, label = train_labels_smote)
test_matrix <- xgb.DMatrix(data = test_features)

params <- list(
  "objective" = "binary:logistic",
  "eval_metric" = "error",
  "max_depth" = 6,
  "eta" = 0.3,
  "nthread" = 4
)

model_xgboost_smote <- xgb.train(params = params, data = train_matrix_smote, nrounds = 100)
predicted_xgboost_smote <- predict(model_xgboost_smote, test_matrix)
binary_predictions_xg_smote <- ifelse(predicted_xgboost_smote > 0.5, 1, 0)
sink(paste('results/xgboost-smote.txt'))
print(model_xgboost_smote)
print('------------------')
cf <- confusionMatrix(factor(binary_predictions_xg_smote), factor(test_labels), positive='1')
print(cf)
print(cf[3]) #overall col
print(cf[4]) #byclass col
sink()
roc_obj_xgboost_smote <- roc(factor(test_labels), predicted_xgboost_smote)
auc_xg_smote <- auc(roc_obj_xgboost_smote)
plot(roc_obj_xgboost_smote, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
text(0.5, 0.5, paste("AUC =", round(auc_xg_smote, 2)), adj = c(0.5, 0.5))
png("results/xgboost-smote.png", width = 600, height = 600)
plot(roc_obj_xgboost_smote, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
text(0.5, 0.5, paste("AUC =", round(auc_xg_smote, 2)), adj = c(0.5, 0.5))
dev.off()
# 繪製importance matrix
importance_matrix_smote <- xgb.importance(model = model_xgboost_smote)
print(importance_matrix_smote)
# install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
xgb.ggplot.importance(importance_matrix = importance_matrix_smote)
png("results/xgboost-smote-importance-matrix.png", width = 600, height = 600)
xgb.ggplot.importance(importance_matrix = importance_matrix_smote)
dev.off()

# install.packages("DiagrammeR")
library(DiagrammeR)
library(rpart.plot)
xgb.plot.tree(model = model_xgboost_smote, trees = 1, plot_width = 500,plot_height = 500)
tree_first <- xgb.plot.tree(model = model_xgboost_smote, trees = 1, plot_width = 500,plot_height = 500)
tree_first
# export_graph(tree_first, file_name = "decision_tree.png", width = 800, height = 600)



# xgboost with smote and parameters objective use hinge
# 加載訓練數據和測試數據，並準備特徵矩陣和標籤
train_features_smote <- as.matrix(training_smote_NonNA[, -1])
train_labels_smote <- as.matrix(factor(training_smote_NonNA[, 1]))
train_labels_smote <- ifelse(train_labels_smote=='No', 0,1)
test_features <- as.matrix(testing[, -1])
test_labels <- as.matrix(testing[, 1])
test_labels <- ifelse(test_labels=='No', 0,1)
# 將數據轉換為DMatrix格式
train_matrix_smote <- xgb.DMatrix(data = train_features_smote, label = train_labels_smote)
test_matrix <- xgb.DMatrix(data = test_features)

params <- list(
  "objective" = "binary:hinge",
  "eval_metric" = "error",
  "max_depth" = 6,
  "eta" = 0.3,
  "nthread" = 4
)

model_xgboost_smote_hinge <- xgb.train(params = params, data = train_matrix_smote, nrounds = 100)
predicted_smote_hinge <- predict(model_xgboost_smote_hinge, test_matrix)
sink(paste('results/xgboost-binary-hinge-smote.txt'))
print(model_xgboost_smote_hinge)
print('------------------')
cf <- confusionMatrix(factor(predicted_smote_hinge), factor(test_labels), positive='1')
print(cf)
print(cf[3]) #overall col
print(cf[4]) #byclass col
sink()
# roc_obj_hinge <- roc(factor(test_labels), predicted_smote)
# auc <- auc(roc_obj)
# plot(roc_obj, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
# text(0.5, 0.5, paste("AUC =", round(auc, 2)), adj = c(0.5, 0.5))
# jpeg("results/xgboost-binary-hinge-smote.jpg", width = 600, height = 600)
# dev.off()

# test xgboost threshold 0.4 0.5 0.6
binary_predictions_xg_smote_04 <- ifelse(predicted_xgboost_smote > 0.4, 1, 0)
sink(paste('results/xgboost-smote-04.txt'))
print(model_xgboost_smote)
print('------------------')
cf <- confusionMatrix(factor(binary_predictions_xg_smote_04), factor(test_labels), positive='1')
print(cf)
print(cf[3]) #overall col
print(cf[4]) #byclass col
sink()
roc_obj_xgboost_smote_04 <- roc(factor(test_labels), redicted_xgboost_smote)
auc_xg_smote_04 <- auc(roc_obj_xgboost_smote_04)
plot(roc_obj_xgboost_smote_04, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
text(0.5, 0.5, paste("AUC =", round(auc_xg_smote_04, 2)), adj = c(0.5, 0.5))
png("results/xgboost-smote04.png", width = 600, height = 600)
plot(roc_obj_xgboost_smote_04, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
text(0.5, 0.5, paste("AUC =", round(auc_xg_smote_04, 2)), adj = c(0.5, 0.5))
dev.off()

# test xgboost threshold 0.4 0.5 0.6
binary_predictions_xg_smote_06 <- ifelse(predicted_xgboost_smote > 0.6, 1, 0)
sink(paste('results/xgboost-smote-06.txt'))
print(model_xgboost_smote)
print('------------------')
cf <- confusionMatrix(factor(binary_predictions_xg_smote_06), factor(test_labels), positive='1')
print(cf)
print(cf[3]) #overall col
print(cf[4]) #byclass col
sink()
roc_obj_xgboost_smote_06 <- roc(factor(test_labels), predicted_xgboost_smote)
auc_xg_smote_06 <- auc(roc_obj_xgboost_smote_06)
plot(roc_obj_xgboost_smote_06, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
text(0.5, 0.5, paste("AUC =", round(auc_xg_smote_06, 2)), adj = c(0.5, 0.5))
png("results/xgboost-smote06.png", width = 600, height = 600)
plot(roc_obj_xgboost_smote_06, main = "ROC Curve", xlab = "False Positive Rate", ylab = "True Positive Rate")
text(0.5, 0.5, paste("AUC =", round(auc_xg_smote_06, 2)), adj = c(0.5, 0.5))
dev.off()



# predicted <- predict(model_rpart_unsmote, newdata = testing, type="prob")
# labels <- ifelse(testing$HeartDisease == "Yes", 1, 0)
# pred <- prediction(predicted$Yes, labels)
# perf <- performance(pred, "prec", "rec")
# auc  <- performance(pred, 'auc')
# auc  <- unlist(slot(auc,"y.values"))
# plot(perf, main = "ROC Curve")
# legend("bottomright", legend = paste("AUC =", round(auc, 2)), bty = "n")