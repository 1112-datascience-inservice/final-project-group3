# Make sure your path is "/final-project-group3/code"
# Please Use Command Rscript xgboost_model.R
# Use packages
library(xgboost)
library(caret)
library(fastDummies)
library(ROCR)
library(dplyr)
library(performanceEstimation)
# Load Arg
# args <- commandArgs(trailingOnly=TRUE)
# k_fold_num <- NA
# i<-1
# while (i < length(args)) {
#   if (args[i] == "--fold") {
#     k_fold_num<-args[i+1]
#     i <- i+1
#   } else {
#     stop(paste("Unknown flag", args[i]), call.=FALSE)
#   }
#   i<-i+1
# }
# if (is.na(k_fold_num)) {
#   stop("Missing required parameters '--fold'", call.=FALSE)
# } else{
#   k_fold_num <- as.numeric(k_fold_num)
#   if (k_fold_num < 3){
#     stop("K-Fold number is not qualified! Recommedation number is >= 3.", call.=FALSE)
#   }
# }
# Read Data
train_data <- read.csv("../data/heart_2020_cleaned.csv", header = T)
# Data
coluniqe_Race <- unique(train_data$Race)
coluniqe_GenHealth <- unique(train_data$GenHealth)
coluniqe_AgeCategory <- unique(train_data$AgeCategory)
# Data Process
duplicate<- duplicated(train_data)
if (sum(duplicate) > 0) {
  sprintf("has duplicated row %d", sum(duplicate))
  train_data <- distinct(train_data) # remove duplicate data
}
# Turn Yes/No to 1/0
train_data$HeartDisease <- ifelse(train_data$HeartDisease == "Yes", 1, 0)
train_data$Smoking <- ifelse(train_data$Smoking == "Yes", 1, 0)
train_data$AlcoholDrinking <- ifelse(train_data$AlcoholDrinking == "Yes", 1, 0)
train_data$Stroke <- ifelse(train_data$Stroke == "Yes", 1, 0)
train_data$DiffWalking <- ifelse(train_data$DiffWalking == "Yes", 1, 0)
train_data$Sex <- ifelse(train_data$Sex == "Male", 1, 0)
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
#Rename the dummy variables
f_in_csv<-rename(f_in_csv
    , Race_American_Indian_Alaskan_Native = 'Race_American Indian/Alaskan Native'
    , Diabetic_No_borderline_diabetes ='Diabetic_No, borderline diabetes'
    , Diabetic_Yes_during_pregnancy ='Diabetic_Yes (during pregnancy)'
    , GenHealth_Very_Good='GenHealth_Very good')
# K fold
set.seed(55)
k <- 5
train_data <- train_data[sample(nrow(train_data)),]
folds <- cut(seq(1, nrow(train_data)), breaks = k, labels = FALSE)
test_set <- train_data[folds == 1,]
validation_set <- train_data[folds == 2,]
training_set <- smote(HeartDisease ~ ., data = train_data[!(folds %in% c(1, 2)), ])
#drop na after smote
training_set <- training_set[!is.na(training_set$HeartDisease),]
# Prepare Model Data
dtrain <- xgb.DMatrix(data = as.matrix(training_set[,-1]), label = as.numeric(training_set$HeartDisease))
dvalidation <- xgb.DMatrix(data = as.matrix(validation_set[,-1]), label = as.numeric(validation_set$HeartDisease))
dtest <- xgb.DMatrix(data = as.matrix(test_set[,-1]), label = as.numeric(test_set$HeartDisease))
# XGboost Model
bst <- xgb.train(data=dtrain, max.depth=3
                 , eta=0.01, nthread = 2, nround=2000
                 , eval.metric = "error"
                 , eval.metric = "logloss"
                 , objective = "binary:logistic")
print(xgb.importance(model = bst))
xgb.plot.importance(importance_matrix = xgb.importance(model = bst))
# Without Smote to do XGboost
training_set_no_smote <- train_data[!(folds %in% c(1, 2)), ]
dtrain_no_smote <- xgb.DMatrix(data = as.matrix(training_set_no_smote[,-1]), label = as.numeric(training_set_no_smote$HeartDisease))
bst_no_smote <- xgb.train(data=dtrain, max.depth=3
                          , eta=0.01, nthread = 2, nround=2000
                          , eval.metric = "error"
                          , eval.metric = "logloss"
                          , objective = "binary:logistic")
print(xgb.importance(model = bst_no_smote))
xgb.plot.importance(importance_matrix = xgb.importance(model = bst_no_smote))
# Precision
pred.val<-predict(bst,dvalidation)
pred.test<-round(predict(bst,dtest))
pr_test <- prediction(pred.test, test_set$HeartDisease)
pr_val <- prediction(pred.val, validation_set$HeartDisease)
xgboost_roc_test <- roc(test_set$HeartDisease,as.numeric(pred.test))
plot(xgboost_roc_test, print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", 
     print.thres=TRUE,main='ROC curve Test with Smote')
xgboost_roc_val <- roc(validation_set$HeartDisease,as.numeric(pred.val))
plot(xgboost_roc_val, print.auc=TRUE, auc.polygon=TRUE, 
     grid=c(0.1, 0.2),grid.col=c("green", "red"), 
     max.auc.polygon=TRUE,auc.polygon.col="skyblue", 
     print.thres=TRUE,main='ROC curve Validation with Smote')
threshold <- 0.5
predicted_test_labels <- ifelse(pred.test >= threshold, 1, 0)
predicted_val_labels <- ifelse(pred.val >= threshold, 1, 0)
# 計算準確率
accuracy_test <- sum(predicted_test_labels == test_set$HeartDisease) / length(test_set$HeartDisease)
accuracy_val <- sum(predicted_val_labels == validation_set$HeartDisease) / length(validation_set$HeartDisease)
print(paste("Test Accuracy:", accuracy_test))
print(paste("Validation Accuracy:", accuracy_val))
# ROC
prf_test <- performance(pr_test, measure = "tpr", x.measure = "fpr")
prf_val <- performance(pr_val, measure = "tpr", x.measure = "fpr")
# AUC
test_auc <- performance(pr_test, measure = "auc")
test_auc <- test_auc@y.values[[1]]
print(paste("Test AUC:", test_auc))
val_auc <- performance(pr_val, measure = "auc")
val_auc <- val_auc@y.values[[1]]
print(paste("Validation AUC:", val_auc))
# 計算 sensitivity, specificity, precision, Accuracy, F1
# TEST
tmp_df_test <- data.frame(pred_y=predicted_test_labels, y=test_set$HeartDisease)
# 患病=1, 未患病=0
TP <- sum(apply(tmp_df_test, 1, function(row) all(row == 1)), na.rm = TRUE)
FP <- sum(apply(tmp_df_test, 1, function(row) all(row == list(1, 0))), na.rm = TRUE)
TN <- sum(apply(tmp_df_test, 1, function(row) all(row == 0)), na.rm = TRUE)
FN <- sum(apply(tmp_df_test, 1, function(row) all(row == list(0, 1))), na.rm = TRUE)
test_sensitivity <- format(round(TP/(TP+FN),4), nsmall = 2) # recall、rue positive rate
test_specificity <- format(round(TN/(FP+TN),4), nsmall = 2) # True negative rate)
test_precision   <- format(round(TP/(TP+FP),4), nsmall = 2)
test_Accuracy    <- format(round((TP+TN) / (TP+FP+TN+FN) ,4), nsmall = 2)
test_F1 <- format(round(  2 * ((TP/(TP+FP)) * (TP/(TP+FN))) / ((TP/(TP+FP)) + (TP/(TP+FN))) ,4), nsmall = 2)
# Validation
tmp_df_validation <- data.frame(pred_y=predicted_val_labels, y=validation_set$HeartDisease)
# 患病=1, 未患病=0
TP <- sum(apply(tmp_df_validation, 1, function(row) all(row == 1)), na.rm = TRUE)
FP <- sum(apply(tmp_df_validation, 1, function(row) all(row == list(1, 0))), na.rm = TRUE)
TN <- sum(apply(tmp_df_validation, 1, function(row) all(row == 0)), na.rm = TRUE)
FN <- sum(apply(tmp_df_validation, 1, function(row) all(row == list(0, 1))), na.rm = TRUE)
val_sensitivity <- format(round(TP/(TP+FN),4), nsmall = 4) # recall、rue positive rate
val_specificity <- format(round(TN/(FP+TN),4), nsmall = 4) # True negative rate)
val_precision   <- format(round(TP/(TP+FP),4), nsmall = 4)
val_Accuracy    <- format(round((TP+TN) / (TP+FP+TN+FN) ,4), nsmall = 2)
val_F1 <- format(round(  2 * ((TP/(TP+FP)) * (TP/(TP+FN))) / ((TP/(TP+FP)) + (TP/(TP+FN))) ,4), nsmall = 2)

# Precision (No Smote)
pred_no_smote.val<-predict(bst_no_smote,dvalidation)
pred_no_smote.test<-predict(bst_no_smote,dtest)
pr_no_smote_test <- prediction(pred_no_smote.test, test_set$HeartDisease)
pr_no_smote_val <- prediction(pred_no_smote.val, validation_set$HeartDisease)
threshold <- 0.5
predicted_test_labels_no_smote <- ifelse(pred_no_smote.test >= threshold, 1, 0)
predicted_val_labels_no_smote <- ifelse(pred_no_smote.val >= threshold, 1, 0)
# 計算準確率
accuracy_test_no_smote <- sum(predicted_test_labels_no_smote == test_set$HeartDisease) / length(test_set$HeartDisease)
accuracy_val_no_smote <- sum(predicted_val_labels_no_smote == validation_set$HeartDisease) / length(validation_set$HeartDisease)
print(paste("Test Accuracy no smote:", accuracy_test_no_smote))
print(paste("Validation Accuracy no smote:", accuracy_val_no_smote))
# ROC
prf_test_no_smote <- performance(pr_no_smote_test, measure = "tpr", x.measure = "fpr")
prf_val_no_smote <- performance(pr_no_smote_val, measure = "tpr", x.measure = "fpr")
# AUC
test_auc_no_smote <- performance(pr_no_smote_test, measure = "auc")
test_auc_no_smote <- test_auc_no_smote@y.values[[1]]
print(paste("Test AUC no smote:", test_auc_no_smote))
val_auc_no_smote <- performance(pr_no_smote_val, measure = "auc")
val_auc_no_smote <- val_auc_no_smote@y.values[[1]]
print(paste("Validation AUC no smote:", val_auc_no_smote))
# TEST
tmp_df_test_ns <- data.frame(pred_y=predicted_test_labels_no_smote, y=test_set$HeartDisease)
# 患病=1, 未患病=0
TP <- sum(apply(tmp_df_test_ns, 1, function(row) all(row == 1)), na.rm = TRUE)
FP <- sum(apply(tmp_df_test_ns, 1, function(row) all(row == list(1, 0))), na.rm = TRUE)
TN <- sum(apply(tmp_df_test_ns, 1, function(row) all(row == 0)), na.rm = TRUE)
FN <- sum(apply(tmp_df_test_ns, 1, function(row) all(row == list(0, 1))), na.rm = TRUE)
test_ns_sensitivity <- format(round(TP/(TP+FN),4), nsmall = 2) # recall、rue positive rate
test_ns_specificity <- format(round(TN/(FP+TN),4), nsmall = 2) # True negative rate)
test_ns_precision   <- format(round(TP/(TP+FP),4), nsmall = 2)
test_ns_Accuracy    <- format(round((TP+TN) / (TP+FP+TN+FN) ,4), nsmall = 2)
test_ns_F1 <- format(round(  2 * ((TP/(TP+FP)) * (TP/(TP+FN))) / ((TP/(TP+FP)) + (TP/(TP+FN))) ,4), nsmall = 2)
# Validation
tmp_df_validation_ns <- data.frame(pred_y=predicted_val_labels_no_smote, y=validation_set$HeartDisease)
# 患病=1, 未患病=0
TP <- sum(apply(tmp_df_validation_ns, 1, function(row) all(row == 1)), na.rm = TRUE)
FP <- sum(apply(tmp_df_validation_ns, 1, function(row) all(row == list(1, 0))), na.rm = TRUE)
TN <- sum(apply(tmp_df_validation_ns, 1, function(row) all(row == 0)), na.rm = TRUE)
FN <- sum(apply(tmp_df_validation_ns, 1, function(row) all(row == list(0, 1))), na.rm = TRUE)
val_ns_sensitivity <- format(round(TP/(TP+FN),4), nsmall = 2) # recall、rue positive rate
val_ns_specificity <- format(round(TN/(FP+TN),4), nsmall = 2) # True negative rate)
val_ns_precision   <- format(round(TP/(TP+FP),4), nsmall = 2)
val_ns_Accuracy    <- format(round((TP+TN) / (TP+FP+TN+FN) ,2), nsmall = 2)
val_ns_F1 <- format(round(  2 * ((TP/(TP+FP)) * (TP/(TP+FN))) / ((TP/(TP+FP)) + (TP/(TP+FN))) ,4), nsmall = 2)
# read files
name_ls <- c('Validation with smote', 'Test with smote', 'Validation without smote', 'Test without smote')
sensitivity_ls <- c(test_sensitivity, val_sensitivity, test_ns_sensitivity, val_ns_sensitivity)
specificity_ls <- c(test_specificity, val_specificity, test_ns_specificity, val_ns_specificity)
F1score_ls <- c(test_F1, val_F1, test_ns_F1, val_ns_F1)
auc_ls <- c(test_auc, val_auc, test_auc_no_smote, val_auc_no_smote)
out_data <- data.frame(
                method = name_ls, 
                sensitivity = sensitivity_ls, 
                specificity = specificity_ls,
                F1 = F1score_ls,
                auc_ls = auc_ls,
                stringsAsFactors = F)
write.csv(out_data, "../results/xgb_output.csv", row.names = FALSE, quote = FALSE)
