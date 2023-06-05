# smote from raw data
#  Rscript.exe code/xgboost_111971005.R --input data/heart_2020_cleaned.csv 
# use already smote data and save your time
#  Rscript.exe code/xgboost_111971005.R --input data/heart_2020_smote.csv 

# --------------installed packages-----------------------
# install.packages('rpart.plot')
# install.packages('ggplot2')
# install.packages('png')
# install.packages('xgboost')
# install.packages('lightgbm')
# -------------------------------------------------------
library(xgboost)
library(dplyr)
library(fastDummies)
library(caret)
library(rpart.plot)
library(performanceEstimation)
library(pROC)
library(png)

args <- commandArgs(trailingOnly = TRUE)
for (i in args){
  if (i == "--input") {
    input <- args[which(args == i) + 1]
  }
}

f_in_csv <- read.csv("data/heart_2020_cleaned.csv")
f_in_csv <- data.frame(f_in_csv)

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

# installed.packages('rpart.plot')

# set the training & testing data
data <- f_in_csv
ind_train <- createDataPartition(y = data$HeartDisease, p= 0.7, list = FALSE)
training <- data[ind_train,]
testing <- data[-ind_train,]
message("training set:")
dim(training)
message("testing set:")
dim(testing)


# use smote to select sample
# SMOTE代表合成少数类过采样技术（Synthetic Minority Oversampling Technique）
# set the dataset into train, test, valid and use smote on training data.
# install.packages("performanceEstimation")
# input = "data/heart_2020_smote.csv"
input == "data/heart_2020_cleaned.csv"
if(input == "data/heart_2020_cleaned.csv"){
  training_smote <- smote(HeartDisease ~ ., data = training)
  write.csv(training_smote, 'data/heart_2020_smote.csv', row.names = FALSE, quote = FALSE)
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

#install.packages("xgboost")
# xgboost with unsmote
library(xgboost)
message("************************")
model_select_message <- sprintf("Training model is: %s", "xgboost")
print(model_select_message)

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
# predict test data
predicted_xgboost_smote <- predict(model_xgboost_smote, test_matrix)

get_threshold_cf <- function(predicted, labels, txt_name){
  sequence <- seq(from = 0, to = 0.95, by = 0.05)
  threshold_ls <- c()
  accuracy_ls <- c()
  precision_ls <- c()
  sensitivity_ls <- c()
  specificity_ls <- c()
  F1score_ls <- c()
  max_f1 = 0
  max_f1_threshold = 0
  for( i in sequence){
    temp_pred <- ifelse(predicted >= i, 1, 0)
    cf <- confusionMatrix(factor(temp_pred), factor(labels), positive='1')
    if(max_f1 < cf[[4]][7]){
      max_f1 = cf[[4]][7]
      max_f1_threshold = i
    }
    accuracy_ls <- c(accuracy_ls, cf[[3]][1])
    precision_ls <- c(precision_ls, cf[[3]])
    sensitivity_ls <- c(sensitivity_ls, cf[[4]][1])
    specificity_ls <- c(specificity_ls, cf[[4]][2])
    precision_ls <- c(precision_ls, cf[[4]][5])
    F1score_ls <- c(F1score_ls, cf[[4]][7])
    threshold_ls <- c(threshold_ls, i)
  }
  result_frame <- data.frame(threshold=threshold_ls, sensitivity=sensitivity_ls, specificity=specificity_ls, F1=F1score_ls)
  sink(paste('results/', txt_name))
  print(result_frame)
  print(sprintf("Max F1 %s at threshold %s", max_f1, max_f1_threshold))
  print(sprintf("Null model accuracy: %s", cf[[3]][5]))
  sink()
  return(max_f1_threshold)
}

max_f1_threshold <- get_threshold_cf(predicted_xgboost_smote, test_labels, txt_name='xgboost-smote-threshold-cf')
binary_predictions_xg_smote <- ifelse(predicted_xgboost_smote > max_f1_threshold, 1, 0)

accuracy_ls <- c()
precision_ls <- c()
sensitivity_ls <- c()
specificity_ls <- c()
F1score_ls <- c()
auc_ls <- c()

cf <- confusionMatrix(factor(binary_predictions_xg_smote), factor(test_labels), positive='1')
test_roc = roc(factor(test_labels) ~ predicted_xgboost_smote, plot = TRUE,
               print.auc = TRUE,legacy.axes=TRUE) 
auc <- auc(test_roc)
png("results/xgboost-smote-roc.png", width = 600, height = 600)
test_roc = roc(factor(test_labels) ~ predicted_xgboost_smote, plot = TRUE,
               print.auc = TRUE,legacy.axes=TRUE) 
dev.off()

accuracy_ls <- c(accuracy_ls, cf[[3]][1])
sensitivity_ls <- c(sensitivity_ls, cf[[4]][1])
specificity_ls <- c(specificity_ls, cf[[4]][2])
precision_ls <- c(precision_ls, cf[[4]][5])
F1score_ls <- c(F1score_ls, cf[[4]][7])
auc_ls <- c(auc_ls, auc)

# 繪製importance matrix
importance_matrix_smote <- xgb.importance(model = model_xgboost_smote)
importance_matrix_df <- data.frame(importance_matrix_smote)
write.csv(importance_matrix_df, 'results/xgboost-importance-features.csv', row.names = FALSE, quote = FALSE)
# install.packages("Ckmeans.1d.dp")
library(Ckmeans.1d.dp)
xgb.ggplot.importance(importance_matrix = importance_matrix_smote)
png("results/xgboost-importance-features.png", width = 600, height = 600)
xgb.ggplot.importance(importance_matrix = importance_matrix_smote)
dev.off()

# select 13 features
# "BMI+Smoking+Stroke+DiffWalking+Sex+AgeCategory+Asthma+KidneyDisease+Race_Hispanic+GenHealth_Excellent+GenHealth_Fair+GenHealth_Good+GenHealth_Poor"
features_selected <- c("BMI","Smoking","Stroke","DiffWalking","Sex","AgeCategory","Asthma","KidneyDisease","Race_Hispanic","GenHealth_Excellent","GenHealth_Fair","GenHealth_Good","GenHealth_Poor")
# features_selected <- c(1, 2, 4, 7, 8, 9, )
train_features_selected <- training_smote_NonNA[, features_selected]
test_features_selected <- testing[, features_selected]
train_features <- as.matrix(train_features_selected)
train_labels <- as.matrix(factor(training_smote_NonNA[, 1]))
train_labels <- ifelse(train_labels=='No', 0,1)
test_features <- as.matrix(test_features_selected)
test_labels <- as.matrix(testing[, 1])
test_labels <- ifelse(test_labels=='No', 0,1)
train_matrix_selected <- xgb.DMatrix(data = train_features, label = train_labels)
test_matrix_selected <- xgb.DMatrix(data = test_features)

model_xgboost_selected <- xgb.train(params = params, data = train_matrix_selected, nrounds = 100)
# predict test data
predicted_features_selected <- predict(model_xgboost_selected, test_features)
selected_roc = roc(factor(test_labels) ~ predicted_features_selected, plot = FALSE,
               print.auc = TRUE,legacy.axes=TRUE)
auc <- auc(selected_roc)

max_fa_threshold_selected <- get_threshold_cf(predicted_features_selected, test_labels, txt_name='xgboost-selected-threshold-cf')
binary_predictions_selected <- ifelse(predicted_features_selected > max_fa_threshold_selected, 1, 0)
cf <- confusionMatrix(factor(binary_predictions_selected), factor(test_labels), positive='1')
accuracy_ls <- c(accuracy_ls, cf[[3]][1])
sensitivity_ls <- c(sensitivity_ls, cf[[4]][1])
specificity_ls <- c(specificity_ls, cf[[4]][2])
precision_ls <- c(precision_ls, cf[[4]][5])
F1score_ls <- c(F1score_ls, cf[[4]][7])
auc_ls <- c(auc_ls, auc)
method <- c("29Features", "13Features")
out_data <- data.frame(
  Method = method,
  Accuracy = accuracy_ls,
  Precision = precision_ls,
  Sensitivity = sensitivity_ls,
  specificity_ls = specificity_ls,
  'F1 Score' = F1score_ls,
  AUC = auc_ls,
  stringsAsFactors = FALSE
)
write.csv(out_data, 'results/xgboost-results.csv', row.names = FALSE, quote = FALSE)