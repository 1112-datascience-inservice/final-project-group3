library('rpart')
library(randomForest)
library(smotefamily)
library('performanceEstimation')
library('fastDummies')
library('dplyr')
# library('DMwR2')
library('caret')
library('pROC')

options(warn=-1)

transform_data_column <- function (df) {
    df$AgeCategory[df$AgeCategory=='18-24'] <- 21
    df$AgeCategory[df$AgeCategory=='25-29'] <- 27
    df$AgeCategory[df$AgeCategory=='30-34'] <- 32
    df$AgeCategory[df$AgeCategory=='35-39'] <- 37
    df$AgeCategory[df$AgeCategory=='40-44'] <- 42
    df$AgeCategory[df$AgeCategory=='45-49'] <- 47
    df$AgeCategory[df$AgeCategory=='50-54'] <- 52
    df$AgeCategory[df$AgeCategory=='55-59'] <- 57
    df$AgeCategory[df$AgeCategory=='60-64'] <- 62
    df$AgeCategory[df$AgeCategory=='65-69'] <- 67
    df$AgeCategory[df$AgeCategory=='70-74'] <- 72
    df$AgeCategory[df$AgeCategory=='75-79'] <- 77
    df$AgeCategory[df$AgeCategory=='80 or older'] <-80
    df$AgeCategory<-as.numeric(df$AgeCategory)

    df$Smoking<- ifelse(df$Smoking=='Yes',1,0)
    df$AlcoholDrinking<-ifelse(df$AlcoholDrinking=='Yes',1,0)
    df$Stroke<-ifelse(df$Stroke=='Yes',1,0)
    df$DiffWalking<-ifelse(df$DiffWalking=='Yes',1,0)
    df$Sex<-ifelse(df$Sex=='Male',1,0)
    df$PhysicalActivity<-ifelse(df$PhysicalActivity=='Yes',1,0)
    df$Asthma<-ifelse(df$Asthma=='Yes',1,0)
    df$KidneyDisease<-ifelse(df$KidneyDisease=='Yes',1,0)
    df$SkinCancer<-ifelse(df$SkinCancer=='Yes',1,0)
    df <- dummy_cols(df, select_columns = c('Race', 'Diabetic', 'GenHealth'), remove_selected_columns = TRUE)

    #Rename the dummy variables
    # df<-rename(df
    #     , Race_American_Indian_Alaskan_Native = 'Race_American Indian/Alaskan Native'
    #     ,Diabetic_No_borderline_diabetes ='Diabetic_No, borderline diabetes'
    #     , Diabetic_Yes_during_pregnancy ='Diabetic_Yes (during pregnancy)'
    #     ,GenHealth_Very_Good='GenHealth_Very good')
    colnames(df)[colnames(df) == 'Race_American Indian/Alaskan Native'] <- 'Race_American_Indian_Alaskan_Native'
    colnames(df)[colnames(df) == 'Diabetic_No, borderline diabetes'] <- 'Diabetic_No_borderline_diabetes'
    colnames(df)[colnames(df) == 'Diabetic_Yes (during pregnancy)'] <- 'Diabetic_Yes_during_pregnancy'
    colnames(df)[colnames(df) == 'GenHealth_Very good'] <- 'GenHealth_Very_Good'

    df$HeartDisease <- ifelse(df$HeartDisease=='Yes', 1,0)
    df$HeartDisease <- as.factor(df$HeartDisease)

    return(df)
}

split_data <- function (df) {
    # Split train & validation
    train.index <- createDataPartition(df$HeartDisease, p = .7, list = FALSE)
    train_df <- df[train.index,]
    valid_df <- df[-train.index,]
    # print(nrow(train_df))
    # print(table(train_df$HeartDisease))
    # print(nrow(valid_df))
    # print(table(valid_df$HeartDisease))
    return(list(train_df, valid_df))
}

balance_data <- function (df) {
    # Handle Class Imbalancy
    # df <- SMOTE(HeartDisease ~ ., data=df[2:101, ], perc.over = 3, K = 5, perc.under = 1)

    # Old smote method
    # smote_data <- SMOTE(df[,-1], df[,1], K=5, dup_size=8)
    # colnames(smote_data$data)[colnames(smote_data$data) == "class"] <- "HeartDisease"
    # smote_data$data <- smote_data$data[, c(ncol(smote_data$data), 1:(ncol(smote_data$data)-1))]
    # df <- smote_data$data
    # return(df)

    # New smote method
    # smote_data <- smote(HeartDisease ~ ., data = df[1:nrow(df),])
    # smote_data <- smote(HeartDisease ~ ., data = df)
    # smote_data <- na.omit(smote_data)
    # smote_file <- "smoted_data.csv"
    # dir.create(dirname(smote_file), recursive = TRUE, showWarnings = FALSE)
    # write.csv(smote_data, smote_file, row.names = FALSE, quote = FALSE)
    # stop("smoted_data.csv is generated.")
    smote_data <- read.csv("../data/smoted_data.csv", header=TRUE)
    return(smote_data)
}


# Initialize parameters
fold <- 3
input_file <- NA
# output_file <- NA
i <- 1
start_time <- Sys.time()

# Read & parse parameters
args = commandArgs(trailingOnly=TRUE)
if (length(args)==0) {
    stop("USAGE: Rscript Jude_111971024.R --input heart_2020_cleaned.csv", call.=FALSE)
}
while (i < length(args)) {
    if (args[i] == "--fold") {
        fold <- args[i+1]
        i <- i+1
    } 
    else if (args[i] == "--input") {
        input_file <- args[i+1]
        i <- i+1
    } 
    # else if (args[i] == "--output") {
    #     output_file <- args[i+1]
    #     i <- i+1
    # } 
    else {
        stop(paste("Unknown flag", args[i]), call.=FALSE)
    }
    i <- i+1
}

# Check parameters
if (is.na(fold)) {
    stop("Missing required parameters '--fold'", call.=FALSE)
}
if (any(is.na(input_file))) {
    stop("missing input file", call.=FALSE)
}
# if (any(is.na(output_file))) {
#     stop("missing output file", call.=FALSE)
# }
if (!file.exists(input_file)) {
    stop(paste("Input path does't exist: ", input_file), call.=FALSE)
}
message('input_file: ',input_file)

# Check fold
fold <- as.integer(fold)
if (fold < 3) {
    stop("fold must bigger than 3!", call.=FALSE)
}

df <- read.csv(input_file, header=TRUE)
set.seed(42)

# Shuffle samples
df <- df[sample(1:nrow(df)), ]

# Testing
# df <- df[1:10000,]
# df <- df[1:nrow(df) - 100000,]

df <- transform_data_column(df=df)
# selected_features = c('HeartDisease', 'BMI', 'AlcoholDrinking', 'Stroke', 'PhysicalHealth', 'MentalHealth',
#  'DiffWalking', 'Sex', 'AgeCategory', 'Asthma', 'KidneyDisease', 'Race_Black', 
#  'Race_Hispanic', 'Race_White', 'Diabetic_Yes', 'GenHealth_Fair', 'GenHealth_Poor')
# df <- df[, selected_features]
# fold_size <- nrow(df) %/% fold

result <- split_data(df=df)
train_df <- result[[1]]
valid_df <- result[[2]]

message('Before somte: ')
print(table(train_df$HeartDisease))
train_df <- balance_data(df=train_df)
# train_df <- train_df[, selected_features]
train_df$HeartDisease <- as.factor(train_df$HeartDisease)
message('After somte: ')
print(table(train_df$HeartDisease))

model <- randomForest(HeartDisease ~ ., data=train_df, ntree=100, mtry=3, importance=TRUE)
val_pred <- predict(model, valid_df, type="class")
# print(val_pred)
# stop()
# val_acc <- round(mean(val_pred == valid_df$HeartDisease), 2)
# message(val_acc)

# Importance
feature_importance <- importance(model)
print(feature_importance)
pdf("../results/varImpPlot.pdf")
varImpPlot(model)
dev.off()
# selected_features <- names(feature_importance)[feature_importance > threshold]
# train_selected <- train_df[, c("HeartDisease", selected_features)]


confusion_matrix <- table(valid_df$HeartDisease, val_pred)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
TN <- sum(valid_df$HeartDisease == 0 & val_pred == 0)
FP <- sum(valid_df$HeartDisease == 0 & val_pred == 1)
specificity <- TN / (TN + FP)
f1_score <- 2 * precision * recall / (precision + recall)

accuracy <- round(accuracy, 2)
precision <- round(precision, 2)
recall <- round(recall, 2)
specificity <- round(specificity, 2)
f1_score <- round(f1_score, 2)


message('Confusion Matrix:')
print(confusion_matrix)
message('Accuracy:', accuracy)
message('Precision:', precision)
message('sensitivity(Recall):', recall)
message('Specificity:', specificity)
message('F1 Score:', f1_score)

#AUC
roc_obj <- roc(valid_df$HeartDisease, as.numeric(val_pred))
auc <- auc(roc_obj)
auc <- round(auc, 2)
message('AUC:', auc)



#ROC
pdf("../results/roc_curve_after_smote.pdf")
plot(roc_obj, main = "ROC Curve")
abline(0, 1, lty = 2)  # 繪製對角線
legend("bottomright", legend = paste("AUC =", auc), bty = "n")  # 在圖例中顯示 AUC
dev.off()

# Write to file
result_df <- data.frame(
  Accuracy = accuracy,
  Precision = precision,
  Sensitivity = recall,
  Specificity = specificity,
  F1_Score = f1_score,
  AUC = auc
)
output_file <- "../results/result.csv"
dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
write.csv(result_df, output_file, row.names = FALSE, quote = FALSE)

end_time <- Sys.time()
cat("Time takens: ", end_time - start_time, ' seconds.\n')
