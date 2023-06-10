random_forest_model <- function(train_df, test_df) {

  ntree <- 100
  mtry <- 3

  accuracy_ls <- c()
  precision_ls <- c()
  sensitivity_ls <- c()
  specificity_ls <- c()
  F1score_ls <- c()
  auc_ls <- c()

  # feature 29
  model <- randomForest(factor(HeartDisease) ~ ., data=train_df, ntree=ntree, mtry=mtry, importance=TRUE)

  # Importance
  feature_importance <- importance(model)

  output_imporatance <- "../results/randomForest/varImpPlot.pdf"
  dir.create(dirname(output_imporatance), recursive = TRUE, showWarnings = FALSE)
  # print(feature_importance)
  pdf("../results/randomForest/varImpPlot.pdf")
  varImpPlot(model)
  dev.off()

  val_pred <- predict(model, test_df, type="class")
  
  confusion_matrix <- table(test_df$HeartDisease, val_pred)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  TN <- sum(test_df$HeartDisease == 0 & val_pred == 0)
  FP <- sum(test_df$HeartDisease == 0 & val_pred == 1)
  specificity <- TN / (TN + FP)
  f1_score <- 2 * precision * recall / (precision + recall)

  accuracy <- round(accuracy, 2)
  precision <- round(precision, 2)
  recall <- round(recall, 2)
  specificity <- round(specificity, 2)
  f1_score <- round(f1_score, 2)

  #AUC
  roc_obj <- roc(test_df$HeartDisease, as.numeric(val_pred))
  auc <- auc(roc_obj)
  auc <- round(auc, 2)
  # message('AUC:', auc)

  #ROC
  pdf("../results/randomForest/roc_29.pdf")
  plot(roc_obj, main = "ROC Curve")
  abline(0, 1, lty = 2)  # 繪製對角線
  legend("bottomright", legend = paste("AUC =", auc), bty = "n")  # 在圖例中顯示 AUC
  dev.off()

  accuracy_ls <- c(accuracy_ls, accuracy)
  sensitivity_ls <- c(sensitivity_ls, recall)
  specificity_ls <- c(specificity_ls, specificity)
  precision_ls <- c(precision_ls, precision)
  F1score_ls <- c(F1score_ls, f1_score)
  auc_ls <- c(auc_ls, auc)


  # feature 13
  selected_features = c('HeartDisease', 'BMI', 'Smoking', 'Stroke','DiffWalking', 'Sex', 'AgeCategory', 'Asthma', 'KidneyDisease', 'Race_Hispanic', 'GenHealth_Excellent', 'GenHealth_Good', 'GenHealth_Fair', 'GenHealth_Poor')

  train_df <- train_df[, selected_features]
  test_df <- test_df[, selected_features]

  model <- randomForest(factor(HeartDisease) ~ ., data=train_df, ntree=ntree, mtry=mtry, importance=TRUE)
  val_pred <- predict(model, test_df, type="class")

  confusion_matrix <- table(test_df$HeartDisease, val_pred)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
  recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  TN <- sum(test_df$HeartDisease == 0 & val_pred == 0)
  FP <- sum(test_df$HeartDisease == 0 & val_pred == 1)
  specificity <- TN / (TN + FP)
  f1_score <- 2 * precision * recall / (precision + recall)

  accuracy <- round(accuracy, 2)
  precision <- round(precision, 2)
  recall <- round(recall, 2)
  specificity <- round(specificity, 2)
  f1_score <- round(f1_score, 2)

  #AUC
  roc_obj <- roc(test_df$HeartDisease, as.numeric(val_pred))
  auc <- auc(roc_obj)
  auc <- round(auc, 2)
  # message('AUC:', auc)

  #ROC
  pdf("../results/randomForest/roc_13.pdf")
  plot(roc_obj, main = "ROC Curve")
  abline(0, 1, lty = 2)  # 繪製對角線
  legend("bottomright", legend = paste("AUC =", auc), bty = "n")  # 在圖例中顯示 AUC
  dev.off()

  accuracy_ls <- c(accuracy_ls, accuracy)
  sensitivity_ls <- c(sensitivity_ls, recall)
  specificity_ls <- c(specificity_ls, specificity)
  precision_ls <- c(precision_ls, precision)
  F1score_ls <- c(F1score_ls, f1_score)
  auc_ls <- c(auc_ls, auc)

# Write to file
  Feature <- c("29Features", "13Features")
  out_data <- data.frame(
    Feature = Feature,
    Accuracy = accuracy_ls,
    Precision = precision_ls,
    Sensitivity = sensitivity_ls,
    Specificity = specificity_ls,
    'F1 Score' = F1score_ls,
    AUC = auc_ls
  )
  output_file <- "../results/randomForest/result.csv"
  write.csv(out_data, output_file, row.names = FALSE, quote = FALSE)
}