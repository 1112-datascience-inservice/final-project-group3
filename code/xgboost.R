if (require(xgboost)) {
  print("library xgboost load success.")
} else {
  print("library xgboost not exist, start to install.")
  install.packages('xgboost', repos = "http://cran.us.r-project.org")
  if (require(xgboost)) {
    print('library xgboost load success.')
  } else {
    stop("library xgboost load failed.")
  }
}

xgboost_model <- function(train_df, test_df) {
    # Prepare Model Data
    # dm_train_df <- xgb.DMatrix(data = as.matrix(train_df[,-1]), label = as.numeric(train_df$HeartDisease))
    # dm_test_df <- xgb.DMatrix(data = as.matrix(test_df[,-1]), label = as.numeric(test_df$HeartDisease))

    # model <- xgb.train(data=dm_train_df,
    #                     max.depth=6,
    #                     eta=0.3,
    #                     nthread = 4,
    #                     eval.metric = "error",
    #                     objective = "binary:logistic"
    #                   )

    # predict_prob<-predict(model, dm_test_df)

    # threshold <- 0.5
    # predictions <- ifelse(predict_prob >= threshold, 1, 0)

    # return(predictions)

    # xgboost with smote
    # 加載訓練數據和測試數據，並準備特徵矩陣和標籤
    train_features_smote <- as.matrix(train_df[, -1])
    train_labels_smote <- as.matrix(factor(train_df[, 1]))
    train_labels_smote <- ifelse(train_labels_smote=='No', 0,1)
    test_features <- as.matrix(test_df[, -1])
    test_labels <- as.matrix(test_df[, 1])
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
}