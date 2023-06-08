if (require(xgboost)) {
  message("library xgboost load success.")
} else {
  stop("library xgboost not exist, please install.")
}

xgboost_model <- function(train_df, test_df) {
    # Prepare Model Data
    dm_train_df <- xgb.DMatrix(data = as.matrix(train_df[,-1]), label = as.numeric(train_df$HeartDisease))
    dm_test_df <- xgb.DMatrix(data = as.matrix(test_df[,-1]), label = as.numeric(test_df$HeartDisease))

    model <- xgb.train(data=dm_train_df,
                        max.depth=6,
                        eta=0.3,
                        nthread = 4,
                        eval.metric = "error",
                        objective = "binary:logistic"
                      )

    predict_prob<-predict(model, dm_test_df)

    threshold <- 0.5
    predictions <- ifelse(predict_prob >= threshold, 1, 0)

    return(predictions)
}