if (require(xgboost)) {
  message("library xgboost load success.")
} else {
  stop("library xgboost not exist, please install.")
}

train_xgboost_model <- function(train_df) {
    # Prepare Model Data
    dm_train_df <- xgb.DMatrix(data = as.matrix(train_df[,-1]), label = as.numeric(train_df$HeartDisease))

    model <- xgb.train(data=dm_train_df, max.depth=3
                     , eta=0.01, nthread = 2, nround=2000
                     , eval.metric = "error"
                     , eval.metric = "logloss"
                     , objective = "binary:logistic")

    return(model)
}