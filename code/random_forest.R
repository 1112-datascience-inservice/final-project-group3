if (require(randomForest)) {
  message("library randomForest load success.")
} else {
  stop("library randomForest not exist, please install.")
}

random_forest_model <- function(train_df, test_df) {
  # base_cols <- "BMI+Smoking+Stroke+DiffWalking+Sex+AgeCategory+Asthma+KidneyDisease+Race_Hispanic+GenHealth_Excellent+GenHealth_Fair+GenHealth_Good+GenHealth_Poor"
    model <- randomForest(as.factor(HeartDisease)~. ,data=train_df, importance=TRUE, ntree=100, mtry=3, keep.forest = TRUE)

    predictions <- predict(model, test_df, type="class")

    return(predictions)
}