if (require(rpart)) {
  message("library rpart load success.")
} else {
  stop("library rpart not exist, please install.")
}

train_decision_tree_model <- function(train_df) {
    base_cols <- "BMI+Smoking+Stroke+DiffWalking+Sex+AgeCategory+Asthma+KidneyDisease+Race_Hispanic+GenHealth_Excellent+GenHealth_Fair+GenHealth_Good+GenHealth_Poor"
    model <- rpart(paste("HeartDisease", base_cols, sep="~"), data=train_df, control=rpart.control(minsplit = 5, minbucket = 5, cp = 0.001), method="class")

    return(model)
}