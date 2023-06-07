source("data_preprocess.R")


if (require(randomForest)) {
  message("library randomForest load success.")
} else {
  stop("library randomForest not exist, please install.")
}

if (require(rpart)) {
  message("library rpart load success.")
} else {
  stop("library rpart not exist, please install.")
}

#input資料檢查
args <- commandArgs(trailingOnly=TRUE)

i<-1
data_source <- NA
model_selection <- NA
while (i < length(args)) {
  if (args[i] == "--data_source" || args[i] == "--d") {
    data_source<-args[i+1]
    i <- i+1
  } else if (args[i] == "--model") {
    model_selection<-args[i+1]
    i <- i+1
  } else {
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

if (is.na(data_source)) {
  	stop("Missing required parameters '--data_source'", call.=FALSE)
} else if (data_source != 1 && data_source != 2) {
	stop("data_source must 1(origin) or 2(pre-processed)", call.=FALSE)
} else {
  message("data_source using is ", data_source)
}

if (is.na(model_selection)) {
  stop("Missing required parameters '--model'", call.=FALSE)
} else if (model_selection != 1 && model_selection != 2 && model_selection != 3) {
	stop("model must 1(decision tree) or 2(random forest) or 3(xgboost)", call.=FALSE)
}else{
  message("model using is ", model_selection)  
}


#取得資料
train_df = data.frame()
test_df = data.frame()
if (data_source == 1) {
	data_get_result <- get_origin_data()	
	train_df <- data_get_result[[1]]
	test_df <- data_get_result[[2]]
} else if (data_source == 2) {
	#讀取已smote檔案
	data_get_result <- get_preprocessed_data()
	train_df <- data_get_result[[1]]
	test_df <- data_get_result[[2]]
}

summary(train_df)
summary(test_df)

#根據input 選擇model
model <- NA
if (nrow(train_df) > 0) {
	if (model_selection == 1) {
		# Decision Tree
		base_cols <- "BMI+Smoking+Stroke+DiffWalking+Sex+AgeCategory+Asthma+KidneyDisease+Race_Hispanic+GenHealth_Excellent+GenHealth_Fair+GenHealth_Good+GenHealth_Poor"
    	model <- rpart(paste("HeartDisease", base_cols, sep="~"), data=train_df, control=rpart.control(minsplit = 5, minbucket = 5, cp = 0.001), method="class")
	} else if (model_selection == 2) {
		# Random Forest
		# base_cols <- "BMI+Smoking+Stroke+DiffWalking+Sex+AgeCategory+Asthma+KidneyDisease+Race_Hispanic+GenHealth_Excellent+GenHealth_Fair+GenHealth_Good+GenHealth_Poor"
		model <- randomForest(as.factor(HeartDisease)~. ,data=train_df, importance=TRUE, ntree=10, mtry=3, keep.forest = TRUE)
	} else if (model_selection == 3) {
		#XGBoost
		source("xgboost.R")
		model <- train_xgboost_model(train_df)
	}
} else {
	stop("訓練資料取得失敗")
}


#test data predict
predictions = predict(model, newdata = test_df, type = "class")



#評估結果
confusion_matrix <- table(test_df$HeartDisease, predictions)
# cf <- confusionMatrix(factor(binary_predictions_selected), factor(test_labels), positive='1')

print(confusion_matrix)


