source("data_preprocess.R")
source("decision_tree.R")
source("random_forest.R")
source("xgboost.R")
source("evaluation.R")


#input資料檢查
args <- commandArgs(trailingOnly=TRUE)

i<-1
data_source <- NA
model_selection <- NA
while (i < length(args)) {
  if (args[i] == "--data_source" || args[i] == "--d") {
    data_source<-args[i+1]
    i <- i+1
  } else if (args[i] == "--model" || args[i] == "--m") {
    model_selection<-args[i+1]
    i <- i+1
  } else {
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}

if (is.na(data_source)) {
  stop("Missing required parameters '--data_source' or '--d'", call.=FALSE)
} else if (data_source != 1 && data_source != 2) {
	stop("data_source must be 1(origin) or 2(pre-processed)", call.=FALSE)
} else {
  message("data_source using is ", data_source)
}

if (is.na(model_selection)) {
  stop("Missing required parameters '--model' or '--m'", call.=FALSE)
} else if (model_selection != 1 && model_selection != 2 && model_selection != 3) {
	stop("model must be 1(decision tree) or 2(random forest) or 3(xgboost)", call.=FALSE)
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
		model <- train_decision_tree_model(train_df)
	} else if (model_selection == 2) {
		# Random Forest
		model <- train_random_forest_model(train_df)
	} else if (model_selection == 3) {
		#XGBoost - xgboost.R
		model <- train_xgboost_model(train_df)
	}
} else {
	stop("訓練資料取得失敗")
}


#test data predict
predictions = predict(model, newdata = test_df, type = "class")


#評估結果
evaluation(test_df$HeartDisease, predictions)


