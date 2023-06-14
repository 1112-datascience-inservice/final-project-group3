source("library.R")
source("data_preprocess.R")
source("decision_tree.R")
source("random_forest.R")
source("xgboost.R")

message("=======START=======")
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
	data_source_str <- case_when(
		data_source == 1 ~ 'orgin data',
		data_source == 2 ~ 'pre-processed data',
		TRUE ~ data_source
	)
  message("data_source using is ", data_source_str)
}

if (is.na(model_selection)) {
  stop("Missing required parameters '--model' or '--m'", call.=FALSE)
} else if (model_selection != 1 && model_selection != 2 && model_selection != 3) {
	stop("model must be 1(decision tree) or 2(random forest) or 3(xgboost)", call.=FALSE)
}else{
	model_selection_str <- case_when(
		model_selection == 1 ~ 'decision tree',
		model_selection == 2 ~ 'random forest',
		model_selection == 3 ~ 'xgboost',
		TRUE ~ model_selection
	)
  message("model using is ", model_selection_str)  
}


if (model_selection == 1) {
	if (data_source == 2) {
		stop("抱歉 Decision Tree 不支援讀取預處理資料", call.=FALSE)
	}
	decision_tree_model()

} else {
	#取得資料
	train_df = data.frame()
	valid_df = data.frame()
	test_df = data.frame()

	if (data_source == 1) {
		data_get_result <- get_origin_data()	
		train_df <- data_get_result[[1]]
		valid_df <- data_get_result[[2]]
		test_df <- data_get_result[[3]]
	} else if (data_source == 2) {
		#讀取preprocess檔案
		data_get_result <- get_preprocessed_data()
		train_df <- data_get_result[[1]]
		valid_df <- data_get_result[[2]]
		test_df <- data_get_result[[3]]
	}

	#根據input 選擇model
	predictions = data.frame()
	if (nrow(train_df) > 0) {
		if (model_selection == 2) {
			# Random Forest
			random_forest_model(train_df, test_df)
		} else if (model_selection == 3) {
			#XGBoost - xgboost.R
			xgboost_model(train_df, test_df)
		}
	} else {
	stop("訓練資料取得失敗")
	}
}
message("=======END=======")

