if (require(dplyr)) {
  message("library dplyr load success.")
} else {
  stop("library dplyr not exist, please install.")
}

if (require(fastDummies)) {
  message("library fastDummies load success.")
} else {
  stop("library fastDummies not exist, please install.")
}

if (require(caret)) {
  message("library caret load success.")
} else {
  stop("library caret not exist, please install.")
}


get_data <- function() {

	f_in = '../data/heart_2020_cleaned.csv'
	
	df <- read.csv(f_in, header = T)
	
	duplicate<- duplicated(df)
	if (sum(duplicate) > 0){
	    sprintf("has duplicated row %d", sum(duplicate))
	    df <- distinct(df)
	}
	
	# Age should be numerical, I will convert age variable into numeric variable taking mean age of each level of age.
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
	
	
	#Encode categorical variables
	df$HeartDisease<- ifelse(df$HeartDisease=='Yes',1,0)
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
	
	
	df<-rename(df
	    , Race_American_Indian_Alaskan_Native = 'Race_American Indian/Alaskan Native'
	    ,Diabetic_No_borderline_diabetes ='Diabetic_No, borderline diabetes'
	    , Diabetic_Yes_during_pregnancy ='Diabetic_Yes (during pregnancy)'
	    ,GenHealth_Very_Good='GenHealth_Very good')


	# Shuffle samples
	df <- df[sample(1:nrow(df)), ]

	# split data train 70% test 30%
    train_index <- createDataPartition(df$HeartDisease, p = .7, list = FALSE)
    train_df <- df[train_index,]
    test_df <- df[-train_index,]

    # smote train data
    train_df <- smote(HeartDisease ~ ., data = train_df)

	return(list(train_df, test_df))
}