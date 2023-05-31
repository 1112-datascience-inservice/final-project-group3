library('fastDummies')
library('dplyr')

get_data <- function() {

	f_in = '../data/heart_2020_cleaned.csv'
	
	f_in_csv <- read.csv(f_in, header = T)
	
	duplicate<- duplicated(f_in_csv)
	if (sum(duplicate) > 0){
	    sprintf("has duplicated row %d", sum(duplicate))
	    f_in_csv <- distinct(f_in_csv)
	}
	
	# Age should be numerical, I will convert age variable into numeric variable taking mean age of each level of age.
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='18-24'] <- 21
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='25-29'] <- 27
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='30-34'] <- 32
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='35-39'] <- 37
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='40-44'] <- 42
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='45-49'] <- 47
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='50-54'] <- 52
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='55-59'] <- 57
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='60-64'] <- 62
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='65-69'] <- 67
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='70-74'] <- 72
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='75-79'] <- 77
	f_in_csv$AgeCategory[f_in_csv$AgeCategory=='80 or older'] <-80
	f_in_csv$AgeCategory<-as.numeric(f_in_csv$AgeCategory)
	
	
	#Encode categorical variables
	f_in_csv$HeartDisease<- ifelse(f_in_csv$HeartDisease=='Yes',1,0)
	f_in_csv$Smoking<- ifelse(f_in_csv$Smoking=='Yes',1,0)
	f_in_csv$AlcoholDrinking<-ifelse(f_in_csv$AlcoholDrinking=='Yes',1,0)
	f_in_csv$Stroke<-ifelse(f_in_csv$Stroke=='Yes',1,0)
	f_in_csv$DiffWalking<-ifelse(f_in_csv$DiffWalking=='Yes',1,0)
	f_in_csv$Sex<-ifelse(f_in_csv$Sex=='Male',1,0)
	f_in_csv$PhysicalActivity<-ifelse(f_in_csv$PhysicalActivity=='Yes',1,0)
	f_in_csv$Asthma<-ifelse(f_in_csv$Asthma=='Yes',1,0)
	f_in_csv$KidneyDisease<-ifelse(f_in_csv$KidneyDisease=='Yes',1,0)
	f_in_csv$SkinCancer<-ifelse(f_in_csv$SkinCancer=='Yes',1,0)
	f_in_csv <- dummy_cols(f_in_csv, select_columns = c('Race', 'Diabetic', 'GenHealth'), remove_selected_columns = TRUE)
	
	
	f_in_csv<-rename(f_in_csv
	    , Race_American_Indian_Alaskan_Native = 'Race_American Indian/Alaskan Native'
	    ,Diabetic_No_borderline_diabetes ='Diabetic_No, borderline diabetes'
	    , Diabetic_Yes_during_pregnancy ='Diabetic_Yes (during pregnancy)'
	    ,GenHealth_Very_Good='GenHealth_Very good')

}