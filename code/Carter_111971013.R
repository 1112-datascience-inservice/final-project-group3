# 【1】 
# cd to path : \final-project-group3\code
# & 'C:\Program Files\R\R-4.3.0\bin\Rscript.exe' Carter_111971013.R --input ../data/heart_2020_cleaned.csv --output ../results/111971013 --model_name rpart

# 【2】
#packages
#1. rpart / rpart.plot：decision tress
#2. corrplot：correlation among cols
#3. performanceEstimation：smote to tackle unbalanced dataset
#4. dplyr：tackele dataframe
#5. ROCR：roc curve and aus



# =================================================
# =====ProgramStart================================
# =================================================

args <- commandArgs(trailingOnly=TRUE)

# =====(1-1)initialize variables================================
k_fold_num <- NA
f_in <- NA
f_out <- NA
model_name <- NA
# =====(1-2)parse necessary params================================
i<-1
while (i < length(args)) {
  if (args[i] == "--fold") {
    k_fold_num<-args[i+1]
    i <- i+1
  } else if (args[i] == "--input") {
    j <- grep("-", c(args[(i+1):length(args)], "-"))[1]
    f_in <- args[(i+1):(i+j-1)]
    i <- i+j-1
  } else if (args[i] == "--output") {
    f_out <- args[i+1]
    i <- i+1
  } else if (args[i] == "--model_name") {
    model_name<-args[i+1]
    i <- i+1
  }
  else {
    stop(paste("Unknown flag", args[i]), call.=FALSE)
  }
  i<-i+1
}


# =====(1-3)check whether parameters are correct================================
k_fold_num <- 3
if (is.na(k_fold_num)) {
  stop("Missing required parameters '--fold'", call.=FALSE)
}else{
  k_fold_num <- as.numeric(k_fold_num)
  if (k_fold_num < 3){
    stop("K-Fold number is not qualified! Recommedation number is >= 3.", call.=FALSE)
  }
}
if (is.na(model_name)) {
  stop("Missing required parameters '--model_name'", call.=FALSE)
}else{
  message("Model using is ", model_name)
  
}
if (any(is.na(f_in))) {
  stop("missing flag", call.=FALSE)
}else{
  if (!file.exists(f_in)) {
    stop(paste("Input path does't exist!!", f_in), call.=FALSE)
  }else{
    message("input file path = ", f_in)
  }
}
if (any(is.na(f_out))) {
  stop("missing flag", call.=FALSE)
}else{
  # The path which has to be fixed in windows is included as followed:
  #     「/example/input1.csv」、「\\example\\input1.csv」、「//example//input1.csv」
  # After fixing, it will all be corrected to the string started with "C:".
  #     「C:\\example\\input1.csv」
  # -----------------------------------
  # In contrast, The path which won't be fixed in windows is included as followed:
  #     「./example/input1.csv」、「../../example/input1.csv」、
  #     「.//example//input1.csv」、「.\\example\\input1.csv」
  #
  
  f_out <- sprintf("%s/%s_%s", f_out, model_name, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
  os_name = Sys.info()[['sysname']]
  if(os_name == "Windows" && (startsWith(f_out, "/") || startsWith(f_out, "\\"))  )
  {
    f_out <- gsub("[/]+", "\\\\", f_out)
    f_out <- paste("C:", f_out, sep="")
  }
  message("output file path = ", f_out)
}



# =====(2-1)read file in================================
f_in_csv <- read.csv(f_in, header = T)
dir.create(f_out, recursive = TRUE, showWarnings = FALSE)

# =====(2-2)examine data: data================================
#print(summary(f_in_csv))
# =====(2-2-1)unique col if its data isn't numeric================================
#for(i in colnames(f_in_csv))
#{
#    if(i %in% c("BMI", "PhysicalHealth", "MentalHealth", "SleepTime"))
#    {next}
#    category <- unique(f_in_csv[[i]])
#    
#    print(paste("col_name：", i))
#    print(category)
#    
#}
# =====(2-2-2)check na================================
for(i in colnames(f_in_csv))
{
    na_nums <- 0
    na_nums <- sum(is.na(f_in_csv[[i]]))
    
    if (na_nums > 0)
    {
       print(i)
       print(na_nums)
    }    

}
# =====(2-2-3)dicard duplicated row================================
library('dplyr')
duplicate<- duplicated(f_in_csv)
if (sum(duplicate) > 0){
    sprintf("has duplicated row %d", sum(duplicate))
    f_in_csv <- distinct(f_in_csv)

}

# =====(2-2-4)must-have preprocess================================
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
library('fastDummies')
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
#Rename the dummy variables
f_in_csv<-rename(f_in_csv
    , Race_American_Indian_Alaskan_Native = 'Race_American Indian/Alaskan Native'
    ,Diabetic_No_borderline_diabetes ='Diabetic_No, borderline diabetes'
    , Diabetic_Yes_during_pregnancy ='Diabetic_Yes (during pregnancy)'
    ,GenHealth_Very_Good='GenHealth_Very good')


# =====(2-2-4)Addressing unbalanced data================================
library('performanceEstimation') #instead of library('smotefamily')
#smote_f_in_csv <- smote(HeartDisease ~ ., data  =f_in_csv[1:100000,])
#for(i in colnames(smote_f_in_csv))
#{
#    na_nums <- 0
#    na_nums <- sum(is.na(smote_f_in_csv[[i]]))
#    
#    if (na_nums > 0)
#    {
#       print(i)
#       print(na_nums)
#    }    
#
#}
#head(smote_f_in_csv[is.na(smote_f_in_csv$HeartDisease),])
#nrow(smote_f_in_csv)
#nrow(smote_f_in_csv[!is.na(smote_f_in_csv$HeartDisease),])
#nrow(smote_f_in_csv[is.na(smote_f_in_csv$HeartDisease),])
#smote_f_in_csv <- smote_f_in_csv[!is.na(smote_f_in_csv$HeartDisease),]
#nrow(smote_f_in_csv)

# =====(3)Feature reduction：PCA、cor================================
# =====(3-1-(1))prcomp method================================
# log transform 
log.f_log <- log1p(f_in_csv[ ,2:30])
f_log.disease <- f_in_csv[ ,1]
# apply PCA - scale. = TRUE is highly advisable, but default is FALSE. 
disease.pca <- prcomp(log.f_log,center = TRUE, scale. = TRUE)

# =====(3-1-(2))FactoMineR method================================
#log.f_log <- log1p(f_in_csv[ ,2:30])
#f_log.disease <- f_in_csv[ ,1]
#disease.pca <- PCA(log.f_log,  graph = FALSE)

#fviz_contrib(disease.pca, choice = "var", axes = 1, top = 10)
#fviz_screeplot(disease.pca, addlabels = TRUE)

# =====(3-2)cols correlation================================
df_cor_f_in_csv <- data.frame(cor(f_in_csv[, -1]))
#cor_cols <- colnames(df_cor_f_in_csv)[df_cor_f_in_csv[1]>0]
write.csv(df_cor_f_in_csv, sprintf("%s/cor_cols.csv", f_out))


library(corrplot)
#cor(f_in_csv[, -1])
png(file=sprintf("%s/cor_plot_29col.png", f_out), width=1920, height=1080, res=141)
corrplot(cor(f_in_csv[, -1], method = "pearson"), number.cex = .9, method = "square", 
         hclust.method = "ward", order = "FPC",
         type = "full"
         , cl.cex = 1.5*0.6
         , tl.cex=1.5*0.6 ,tl.col = "black")                     
dev.off()

# =====(4-1)prepare base cols for rpart================================
base_cols <- paste(colnames(f_in_csv)[-1], collapse="+")# "V3+V4+...+V5602"
#base_cols <- paste(cor_cols, collapse="+")# "V3+V4+...+V5602"


# =====(4-2)shuffle datarows and define sub-datarows in each fold ================================
set.seed(55)
f_in_nums <- nrow(f_in_csv)
fold_interval <- round( f_in_nums / k_fold_num)
f_in_csv <- f_in_csv[sample(1:f_in_nums), ]


# =====(4-3)divide data into k-fold================================
set_ls <- c()
train_Accuracy_ls <- c()
train_Precision_ls <- c()
train_Sensitivity_ls <- c()
train_Specificity_ls <- c()
train_F1_ls <- c()
train_auc_ls <- c()
valid_Accuracy_ls <- c()
valid_Precision_ls <- c()
valid_Sensitivity_ls <- c()
valid_Specificity_ls <- c()
valid_F1_ls <- c()
valid_auc_ls <- c()
test_Accuracy_ls <- c()
test_Precision_ls <- c()
test_Sensitivity_ls <- c()
test_Specificity_ls <- c()
test_F1_ls <- c()
test_auc_ls <- c()
#model_ls <- c()
#model_fold_best <- NA

library('ROCR')
library('rpart')
library('rpart.plot')

for( m in seq(1, k_fold_num)){

  r <- m - 1
  test_lw <- 1+fold_interval*r
  test_up <- fold_interval*(r+1)
  valid_lw <- fold_interval*(r+1)+1
  valid_up <- fold_interval*(r+2)
  valid_backward_up <- 0
  
  if(valid_up > f_in_nums)
  {
    if(valid_lw > f_in_nums)
    {
      valid_lw <- 1
      valid_up <- fold_interval
    }
    else
    {
      valid_backward_up <-  fold_interval - (f_in_nums - valid_lw + 1)
    }
  }
  
  
  message(sprintf("\nThe %s-Fold: each fold has %s rows", m, fold_interval))
  message(sprintf("df_test_set is from %s to %s", test_lw, test_up ))
  
  df_test_set <- f_in_csv[test_lw:test_up,]
  
  
  
  
  if(valid_backward_up == 0)
  {
    df_validate_set <- f_in_csv[valid_lw:valid_up,]
    df_train_set <- smote(HeartDisease ~ ., data  =f_in_csv[c(-(test_lw:test_up), -(valid_lw:valid_up)),])
    #df_train_set <- f_in_csv[c(-(test_lw:test_up), -(valid_lw:valid_up)),]
    message(sprintf("df_validate_set is from %s to %s",valid_lw, valid_up))
    message(sprintf("df_train_set isn't within %s to %s and %s to %s , balanced num is %s",test_lw, test_up, valid_lw, valid_up, nrow(df_train_set)))
    
    
    
    
  }else{
    df_validate_set <- f_in_csv[c(valid_lw:f_in_nums, 1:valid_backward_up),]
    df_train_set <- smote(HeartDisease ~ ., data  =f_in_csv[c(-(test_lw:test_up), -(valid_lw:f_in_nums), -(1:valid_backward_up)),])    
    #df_train_set <- f_in_csv[c(-(test_lw:test_up), -(valid_lw:f_in_nums), -(1:valid_backward_up)),]
    
    
    message(sprintf("df_validate_set is from %s and backward to %s",valid_lw, valid_backward_up))
    message(sprintf("df_train_set isn't within %s to %s and %s and backward to %s , balanced num is %s",test_lw, test_up, valid_lw, valid_backward_up, nrow(df_train_set)))
  
  }  
  

  
  print(table(df_train_set$HeartDisease))
  print("-------------------------------")
  
  
  #drop na after smote
  df_train_set <- df_train_set[!is.na(df_train_set$HeartDisease),]
  
  # =====(4-4)model using decision tree================================
  #print(f_in_csv[-(484:805),]$V2)
  #Error in cbind(yval2, yprob, nodeprob) :
  #  number of rows of matrices must match (see arg 2)
  #Calls: rpart -> cbind  
  tryCatch( { 
  
    model <<- rpart(paste("HeartDisease", base_cols, sep="~"), #rpart(V2 ~ V3 + V4 + V5600 + V5601 + V5602,
                 data=df_train_set, control=rpart.control(
                                                    
                                                    minsplit = 5, minbucket = 5, cp = 0.002
                                                    #minbucket=30
                                                    #, cp=0.00095
                                                    #, maxcompete=4
                                                    #, maxsurrogate=5
                                                    #, usesurrogate=2
                                                    #, xval = 10
                                                    #, surrogatestyle=0
                                                    #, maxdepth=17
                                                    ), 
                 method="class")
  
  }
                  
  , error  = function(w) { print("Hey, there is a error about that all class is the same!")})
  
  #model_ls <- c(model_ls, model)
  png(file=sprintf("%s/model_fold_%s.png", f_out, r+1), width=1920, height=1080, res=141)
  rpart.plot(model)
  dev.off()

  # =====(4-6)make confusion matrix table================================
  # =====Integrate evaluation================================
  # =====roc curve and auc---precision and recall----================================
  
  
  set_ls <- c(set_ls, sprintf("fold%s", r+1))
  
  #---------------------train---------------------
  train_frame <- data.frame(truth=df_train_set$HeartDisease,
                          pred=predict(model, newdata=df_train_set, type="class"))
  print(table(train_frame))
  
  TP <- as.numeric(table(train_frame)[2,2])
  FP <- as.numeric(table(train_frame)[1,2])
  TN <- as.numeric(table(train_frame)[1,1])
  FN <- as.numeric(table(train_frame)[2,1])
  sensitivity <- format(round(TP/(TP+FN),2), nsmall = 2) # recall、rue positive rate
  specificity <- format(round(TN/(FP+TN),2), nsmall = 2) # True negative rate)
  precision   <- format(round(TP/(TP+FP),2), nsmall = 2)
  Accuracy    <- format(round((TP+TN) / (TP+FP+TN+FN) ,2), nsmall = 2)
  F1 <- format(round(  2 * ((TP/(TP+FP)) * (TP/(TP+FN))) / ((TP/(TP+FP)) + (TP/(TP+FN))) ,2), nsmall = 2)
  #F1 <- 2 * (precision * sensitivity) / (precision + sensitivity)#### your code_IV ####
  
  train_Accuracy_ls <- c(train_Accuracy_ls, Accuracy)
  train_Precision_ls <- c(train_Precision_ls, precision)
  train_Sensitivity_ls <- c(train_Sensitivity_ls, sensitivity)
  train_Specificity_ls <- c(train_Specificity_ls, specificity)
  train_F1_ls <- c(train_F1_ls, F1)
  
  predict_result <- predict(model, newdata=df_train_set, type="class")
  eval <- prediction(as.numeric(predict_result), df_train_set$HeartDisease)
  jpeg(file=sprintf("%s/train_roc_fold_%s.jpeg", f_out, r+1), quality = 100)
  plot(performance(eval,"tpr","fpr"))
  dev.off()  
  train_auc_ls <- c(train_auc_ls, format(round(attributes(performance(eval,'auc'))$y.values[[1]], 5), nsmall = 5))
    
  #---------------------test---------------------
  test_frame <- data.frame(truth=df_test_set$HeartDisease,
                          pred=predict(model, newdata=df_test_set, type="class"))
  print(table(test_frame))
  TP <- as.numeric(table(test_frame)[2,2])
  FP <- as.numeric(table(test_frame)[1,2])
  TN <- as.numeric(table(test_frame)[1,1])
  FN <- as.numeric(table(test_frame)[2,1])
  sensitivity <- format(round(TP/(TP+FN),2), nsmall = 2) # recall、rue positive rate
  specificity <- format(round(TN/(FP+TN),2), nsmall = 2) # True negative rate)
  precision   <- format(round(TP/(TP+FP),2), nsmall = 2)
  Accuracy    <- format(round((TP+TN) / (TP+TN+FP+FN),2), nsmall = 2)  
  F1 <- format(round(  2 * ((TP/(TP+FP)) * (TP/(TP+FN))) / ((TP/(TP+FP)) + (TP/(TP+FN))) ,2), nsmall = 2)
  
  test_Accuracy_ls <- c(test_Accuracy_ls, Accuracy)
  test_Precision_ls <- c(test_Precision_ls, precision)
  test_Sensitivity_ls <- c(test_Sensitivity_ls, sensitivity)
  test_Specificity_ls <- c(test_Specificity_ls, specificity)
  test_F1_ls <- c(test_F1_ls, F1)
  
  predict_result <- predict(model, newdata=df_test_set, type="class")
  eval <- prediction(as.numeric(predict_result), df_test_set$HeartDisease)
  jpeg(file=sprintf("%s/test_roc_fold_%s.jpeg", f_out, r+1), quality = 100)
  plot(performance(eval,"tpr","fpr"))
  dev.off()  
  test_auc_ls <- c(test_auc_ls, format(round(attributes(performance(eval,'auc'))$y.values[[1]], 5), nsmall = 5))
    
    
  #---------------------valid---------------------
  valid_frame <- data.frame(truth=df_validate_set$HeartDisease,
                          pred=predict(model, newdata=df_validate_set, type="class"))   
  print(table(valid_frame))
  TP <- as.numeric(table(valid_frame)[2,2])
  FP <- as.numeric(table(valid_frame)[1,2])
  TN <- as.numeric(table(valid_frame)[1,1])
  FN <- as.numeric(table(valid_frame)[2,1])
  sensitivity <- format(round(TP/(TP+FN),2), nsmall = 2) # recall、rue positive rate
  specificity <- format(round(TN/(FP+TN),2), nsmall = 2) # True negative rate)
  precision   <- format(round(TP/(TP+FP),2), nsmall = 2)
  Accuracy    <- format(round((TP+TN) / (TP+TN+FP+FN),2), nsmall = 2)  
  F1 <- format(round(  2 * ((TP/(TP+FP)) * (TP/(TP+FN))) / ((TP/(TP+FP)) + (TP/(TP+FN))) ,2), nsmall = 2)
  
  valid_Accuracy_ls <- c(valid_Accuracy_ls, Accuracy)
  valid_Precision_ls <- c(valid_Precision_ls, precision)
  valid_Sensitivity_ls <- c(valid_Sensitivity_ls, sensitivity)
  valid_Specificity_ls <- c(valid_Specificity_ls, specificity)  
  valid_F1_ls <- c(valid_F1_ls, F1)
  
  predict_result <- predict(model, newdata=df_validate_set, type="class")
  eval <- prediction(as.numeric(predict_result), df_validate_set$HeartDisease)
  jpeg(file=sprintf("%s/valid_roc_fold_%s.jpeg", f_out, r+1), quality = 100)
  plot(performance(eval,"tpr","fpr"))
  dev.off()  
  valid_auc_ls <- c(valid_auc_ls, format(round(attributes(performance(eval,'auc'))$y.values[[1]], 5), nsmall = 5))
    
      
  
  message(sprintf("fold%s's train_correct_percent is %s", m, tail(train_Accuracy_ls, n=1)))
  message(sprintf("fold%s's test_correct_percent is %s", m, tail(test_Accuracy_ls, n=1)))
  message(sprintf("fold%s's validate_correct_percent is %s", m, tail(valid_Accuracy_ls, n=1)))
  
  
  
}

# =====(5)write out result================================
out_data <- data.frame(
                set = set_ls, 
                train_Accuracy = train_Accuracy_ls, 
                test_Accuracy = test_Accuracy_ls, 
                valid_Accuracy = valid_Accuracy_ls, 
                
                train_Precision = train_Precision_ls, 
                test_Precision = test_Precision_ls, 
                valid_Precision = valid_Precision_ls, 
                
                train_Sensitivity = train_Sensitivity_ls, 
                test_Sensitivity = test_Sensitivity_ls, 
                valid_Sensitivity = valid_Sensitivity_ls,
                
                train_Specificity = train_Specificity_ls, 
                test_Specificity = test_Specificity_ls,                 
                valid_Specificity = valid_Specificity_ls,

                train_F1 = train_F1_ls, 
                test_F1 = test_F1_ls,                 
                valid_F1 = valid_F1_ls,
                
                train_auc = train_auc_ls,
                test_auc = test_auc_ls,
                valid_auc = valid_auc_ls
                )
                
index <- sapply(out_data[,c( "train_Accuracy", "test_Accuracy", "valid_Accuracy"
                                , "train_Precision", "test_Precision", "valid_Precision"
                                , "train_Sensitivity", "test_Sensitivity", "valid_Sensitivity"
                                , "train_Specificity", "test_Specificity", "valid_Specificity"
                                , "train_F1", "test_F1", "valid_F1"
                                , "train_auc", "test_auc", "valid_auc")], which.max)

out_data<-rbind(out_data,c('best', set_ls[index]))               
write.csv(out_data, sprintf("%s/every_fold_evaluation_result.csv", f_out))            


# =====(5)evaluation================================
# =====(5-1) precision and recall================================
# =====(5-2) roc curve and auc================================
#library('ROCR')
## ROC evaluation
#predict_result <- predict(model, newdata=df_validate_set, type="class")
#eval <- prediction(as.numeric(predict_result), df_validate_set$HeartDisease)
#plot(performance(eval,"tpr","fpr"))
## AUC evaluation
#print(attributes(performance(b,'auc'))$y.values[[1]])
## Making a double density plot
#ggplot(data=df_validate_set) + 
#  geom_density(aes(x=HeartDisease,color=HeartDisease,linetype=HeartDisease))





