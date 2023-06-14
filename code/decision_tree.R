decision_tree_model <- function() {

    # =====(1-1)initialize variables================================
    k_fold_num <- 3
    f_in <- '../data/heart_2020_cleaned.csv'
    f_out <- '../results/decision_tree/'
    
    f_out <- sprintf("%s%s", f_out, format(Sys.time(), "%Y-%m-%d_%H-%M-%S"))
    os_name = Sys.info()[['sysname']]
    if(os_name == "Windows" && (startsWith(f_out, "/") || startsWith(f_out, "\\"))  )
    {
      f_out <- gsub("[/]+", "\\\\", f_out)
      f_out <- paste("C:", f_out, sep="")
    }
   
   	message("input file path = ", f_in)
   	message("output file path = ", f_out)

    # =====(2-1)read file in================================
    f_in_csv <- read.csv(f_in, header = T)
    dir.create(f_out, recursive = TRUE, showWarnings = FALSE)
    # =====(2-1-1)chi-square test:original cols and target y================================
    chi_square_data <- c("col_order", "col_name", "X-squared", "df", "p-value")
    for(i in c(2:length(colnames(f_in_csv))))
    {
        chi_obj <- chisq.test(f_in_csv$HeartDisease, f_in_csv[,i], correct=FALSE)
        
        if(chi_obj[[3]] < 0.05)
        {
            chi_square_data <- rbind(chi_square_data, c(i, colnames(f_in_csv)[[i]], chi_obj[[1]], chi_obj[[2]], as.numeric(chi_obj[[3]])))
        }
    }

    df <- data.frame(chi_square_data)
    colnames(df) <- df[1,]
    df <- df[-1, ] 
    write.csv(df, sprintf("%s/cor_original_chi-square.csv", f_out), row.names = FALSE)
    

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

    # =====(3-2)cols correlation================================
    df_cor_f_in_csv <- data.frame(cor(f_in_csv[, -1]))
    #cor_cols <- colnames(df_cor_f_in_csv)[df_cor_f_in_csv[1]>0]
    write.csv(df_cor_f_in_csv, sprintf("%s/cor_cols.csv", f_out))


    
    #cor(f_in_csv[, -1])
    png(file=sprintf("%s/cor_plot_29col.png", f_out), width=1920, height=1080, res=141)
    corrplot(cor(f_in_csv[, -1], method = "pearson"), number.cex = .9, method = "square", 
             hclust.method = "ward", order = "FPC",
             type = "full"
             , cl.cex = 1.5*0.6
             , tl.cex=1.5*0.6 ,tl.col = "black")                     
    dev.off()

    # =====(3-3)chi-square test: preprocess cols and target y================================
    chi_square_data <- c("col_order", "col_name", "X-squared", "df", "p-value")
    for(i in c(2:length(colnames(f_in_csv))))
    {

        chi_obj <- chisq.test(f_in_csv$HeartDisease, f_in_csv[,i], correct=FALSE)
        
        if(chi_obj[[3]] < 0.05)
        {
            chi_square_data <- rbind(chi_square_data, c(i, colnames(f_in_csv)[[i]], chi_obj[[1]], chi_obj[[2]], as.numeric(chi_obj[[3]])))
            #print(c(i, chi_obj[[1]], chi_obj[[2]], chi_obj[[3]]))
            #message(sprintf("%d : %s col has relation with HeartDisease", i, colnames(f_in_csv)[[i]]))
        }
            
    }

    df <- data.frame(chi_square_data)
    colnames(df) <- df[1,]
    df <- df[-1, ] 
    write.csv(df, sprintf("%s/cor_preprocess_chi-square.csv", f_out), row.names = FALSE)


    # =====(4-1)prepare base cols for rpart================================
    base_cols <- paste(colnames(f_in_csv)[-1], collapse="+")# "V3+V4+...+V5602"
    #base_cols <- "BMI+Smoking+Stroke+DiffWalking+Sex+AgeCategory+Asthma+KidneyDisease+Race_Hispanic+GenHealth_Excellent+GenHealth_Fair+GenHealth_Good+GenHealth_Poor"
    #base_cols <- "PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+PC21+PC22"
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
    model_ls <- c()
    #model_fold_best <- NA

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
        df_train_set <- smote(HeartDisease ~ ., data  =f_in_csv[c(-(test_lw:test_up), -(valid_lw:valid_up)),])#, perc.over = 4, perc.under = 1)
        # df_train_set <- f_in_csv[c(-(test_lw:test_up), -(valid_lw:valid_up)),]
        message(sprintf("df_validate_set is from %s to %s",valid_lw, valid_up))
        message(sprintf("df_train_set isn't within %s to %s and %s to %s , balanced num is %s",test_lw, test_up, valid_lw, valid_up, nrow(df_train_set)))
        
        
        
        
      }else{
        df_validate_set <- f_in_csv[c(valid_lw:f_in_nums, 1:valid_backward_up),]
        df_train_set <- smote(HeartDisease ~ ., data  =f_in_csv[c(-(test_lw:test_up), -(valid_lw:f_in_nums), -(1:valid_backward_up)),])#, perc.over = 4, perc.under = 1)   
        # df_train_set <- f_in_csv[c(-(test_lw:test_up), -(valid_lw:f_in_nums), -(1:valid_backward_up)),]
        
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
                                                        
                                                        #minsplit = 5, minbucket = 5, cp = 0.002
                                                        minsplit = 5, minbucket = 5, cp = 0.001 #better
                                                        #minsplit = 5, minbucket = 5, cp = 0.0005 #better
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
      
      model_ls <- c(model_ls, model)
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

}