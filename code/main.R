source("data_preprocess.R")
library("randomForest")

library('performanceEstimation')

library("pROC")
# library('corrplot')

dataset <- get_data()


# summary(dataset)
table(dataset$HeartDisease)


# cor_data <- cor(dataset$HeartDisease, dataset[2:29])
# cor_data
# corrplot(cor_data, method = "circle")

dataset <- smote(HeartDisease~., data=dataset)

# summary(dataset)
table(dataset$HeartDisease)


sample <- sample(2, nrow(dataset), replace=TRUE, prob=c(0.8,0.2))

train_data <- dataset[sample==1,]
test_data <- dataset[sample==2,]

###########
# err <- as.numeric()
# for (i in 1:(length(names(train_data)))-1) {
# 	model<-randomForest(as.factor(HeartDisease)~. ,data=train_data, ntree=15, mtry=i)	
# 	err <- append(err, mean(model$err.rate))
# 	print(i)
# }
# print(err)
# mtry <- which.min(err)
# print(mtry)
#######

# model<-randomForest(as.factor(HeartDisease)~. ,data=train_data, importance=TRUE, ntree=15, mtry=26, keep.forest = TRUE)
model<-randomForest(as.factor(HeartDisease)~. ,data=train_data, importance=TRUE, ntree=10, mtry=3, keep.forest = TRUE)

print(model)
plot(model)

# importance(model)
# mean decrease in accuracy
varImpPlot(model, type=1, sort=TRUE)
# mean decrease in gini
varImpPlot(model, type=2, sort=TRUE)

# print(model)

predictions = predict(model, newdata = test_data, type = "class")

# summary(predictions)

accuracy <- (sum(predictions == test_data$HeartDisease) / nrow(test_data))
print(paste('accuracy : ' , accuracy))

true_positive <- sum(predictions == "1" & test_data$HeartDisease == "1")
false_positive <- sum(predictions != "1" & test_data$HeartDisease == "0")
false_negative <- sum(predictions == "1" & test_data$HeartDisease == "1")

precision <- true_positive / (true_positive + false_positive)
recall <- true_positive / (true_positive + false_negative)

print(paste('precision : ' , precision))
print(paste('recall : ' , recall))

f1 <- 2 * (precision * recall) / (precision + recall)
print(paste('f1 : ' , f1))


roc_predic <- ifelse(predictions == '1' , 1, 0)
roc_score=roc(test_data$HeartDisease, predictor=roc_predic)

print(roc_score)

plot(roc_score ,main ="ROC curve -- Random Forest ")

