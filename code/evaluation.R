if (require(pROC)) {
  message("library xgboost load success.")
} else {
  stop("library xgboost not exist, please install.")
}

evaluation <- function(test_result, predictions) {
    confusion_matrix <- table(test_result, predictions)
	# cf <- confusionMatrix(factor(binary_predictions_selected), factor(test_labels), positive='1')

    print(confusion_matrix)
#     accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
# precision <- confusion_matrix[2, 2] / sum(confusion_matrix[, 2])
# recall <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
# TN <- sum(valid_df$HeartDisease == 0 & val_pred == 0)
# FP <- sum(valid_df$HeartDisease == 0 & val_pred == 1)
# specificity <- TN / (TN + FP)
# f1_score <- 2 * precision * recall / (precision + recall)

# accuracy <- round(accuracy, 2)
# precision <- round(precision, 2)
# recall <- round(recall, 2)
# specificity <- round(specificity, 2)
# f1_score <- round(f1_score, 2)


# message('Confusion Matrix:')
# print(confusion_matrix)
# message('Accuracy:', accuracy)
# message('Precision:', precision)
# message('sensitivity(Recall):', recall)
# message('Specificity:', specificity)
# message('F1 Score:', f1_score)

# #AUC
# roc_obj <- roc(valid_df$HeartDisease, as.numeric(val_pred))
# auc <- auc(roc_obj)
# auc <- round(auc, 2)
# message('AUC:', auc)



# #ROC
# pdf("../results/roc_curve_after_smote.pdf")
# plot(roc_obj, main = "ROC Curve")
# abline(0, 1, lty = 2)  # 繪製對角線
# legend("bottomright", legend = paste("AUC =", auc), bty = "n")  # 在圖例中顯示 AUC
# dev.off()


	accuracy_ls <- c()
	precision_ls <- c()
	sensitivity_ls <- c()
	specificity_ls <- c()
	F1score_ls <- c()
	auc_ls <- c()

	# selected_roc = roc(factor(test_result) ~ as.numeric(predictions), plot = FALSE,
 #               print.auc = TRUE,legacy.axes=TRUE)
 	selected_roc <- roc(test_result, as.numeric(predictions))
	auc <- auc(selected_roc)

    cf <- confusionMatrix(factor(predictions), factor(test_result), positive='1')
	accuracy_ls <- c(accuracy_ls, cf[[3]][1])
	sensitivity_ls <- c(sensitivity_ls, cf[[4]][1])
	specificity_ls <- c(specificity_ls, cf[[4]][2])
	precision_ls <- c(precision_ls, cf[[4]][5])
	F1score_ls <- c(F1score_ls, cf[[4]][7])
	auc_ls <- c(auc_ls, auc)
	method <- c("29Features", "13Features")
	out_data <- data.frame(
  		Method = method,
  		Accuracy = accuracy_ls,
  		Precision = precision_ls,
  		Sensitivity = sensitivity_ls,
  		specificity_ls = specificity_ls,
  		'F1 Score' = F1score_ls,
  		AUC = auc_ls,
  		stringsAsFactors = FALSE
	)
	print(out_data)
	# write.csv(out_data, 'results/xgboost-results.csv', row.names = FALSE, quote = FALSE)
}