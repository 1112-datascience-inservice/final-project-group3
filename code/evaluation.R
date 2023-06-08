if (require(pROC)) {
  message("library xgboost load success.")
} else {
  stop("library xgboost not exist, please install.")
}

evaluation <- function(test_result, predictions) {
	accuracy_ls <- c()
	precision_ls <- c()
	sensitivity_ls <- c()
	specificity_ls <- c()
	F1score_ls <- c()
	auc_ls <- c()

 	selected_roc <- roc(test_result, as.numeric(predictions))
	auc <- auc(selected_roc)

	#ROC
	pdf("../results/roc_curve.pdf")
	plot(selected_roc, main = "ROC Curve")
	abline(0, 1, lty = 2)  # 繪製對角線
	legend("bottomright", legend = paste("AUC =", auc), bty = "n")  # 在圖例中顯示 AUC
	dev.off()

	#result
    cf <- confusionMatrix(factor(predictions), factor(test_result), positive='1')
	accuracy_ls <- c(accuracy_ls, cf[[3]][1])
	sensitivity_ls <- c(sensitivity_ls, cf[[4]][1])
	specificity_ls <- c(specificity_ls, cf[[4]][2])
	precision_ls <- c(precision_ls, cf[[4]][5])
	F1score_ls <- c(F1score_ls, cf[[4]][7])
	auc_ls <- c(auc_ls, auc)
	
	out_data <- data.frame(
  		'Accuracy' = accuracy_ls,
  		'Precision' = precision_ls,
  		'Sensitivity' = sensitivity_ls,
  		'Specificity' = specificity_ls,
  		'F1 Score' = F1score_ls,
  		'AUC' = auc_ls
	)
	write.csv(out_data, '../results/results.csv', row.names = FALSE, quote = FALSE)

}