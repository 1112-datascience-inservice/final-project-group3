if (require(dplyr)) {
  print("library dplyr load success.")
} else {
  print("library dplyr not exist, start to install.")
  install.packages('dplyr', repos = "http://cran.us.r-project.org")
  if (require(dplyr)) {
    print('library dplyr load success.')
  } else {
    stop("library dplyr load failed.")
  }
}

if (require(fastDummies)) {
  print("library fastDummies load success.")
} else {
  print("library fastDummies not exist, start to install.")
  install.packages('fastDummies', repos = "http://cran.us.r-project.org")
  if (require(fastDummies)) {
    print('library fastDummies load success.')
  } else {
    stop("library fastDummies load failed.")
  }
}

if (require(caret)) {
  print("library caret load success.")
} else {
  print("library caret not exist, start to install.")
  install.packages('caret', repos = "http://cran.us.r-project.org")
  if (require(caret)) {
    print('library caret load success.')
  } else {
    stop("library caret load failed.")
  }
}

if (require(performanceEstimation)) {
  print("library performanceEstimation load success.")
} else {
  print("library performanceEstimation not exist, start to install.")
  install.packages('performanceEstimation', repos = "http://cran.us.r-project.org")
  if (require(performanceEstimation)) {
    print('library performanceEstimation load success.')
  } else {
    stop("library performanceEstimation load failed.")
  }
}

if (require(rpart)) {
  print("library rpart load success.")
} else {
  print("library rpart not exist, start to install.")
  install.packages('rpart', repos = "http://cran.us.r-project.org")
  if (require(rpart)) {
    print('library rpart load success.')
  } else {
    stop("library rpart load failed.")
  }
}
if (require(ROCR)) {
  print("library ROCR load success.")
} else {
  print("library ROCR not exist, start to install.")
  install.packages('ROCR', repos = "http://cran.us.r-project.org")
  if (require(ROCR)) {
    print('library ROCR load success.')
  } else {
    stop("library ROCR load failed.")
  }
}
if (require(rpart.plot)) {
  print("library rpart.plot load success.")
} else {
  print("library rpart.plot not exist, start to install.")
  install.packages('rpart.plot', repos = "http://cran.us.r-project.org")
  if (require(rpart.plot)) {
    print('library rpart.plot load success.')
  } else {
    stop("library rpart.plot load failed.")
  }
}
if (require(corrplot)) {
  print("library corrplot load success.")
} else {
  print("library corrplot not exist, start to install.")
  install.packages('corrplot', repos = "http://cran.us.r-project.org")
  if (require(corrplot)) {
    print('library corrplot load success.')
  } else {
    stop("library corrplot load failed.")
  }
}

if (require(randomForest)) {
  print("library randomForest load success.")
} else {
  print("library randomForest not exist, start to install.")
  install.packages('randomForest', repos = "http://cran.us.r-project.org")
  if (require(randomForest)) {
    print('library randomForest load success.')
  } else {
    stop("library randomForest load failed.")
  }
}

if (require(pROC)) {
  print("library pROC load success.")
} else {
  print("library pROC not exist, start to install.")
  install.packages('pROC', repos = "http://cran.us.r-project.org")
  if (require(pROC)) {
    print('library pROC load success.')
  } else {
    stop("library pROC load failed.")
  }
}

if (require(xgboost)) {
  print("library xgboost load success.")
} else {
  print("library xgboost not exist, start to install.")
  install.packages('xgboost', repos = "http://cran.us.r-project.org")
  if (require(xgboost)) {
    print('library xgboost load success.')
  } else {
    stop("library xgboost load failed.")
  }
}

if (require(Ckmeans.1d.dp)) {
  print("library Ckmeans.1d.dp load success.")
} else {
  print("library Ckmeans.1d.dp not exist, start to install.")
  install.packages('Ckmeans.1d.dp', repos = "http://cran.us.r-project.org")
  if (require(Ckmeans.1d.dp)) {
    print('library Ckmeans.1d.dp load success.')
  } else {
    stop("library Ckmeans.1d.dp load failed.")
  }
}