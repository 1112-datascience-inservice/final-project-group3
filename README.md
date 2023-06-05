# [GroupID] your projrct title
The goals of this project.

## Contributors
|組員|系級|學號|工作分配|
|-|-|-|-|
|何彥南|資科碩二|110753202|團隊中的吉祥物🦒，負責增進團隊氣氛| 
|張小銘|資科碩二|xxxxxxxxx|團隊的中流砥柱，一個人打十個|

## Quick start
You might provide an example commend or few commends to reproduce your analysis, i.e., the following R script
```R
Rscript code/xgboost_111971005-3.R --input data/heart_2020_cleaned.csv
run smote with raw data "heart_2020_cleaned.csv"

Rscript code/xgboost_111971005-3.R --input data/heart_2020_smote.csv
use already smote data "heart_2020_smote.csv"
```

## Folder organization and its related description
idea by Noble WS (2009) [A Quick Guide to Organizing Computational Biology Projects.](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1000424) PLoS Comput Biol 5(7): e1000424.

### docs
* Your presentation, 1112_DS-FP_groupID.ppt/pptx/pdf (i.e.,1112_DS-FP_group1.ppt), by **06.08**
* Any related document for the project
  * xgboost-smote-threshold-cf 
    show  results with different threshold from 0 to 1 
  * xgboost-selected-threshold-cf
    use 13 features selected to train and show results  with different threshold from 0 to 1 
  * xgboost-smote-roc.png 
    show roc curve
  * xgboost-importance-features.png & xgboost-importance-features.csv
    show importance-features
    
### data
* Input
  * Source
  * Format
  * Size 
* Output

### code
* Analysis steps
  1.use carter's method doing Data Preprocessing
  2.set training and testing 7:3
  3.use smote on imbalanced data(training data) 
  4.drop NA label
  5.train xgboost model
  6.select max f1 value at different threshold and get ROC, AUC
  7.select 13 fetures to train xgboost model
  8.select max f1 value at different threshold and get ROC, AUC

* Which method or package do you use? 
  * original packages in the paper
  * additional packages you found

### results
* What is a null model for comparison?
* How do your perform evaluation?
  * Cross-validation, or extra separated data

## References
* Packages you use
* Related publications
