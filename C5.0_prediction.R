source("C5.0_utils.R")
library(C50)
library(caret)

loadDataset("datasets/glass.csv")
dataset
data_folds <- splitData(dataset, folds_count=5, stratified=TRUE, shuffled=TRUE)
data_folds[1]
plot(dataset)

tree_control <- C5.0Control(CF = 0.0)
#tree_control <- C5.0Control(noGlobalPruning = TRUE)
#tree_control <- C5.0Control(winnow = TRUE)
#tree_control <- C5.0Control(minCases = 10)
#tree_control <- C5.0Control()

tree_model <- fitTreeModel(tree_control)
boosted_tree_model <- fitTreeModelBoosted()

# make predictions
predictions <- predict(tree_model, dataset)
# summarize accuracy
table(predictions, iris[121:150,]$Species)
summary(predictions)
model_score <- calculateMeasures("prec_recall")
