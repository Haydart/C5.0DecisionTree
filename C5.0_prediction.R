source("C5.0_utils.R")
library(C50)
library(caret)

loadDataset("datasets/glass.csv")
dataset
plot(dataset)

tree_control <- C5.0Control(CF = 0.0)
#tree_control <- C5.0Control(noGlobalPruning = TRUE)
#tree_control <- C5.0Control(winnow = TRUE)
#tree_control <- C5.0Control(minCases = 10)
#tree_control <- C5.0Control()

data_folds = splitData(dataset, folds_n=5, stratified=TRUE, shuffled=TRUE)

tree_model <- fitTreeModel(tree_control)
boosted_tree_model <- fitTreeModelBoosted()

# make predictions
predictions <- predict(tree_model, dataset)
# summarize accuracy
table(predictions, iris[121:150,]$Species)
summary(predictions)
score <- confusionMatrix(data=predictions, reference=iris[121:150,]$Species, mode="prec_recall")
