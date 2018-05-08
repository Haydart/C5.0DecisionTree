# load the packages
library(C50)
library(caret)

loadDataset <- function(filename) {
  dataset <<- read.csv(filename)
  class_col_index <<- which(names(dataset) == "class")
  dataset[, "class"] <<- as.factor(dataset[, "class"])
}

plotDecisionTree <- function(control_param) {
  tree_model = C5.0.default(dataset[,-class_col_index], dataset[,class_col_index], control = control_param)
  plot(tree_model)
  tree_model
}

fit_tree_model <- function(control_param, boosted=TRUE) {
  if(boosted) {
    tree_model <- C5.0(class~., data=dataset, trials=10)
  }
  tree_model = C5.0.default(dataset[,-class_col_index], dataset[,class_col_index], control = control_param)
  plot(tree_model)
  summary(tree_model)
}

loadDataset("datasets/glass.csv")
dataset
plot(dataset)

tree_control <- C5.0Control(CF = 0.0)
#tree_control <- C5.0Control(noGlobalPruning = TRUE)
#tree_control <- C5.0Control(winnow = TRUE)
#tree_control <- C5.0Control(minCases = 10)
#tree_control <- C5.0Control()

tree_model <- fit_tree_model(tree_control)


folds <- createFolds(iris$Species, k = 10, list = FALSE)
folds

model <- C5.0(Species~., data=iris[1:120,], trials=10)
# summarize the fit
print(fit)
summary(fit)
plot(fit)
# make predictions
predictions <- predict(fit, iris[121:150,])
# summarize accuracy
table(predictions, iris[121:150,]$Species)
summary(predictions)
score <- confusionMatrix(data=predictions, reference=iris[121:150,]$Species, mode="prec_recall")
score
