# load the packages
library(C50)
library(caret)

loadDataset <- function(filename) {
  dataset <<- read.csv(filename)
  class_col_index <<- which(names(dataset) == "class")
  dataset[, "class"] <<- as.factor(dataset[, "class"])
}

plotExampleTree <- function(control_param) {
  example_dec_tree = C5.0.default(dataset[,-class_col_index], dataset[,class_col_index], control = control_param)
  plot(example_dec_tree)
}

loadDataset("datasets/glass.csv")
dataset
plot(dataset)

tree_control <- C5.0Control(CF = 0.0)
#tree_control <- C5.0Control(noGlobalPruning = TRUE)
#tree_control <- C5.0Control(winnow = TRUE)
#tree_control <- C5.0Control(minCases = 10)
#tree_control <- C5.0Control()

plotExampleTree(tree_control)


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
