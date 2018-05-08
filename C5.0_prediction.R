# load the packages
library(C50)
library(caret)

loadDataset <- function(filename) {
  dataset <<- read.csv(filename)
  class_col_index <<- which(names(dataset) == "class")
  dataset[, "class"] <<- as.factor(dataset[, "class"])
}

fitTreeModelBoosted <- function(control_param) {
  tree_model <- C5.0(class~., data=dataset, trials=10)
}

fitTreeModel <- function(control_param) {
  tree_model = C5.0.default(dataset[,-class_col_index], dataset[,class_col_index], control=control_param)
}

summarizeTree <- function(tree_model) {
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

splitData <- function(data, folds_count, stratified, shuffled) {
  if (shuffled) {
    data = data[sample(nrow(data)),]
  }
  if (stratified) {
    foldIndices = createFolds(data$class, k=folds_count)
    folds = list()
    for (i in 1:length(foldIndices)) {
      temp_array = array(as.numeric(unlist(foldIndices[i])))
      folds[[i]] = data[temp_array,]
    }
    folds
  } else {
    folds = split(data, rep(1:folds_count))
  }
}

data_folds = splitData(dataset, folds_n=5, stratified=TRUE, shuffled=TRUE)

tree_model <- fitTreeModel(tree_control)
boosted_tree_model <- fitTreeModelBoosted()


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
