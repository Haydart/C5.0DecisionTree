loadDataset <- function(filename) {
  dataset <<- read.csv(filename)
  class_col_index <<- which(names(dataset) == "class")
  dataset[, "class"] <<- as.factor(dataset[, "class"])
}

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

fitTreeModelBoosted <- function(control_param) {
  tree_model <- C5.0(class~., data=dataset, trials=10)
}

fitTreeModel <- function(control_param) {
  tree_model = C5.0.default(dataset[,-class_col_index], dataset[,class_col_index], control=control_param)
}

summarizeTree <- function(tree_model) {
  print(tree_model)
  plot(tree_model)
  summary(tree_model)
}

calculateMeasures <- function(mode) {
  confusionMatrix(data=predictions[,-class_col_index], reference=dataset$class, mode=mode)
}