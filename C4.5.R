# load the packages
library(C50)
library(caret)

#folds <- crossv_kfold(iris, k=25)
#index <- 10
#folds$test$'1'$idx

loadDataset <- function(filename) {
  data <<- read.csv(filename)
  which(names(data))
  data
}

loadDataset <- function(filename) {
  read.csv(file = filename)
}

loadDataset("datasets/iris.csv")

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
