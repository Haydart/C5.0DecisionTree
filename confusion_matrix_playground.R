#sample caret.confusionMatrix usage
confusionMatrix(iris$Species, sample(iris$Species))
newPrior <- c(.05, .8, .15)
names(newPrior) <- levels(iris$Species)
confusionMatrix(iris$Species, sample(iris$Species), mode="prec_recall")