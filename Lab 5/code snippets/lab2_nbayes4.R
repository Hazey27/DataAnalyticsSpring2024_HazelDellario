# Josh Walters
# install.packages('ElemStatLearn') # not available on CRAN, have to download manually
library('ElemStatLearn')
library("klaR") # different from e1071 naivebayes - try it too!
library("caret")
data(spam, package="ElemStatLearn")

sub = sample(nrow(spam), floor(nrow(spam) * 0.9))
train = spam[sub,]
test = spam[-sub,]

xTrain = train[,-58]
yTrain = train$spam

xTest = test[,-58]
yTest = test$spam

model = train(xTrain,yTrain,'nb',trControl=trainControl(method='cv',number=10)) # nb = naive Bayes

prop.table(table(predict(model$finalModel,xTest)$class,yTest))

# Alternate way to set up a training sample --------------------------------------------------------------
train.ind <- sample(1:nrow(spam), ceiling(nrow(spam)*2/3), replace=FALSE)

# apply NB classifier 
# Error in NaiveBayes.default(X, Y, ...) : 
#   Zero variances for at least one class in variables: A.41

xTrain.copy = xTrain
col_names = colnames(xTrain)

for(c in col_names) {
  col_sum = sum(xTrain[, c])
  if(col_sum == 0) {
    xTrain.copy = subset(xTrain[ , -c])
  }
}

nb.res <- NaiveBayes(spam ~ ., data=spam[train.ind,])



# predict on holdout units
nb.pred <- predict(nb.res, spam[-train.ind,])

# but this also works on the training sample, i.e. without using a `newdata`
head(predict(nb.res))