require(mlbench)
data(HouseVotes84)
library(klaR)
model <- naiveBayes(Class ~ ., data = HouseVotes84)

predict(model, HouseVotes84[1:10,-1])

pred <- predict(model, HouseVotes84[,-1])
table(pred, HouseVotes84$Class)
