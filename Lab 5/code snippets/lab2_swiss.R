data(swiss)
sclass <- kmeans(swiss[2:5], 3) 
table(sclass$cluster, swiss[,2])    
# 
library(e1071)
# this is wrong and I don't know why; model predicts NA for every entry
m <- naiveBayes(Agriculture ~ ., data = swiss)
nb_pred = predict(m, swiss[ , -2])
table(nb_pred[["class"]], swiss[ , 2])

