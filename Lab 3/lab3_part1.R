# Data analysis lab 3

# Hazel Dellario
# 2024/02/13

# Exercise 1

library("tidyverse")
library("kknn")
library("bio3d")
library("fdm2id")
library("flexclust")


# Exercise 1 -------------------------------------------------------

set.seed(12345)
help(par)


par(mar = rep(0.2, 4))

data_Matrix = matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(data_Matrix[1:nrow(data_Matrix), ]))

heatmap(data_Matrix)



set.seed(678910) 
for(i in 1:40){  
  # flipping a coin and getting the data  
  coin_Flip <- rbinom(1, size = 1, prob = 0.5)  
  # if the coin is "Heads", add a common pattern to that row,  
  if(coin_Flip) {    
    data_Matrix[i, ] <- data_Matrix[i, ] + rep(c(0,3), each =5)
  } 
}

par(mar = rep(0.2, 4))

image(1:10, 1:40, t(data_Matrix[1:nrow(data_Matrix), ]))
heatmap(data_Matrix)

par(mar = rep(0.2, 4))
heatmap(data_Matrix)


hh <- hclust(dist(data_Matrix))
data_Matrix_Ordered = data_Matrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(data_Matrix_Ordered)[ , 1:nrow(data_Matrix_Ordered)])
plot(rowMeans(data_Matrix_Ordered), 1:40, xlab = "The Row Mean", ylab = "Row", pch = 19)
plot(colMeans(data_Matrix_Ordered), 1:10, xlab = "The Column Mean", ylab = "Column", pch = 19)

# Exercise 2 ---------------------------------------------------

setwd("C:\\Users\\hazeldellario\\Documents\\GitHub\\DataAnalyticsSpring2024_HazelDellario\\Lab 3")

abalone_data = read.csv("abalone.csv")

# No values of 0 or lower
# sum(which(abalone_data[1:4177, 2:9] < 0)) ==> 0

# No NAs
# sum(is.na(abalone_data)) ==> 0

# Enough values to do 50% train, 20% validate, and 30% test split

train_val_indices <- sample(1:length(abalone_data$Rings) - 1, size = round(length(abalone_data$Rings) * 0.7), replace = FALSE)

abalone_train <- abalone_data[train_val_indices, ]
abalone_test <- abalone_data[-train_val_indices, ] # ~30% of data

val_indices = sample(1:length(abalone_train$Rings) - 1, size = round(length(abalone_train$Rings) * 0.3), replace = FALSE)

abalone_train = abalone_train[-val_indices, ] # ~50% of data
abalone_val = abalone_data[val_indices, ] # ~20% of data


#length and diameter seem to be good predictors
ggplot(abalone_train, aes(x = Length, y = Rings, color = Sex))+
  geom_point(aes(size = Diameter))

#whole weight, shell weight, and shucked weight seem to be good predictors
ggplot(abalone_train, aes(x = Whole.weight, y = Rings, color = Shell.weight))+
  geom_point(aes(size = Shucked.weight))

#height looks like good predictor
ggplot(abalone_train, aes(x = Height, y = Rings, color = Viscera.weight))+
  geom_point(aes(size = Diameter))


cont_knn = train.kknn(formula = Rings ~ ., abalone_train, kmax = 5) # cont = continuous; rings treated as continuous variable

# predict on training data
abalone_train$p_rings = predict(cont_knn, newdata = abalone_train[1:8])
abalone_val$p_rings = predict(cont_knn, newdata = abalone_val[1:8])


ggplot(abalone_val, aes(x = abalone_val$Rings, y = abalone_val$p_rings)) +
  geom_point()

train_rmsd = rmsd(abalone_train$Rings, abalone_train$p_rings) # RMSD is 2.248
val_rmsd = rmsd(abalone_val$Rings, abalone_val$p_rings) # RMSD is 3.536

rmsd_diff = val_rmsd - train_rmsd # Difference of 1.288; I think that's pretty good

summary(cont_knn)

# Age/Rings is continuous... unless we're treating it as if it's ordinal? oh I guess it is ordinal....
# TREATING RINGS AS ORDINAL
abalone_train$Rings_ord = factor(abalone_train$Rings, ordered = TRUE)

ord_knn = train.kknn(formula = Rings_ord ~ ., abalone_train[ , c(1:8, 11)], kmax = 5)

abalone_train$ord_rings = predict(ord_knn, newdata = abalone_train[1:8])
abalone_val$ord_rings = predict(ord_knn, newdata = abalone_val[1:8])



abalone_val$correct[which(abalone_val$Rings == abalone_val$ord_rings)] = TRUE
abalone_val$correct[-which(abalone_val$Rings == abalone_val$ord_rings)] = FALSE

ggplot(abalone_val, aes(x = Rings, y = ord_rings, color = correct)) +
  geom_jitter(width = 0.5, height = 0.5)

ggplot(abalone_val, aes(x = correct, y = Rings)) +
  geom_boxplot()

percent_correct = sum(abalone_val$correct) / length(abalone_val$correct) # 0.44%


# Exercise 3 -------------------------------------------

data(iris)
iris_data = iris[ , 1:4] # excludes 5th column (Species)

kmeans_fits = list()
for(i in 1:15) {
  kmeans_fits[[i]] = kmeans(iris_data, centers = i, iter.max = 1000)
  #iris_data[ , 4 + i] = predict(kmeans_fits[[i]], newdata = iris_data[ , 1:4]) # for some reason, I always get an error
  # when trying to run this line in the same for loop but it's fine if it's run in a separate for loop
}

for(i in 1:15) {
  iris_data[ , 4 + i] = predict(kmeans_fits[[i]], newdata = iris_data[ , 1:4])
}

# 3 clusters
ggplot(iris_data, aes(x = Sepal.Length, y = Sepal.Width, color = factor(V7))) +
  geom_point() +
  labs(color = "Cluster", title = "3 Cluster Kmeans compared to Sepal measurements")

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
  geom_point() +
  labs(color = "Species", title = "Species vs to Sepal measurements")

# actual species clusters
ggplot(iris_data, aes(x = Petal.Length, y = Petal.Width, color = factor(V7))) +
  geom_point() +
  labs(color = "Cluster", title = "3 Cluster Kmeans compared to Petal measurements")

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  labs(color = "Species", title = "Species vs to Petal measurements") # look very similar to 3 cluster kmeans


iris$pred_k3 = iris_data$V7 # adding kmeans k=3 predictions to data with Species info


