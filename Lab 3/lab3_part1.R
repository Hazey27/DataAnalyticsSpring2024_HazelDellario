# Data analysis lab 3

# Hazel Dellario
# 2024/02/13

# Exercise 1

library("tidyverse")

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