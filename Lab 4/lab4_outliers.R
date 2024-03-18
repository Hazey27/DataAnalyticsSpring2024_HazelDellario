# looking for/testing outliers

library("tidyverse")
library("ISLR")

# reading in data
data(mtcars)

cars = cars[1:30, ]
head(cars1)


# creating outliers
car_outliers = data.frame(speed = c(19, 19, 20, 20, 20), 
                          dist = c(190, 186, 210, 220, 218))
head(car_outliers)

cars_altered = rbind(cars, car_outliers)


# par -- get or set graphical parameters
par(mfrow = c(1, 2))
plot(cars_altered$speed, cars_altered$dist, xlim = c(0, 28), ylim = c(0, 230), main = "With Outlier", 
     xlab = "speed", ylab = "dist", pch = "*", col = "red", cex = 2)
altered_dist_lm = lm(dist ~ speed, cars_altered)
abline(altered_dist_lm, col = "blue", lwd = 3, lty = 2)

plot(cars$speed, cars$dist, xlim = c(0, 28), ylim = c(0, 230), main = "Without Outlier", 
     xlab = "speed", ylab = "dist", pch = "*", col = "red", cex = 2)
true_dist_lm = lm(dist ~ speed, cars)
abline(true_dist_lm, col = "blue", lwd = 3, lty = 2)


library("cowplot") # has great function to put ggplots side by side

outlier_graph = ggplot(cars_altered, aes(x = speed, y = dist)) + 
  geom_point(col = "red", shape = 8) + # shape 8 is *
  geom_abline(slope = altered_dist_lm[[1]][2], intercept = altered_dist_lm[[1]][1], col = "blue", linewidth = 1) + #draws line of best fit from linear model
  ylim(c(0, 225)) + #set y limit the same in both graphs so data is not misrepresented
  theme(plot.title = element_text(hjust = 0.5)) + # center align plot title
  ggtitle("With Outliers") # plot title

true_graph = ggplot(cars, aes(x = speed, y = dist)) + 
  geom_point(col = "red", shape = 8) +
  geom_abline(slope = true_dist_lm[[1]][2], intercept = true_dist_lm[[1]][1], col = "blue", linewidth = 1) +
  ylim(c(0, 225)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  ggtitle("Without Outliers")

plot_grid(outlier_graph, true_graph)
