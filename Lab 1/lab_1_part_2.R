# Lab 1 part 2
# focus on data visualization

library("tidyverse")

plot(mtcars$wt, mtcars$mpg)
qplot(mtcars$wt, mtcars$mpg)
qplot(wt, mpg, data = mtcars)
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()
plot(pressure$temperature, pressure$pressure, type = "l")
points(pressure$temperature, pressure$pressure)

lines(pressure$temperature, pressure$pressure/2, col = "red")
points(pressure$temperature, pressure$pressure/2, col = "blue")

qplot(pressure$temperature, pressure$pressure, geom = 'line')
qplot(temperature, pressure, data = pressure, geom = "line")
ggplot(pressure, aes(x = temperature, y = pressure)) +
  geom_line() + 
  geom_point()

barplot(BOD$demand, names.arg = BOD$Time)
table(mtcars$cyl)
barplot(table(mtcars$cyl))
qplot(mtcars$cyl)
qplot(factor(mtcars$cyl))
ggplot(mtcars, aes(x = factor(cyl))) +
  geom_bar()

ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(binwidth = 4)

ggplot(ToothGrowth, aes(x = supp, y = len)) + 
  geom_boxplot()
ggplot(ToothGrowth, aes(x = interaction(supp, dose), y = len)) + 
  geom_boxplot()
