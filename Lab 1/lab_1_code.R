#lab 1 for data analytics

# analyzing EPI (Environmental Performance Index) data from NASA


# Importing
library("caret")
library("janitor")
library("MASS")
library("tidyverse")
library("readxl")


# Setting path
setwd("C:\\Users\\Hazel Dellario\\Documents\\GitHub\\DataAnalyticsSpring2024_HazelDellario\\Lab 1")

EPI_data <- read.csv("2010EPI_data.csv", header = TRUE, skip = 1, na.strings = "..")

#fix(EPI_data) #brings up GUI of data set


#2010 EPI data --------------------------------------------------------------------------------------------------------------------------

# Set columns to correct data types
EPI_data$code <- as.integer(EPI_data$code)
EPI_data$GDPCAP07 <- as.double(EPI_data$GDPCAP07)
EPI_data$Population07 <- as.double(EPI_data$Population07)

EPI_data <- na.omit(EPI_data)

summary(EPI_data, na.rm = TRUE)

boxplot(EPI_data$EPI)
stem(EPI_data$EPI)
hist(EPI_data$EPI)
hist(EPI_data$EPI, breaks = seq(from = 30., to = 95., by = 1.0), prob=TRUE, main = "Histogram of EPI", xlab = "Frequency", ylab = "Density")

lines(density(EPI_data$EPI, na.rm = TRUE, bw = "SJ"))

rug(EPI_data$EPI) #shows x values over x axis


# Determine distribution of data
plot(ecdf(EPI_data$EPI), do.points = FALSE, verticals = TRUE) 

par(pty="s")
qqnorm(EPI_data$EPI)
qqline(EPI_data$EPI) #qq plot with line of best fit; linear regression

# qqplot from generating distribution
x<-seq(30,95,1)
qqplot(qt(ppoints(250), df= 5), x, xlab= "Q-Q plot for t dsn")
qqline(x)


# Exercise 1 ----------------------------------------------------------------------

plot_stats <- function(data, step = 1) {
  y_min = floor(min(data))
  y_max = ceiling(max(data) + step)
  
  hist(data, breaks = seq(y_min, y_max, step), prob=TRUE, #main = paste("Histogram of", data), #creates a title for every data point :_(
       xlab = "Frequency", ylab = "Density")
  
  lines(density(data, na.rm = TRUE, bw = "SJ")) # Adds contour to histogram (try bw=“SJ”)
  
  rug(data) #shows x values over x axis
  
  
  # Determine distribution of data
  plot(ecdf(data), do.points = FALSE, verticals = TRUE) 
  
  par(pty="s")
  qqnorm(data)
  qqline(data) #qq plot with line of best fit; linear regression
  
  # qqplot from generating distribution
  x<-seq(sequence)
  qqplot(qt(ppoints(250), df= 5), x, xlab= "Q-Q plot for t dsn")
  qqline(x)
}

plot_stats(EPI_data$PopulationDensity07, 30)
plot_stats(EPI_data$GDPCAP07, 1000)
plot_stats(EPI_data$DALY, 10)
plot_stats(EPI_data$WATER_H, 10)



boxplot(EPI_data$EPI, EPI_data$DALY)

boxplot(EPI_data$EPI, EPI_data$ENVHEALTH)
boxplot(EPI_data$EPI, EPI_data$ECOSYSTEM)
boxplot(EPI_data$EPI, EPI_data$AIR_H)
boxplot(EPI_data$EPI, EPI_data$WATER_H)
boxplot(EPI_data$EPI, EPI_data$AIR_E) # surprisingly different 
boxplot(EPI_data$EPI, EPI_data$WATER_E) # also quite different
boxplot(EPI_data$EPI, EPI_data$BIODIVERSITY) # different


# Exercise 2: filtering (populations) ----------------------------------------------------------------

filter_GEO <- function(subregion) {
  #return(EPI_data[ , EPI_data$GEO_subregion == subregion]) #gives the wrong dimension matrix; excludes a ton of variables and adds rows?
  newvar <- EPI_data %>% 
    filter(GEO_subregion == subregion)
  return(newvar)
}

EPI_South_Asia <- filter_GEO("South Asia")
EPI_Caribbean <- filter_GEO("Caribbean")
EPI_NA <- filter_GEO("North America")


boxplot(EPI_South_Asia$EPI, EPI_Caribbean$EPI, EPI_NA$EPI,  ylim = c(0, 100), xlab = c("Country"), ylab = "EPI") # Caribbean has pretty good EPI, South Asia has poor EPI (median 60)


boxplot(EPI_South_Asia$PopulationDensity07, EPI_Caribbean$PopulationDensity07, EPI_NA$PopulationDensity07) # NA has lowest pop dens by far
# South Asia has highest pop dens and lowest EPI
# Caribbean has second highest pop dens but highest EPI
# North America sucks

EPI_data$LandLock <- as.logical(EPI_data$Landlock)
EPILand<-EPI_data[EPI_data$Landlock != 1]
Eland <-EPILand[!is.na(EPILand)] %>% 
  as.double() %>% 
  na.omit()
summary(Eland) #a few incredibly extreme outliers (mean and median differ by 2.0e+06)
plot(Eland) #there is just 1 value at 2.6e+09 and just two at ~1.0e+09
#lets cut off at 5e+08 for now; removes only 3 values
Eland_no_outliers <- Eland[-which(Eland > 5.0e+08)]

hist(Eland_no_outliers) #still a ton of outliers, but I don't think we should remove more than a few points. Otherwise we're cherry picking data
hist(Eland_no_outliers, seq(-10, 5.0e+08, 2e+07), prob=TRUE) #this one still looks awful

length(which(Eland > 1e+08)) #only 9 values above 1.0e+08. Will exclude those
Eland_1e8 <- Eland[-which(Eland > 1e+08)]
hist(Eland_1e8) #makes a better histogram
hist(Eland_1e8, seq(-10, 1.0e+08, 2e+06), prob=TRUE) #this still looks terrible but that just might be how the data looks

hist(Eland_1e8, prob = T) # automatic binning in R does a better job creating a picture of the data
lines(density(Eland_1e8, bw = "SJ")) #this looks awful; I can't get it to look better. I think the distribution of the data is too off for this to work
rug(Eland_1e8)


range(EPI_data$No_surface_water) # Only 0 so graphing would not be interesting



# 2016 EPI data -----------------------------------------------------------------------------------------


EPI_2016 <- read_xlsx("2016-epi.xlsx", sheet = 3) %>%  #many different sheets, choosing 5 bc it has EPI data
  clean_names()


plot_stats(EPI_2016$x2016_epi_score, 5)

ggplot(EPI_2016[1:180, ], aes(x = country, y = x2016_epi_score)) +
  geom_col() +
  ylim(c(0, 100)) +
  labs(title = "EPI Scores in 2016") +
  xlab("Country") +
  ylab("EPI Score in 2016")


# Countries with Highest vs Lowest EPI Score -------------------------------------------------------------

# Highest EPI Score: Finland
# Lowest EPI Score: Somalia

ggplot(EPI_2016[c(1, 180), ], aes(x = country, y = x2016_epi_score)) +
  geom_col() +
  ylim(c(0, 100)) +
  labs(title = "EPI Scores in 2016") +
  xlab("Country") +
  ylab("EPI Score in 2016")






