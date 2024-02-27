library(gdata) 
library("tidyverse")
library("readxl")
library("xlsx")

# #faster xls reader but requires perl!
# bronx1<-read_excel(file.choose(),pattern="BOROUGH",stringsAsFactors=FALSE,sheet=1,perl="<SOMEWHERE>/perl/bin/perl.exe") 
# bronx1<-bronx1[which(bronx1$GROSS.SQUARE.FEET!="0" & bronx1$LAND.SQUARE.FEET!="0" & bronx1$SALE.PRICE!="$0"),]

#alternate
bronx1<-read.xlsx("C:\\Users\\hazeldellario\\Documents\\GitHub\\DataAnalyticsSpring2024_HazelDellario\\Lab 3\\code snippets lab 1\\rollingsales_bronx.xls",pattern="BOROUGH",stringsAsFactors=FALSE,sheetIndex=1,startRow=5,header=TRUE)
# View(bronx1)
#

# Part 1 -------------------------------------------------------------------------------------------------

attach(bronx1) # If you choose to attach, leave out the "data=." in lm regression
SALE.PRICE<-sub("\\$","",SALE.PRICE) 
SALE.PRICE<-as.numeric(gsub(",","", SALE.PRICE)) 
GROSS.SQUARE.FEET<-as.numeric(gsub(",","", GROSS.SQUARE.FEET)) 
LAND.SQUARE.FEET<-as.numeric(gsub(",","", LAND.SQUARE.FEET)) 
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 

# must remove 0s for a log transform to work
bronx1$SALE.PRICE[which(bronx1$SALE.PRICE == 0)] = 0.1
bronx1$GROSS.SQUARE.FEET[which(bronx1$GROSS.SQUARE.FEET == 0)] = 0.1

m1<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET))

summary(m1)
abline(m1,col="red",lwd=2) # it doesn't look right, but it honestly could be with how many data points are on the bottom

plot(resid(m1))

# log on all
plot(log(GROSS.SQUARE.FEET), log(SALE.PRICE)) 
m1.2<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET)) 
summary(m1.2)
abline(m1.2,col="red",lwd=2)

plot(resid(m1.2))
 # ok I'm very confused

# Model 2

bronx1$LAND.SQUARE.FEET[which(bronx1$LAND.SQUARE.FEET == 0)] = 0.1


m2<-lm(log(bronx1$SALE.PRICE)~log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET))#+factor(bronx1$NEIGHBORHOOD))

# check_log = function(var) {
#   NAs = sum(is.na(var))
#   NaNs = sum(is.nan(var))
#   Infs = length(which(var == Inf))
#   problems = NAs + NaNs + Infs
#   return(problems)
# }
# 
# check_log(bronx1$SALE.PRICE)
# check_log(bronx1$GROSS.SQUARE.FEET)
# check_log(bronx1$LAND.SQUARE.FEET)

summary(m2)
plot(resid(m2))
# Suppress intercept - using "0+ ..."
m2a<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD))
summary(m2a)
plot(resid(m2a))

# Model 3
m3<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)+factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m3)
plot(resid(m3))

# Model 4
m4<-lm(log(bronx1$SALE.PRICE)~0+log(bronx1$GROSS.SQUARE.FEET)+log(bronx1$LAND.SQUARE.FEET)+factor(bronx1$NEIGHBORHOOD)*factor(bronx1$BUILDING.CLASS.CATEGORY))
summary(m4)
plot(resid(m4))
#


