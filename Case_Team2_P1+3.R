#set wd for franci
setwd("~/Desktop/Time Series Econometrics/Case_Team2/Historical-vintages-of-FRED-MD-2015-01-to-2024-12")

#----Load data and packages
library(dplyr)
library(ggplot2)
library(vars)
library(bootUR)

set.seed(42)

data_full <- read.csv("2020-01.csv")
head(data_full)
View(data_full)

data <- data_full[-1, ] #first row is skipped, so it's easier to work with the data
View(data)



#----get the data/ variables we want
indpro <- data$INDPRO   #industrial production, id6 
trans_indpro <- diff(log(indpro)) #transformed indpro
plot(indpro, main = "Ind prod Index", type = "l", xlab= "Months since Jan 1959", ylab= "index")
plot(trans_indpro, main = "trans Ind prod Index", type= "l", xlab= "Months since Jan 1959", ylab= "index")
#plot transformed according to the transformation code of the appendix

fedfunds <- data$FEDFUNDS  #effective federal funds rate, id84
trans_fedfunds <- diff(fedfunds)  #transformed fed rate
plot(fedfunds, main = "fed fund rates", type= "l", xlab= "Months since Jan 1959", ylab= "index")
plot(trans_fedfunds, main = " trans fed fund rates", type ="l", xlab= "Months since Jan 1959", ylab= "index")

boxplot(fedfunds, main ="Boxplot fed funds rate")
boxplot(trans_fedfunds, main ="trans Boxplot fed funds rate")

cpi <- data$CPIAUCSL  #id113
plot(cpi, main = "consumer price index", type= "l", xlab= "Months since Jan 1959", ylab= "index")
trans_cpi <- diff(diff(log(cpi))) #transformed according to appendix
trans_cpi_1 <- diff(log(cpi)) #transformed with only one diff
plot(trans_cpi_1, main = "transformed with only one diff, cpi",
     xlab= "Months since Jan 1959", ylab= "index", type= "l")
plot(trans_cpi, main = "transformed cpi",
     xlab= "Months since Jan 1959", ylab= "index", type= "l")

#see length of vectors
l_indpro <- length(trans_indpro)
l_indpro 

l_fedfunds <- length(trans_fedfunds)
l_fedfunds

l_cpi <- length(trans_cpi)
l_cpi

#get the min length of all vectors
min_length <- min(l_cpi, l_fedfunds, l_indpro)

trans_fedfunds <- trans_fedfunds[1:min_length]
trans_indpro <- trans_indpro[1:min_length]

#check new length
l_indpro <- length(trans_indpro)
l_indpro 

l_fedfunds <- length(trans_fedfunds)
l_fedfunds

l_cpi <- length(trans_cpi)
l_cpi

#put all data together
var_data <- cbind(trans_indpro, trans_fedfunds, trans_cpi)
View(var_data)
lag_selection <- VARselect(var_data, lag.max= 60, type ="const")  #check for info criteria
print(lag_selection$selection)  #VAR(3) or VAR(4) selected


#display VAR(3)
var_model <- VAR(var_data, p= 3, type = "const")
var_model
summary(var_model)

plot(var_model)

#new ordering of data
#var_data_new has the order: indpro, cpi, fed funds
var_data_new <- var_data[, c(1,3,2)]
lag_selection_new <- VARselect(var_data_new, lag.max= 60, type ="const")  #check for info criteria
print(lag_selection$selection)  #VAR(3) or VAR(4) selected
var_data_new_model <- VAR(var_data_new, p=3, type="const")

summary(var_data_new_model)
plot(var_data_new_model)

#impulsive response functions for new order
irf_var_new <- irf(var_data_new_model, n.ahead = 12, boot= TRUE, ortho = "False")
plot(irf_var_new)

#----Unit root test

##ur test for indpro
indpro_log <- log(indpro)
plot(indpro_log, type="l", main= "ind prod log")

indpro_d2 <- diff(diff(indpro_log))
plot(indpro_d2, type="l", main="ind prod diff^2")
abline(h=0, col= "blue")

indpro_d1 <- diff(indpro_log)
plot(indpro_d1, type="l", main="ind prod diff^1")
abline(h=0, col= "blue")
abline(h=0.01, col="red")

adf_indpro_2 <- adf(diff(indpro_log, differences=2), deterministics= "intercept")
print(adf_indpro_2)
#intercept, because no clear trend is visible in the plot.
#Reject H0 hypothesis, time series is stationary

adf_indpro_1 <- adf(diff(indpro_log, differences=1), deterministics= "intercept")
print(adf_indpro_1)
#intercept, because no clear trend is visible in the plot.
#Reject H0 hypothesis, time series is stationary

adf_indpro_0 <- adf(indpro_log, deterministics= "trend")
print(adf_indpro_0)
#trend, because a clear trend is visible in the plot.
#Fail to reject H0 hypothesis, time series has an unit root

#needed? otherwise delete
adf_all <- boot_ur(var_data, deterministics= "const")
print(adf_all)


#fedfunds unit root test
adf_fedfunds_2 <- adf(diff(fedfunds, differences=2), deterministics= "intercept")
print(adf_fedfunds_2)

adf_fedfunds_1 <- adf(diff(fedfunds, differences=1), deterministics= "intercept")
print(adf_fedfunds_1)

adf_fedfunds_0 <- adf(fedfunds, deterministics= "intercept")
print(adf_fedfunds_0)


#cpi unit root test
adf_cpi_2 <- adf(diff(cpi, differences=2), deterministics= "intercept")
print(adf_cpi_2)
#reject h0 hypothesis, series is stationary of order I(2)

adf_cpi_1 <- adf(diff(cpi, differences=1), deterministics= "intercept")
print(adf_cpi_1)
#reject the h0 hypothesis, series is stationary of order I(1)

adf_cpi_0 <- adf(cpi, deterministics= "trend")
print(adf_cpi_0)
#not possible to reject the H0 hypothesis, unit root

#-----SVAR models by Franci

?SVAR()

# NA = free parameter, 0 = restricted to zero, 1 = fixed (usually diagonal)
amat <- matrix(NA, 3, 3)
diag(amat) <- 1        # each variable shocks itself
amat[2,1] <- 0         # y1 does NOT contemporaneously affect y2
amat[3,1:2] <- 0       # y1, y2 do NOT contemporaneously affect y3
amat

#svar 1 has the order: indpro, fed funds, cpiauscl
svar1 <- SVAR(var_model, estmethod = "direct", Amat = amat)#get svar by a matrix method
svar1
summary(svar1)#look at svar1 responses

irf_responsive1 <- irf(svar1, n.ahead = 24)
plot(irf_responsive1)#plotting the ir function for svar1  

##svar2
amat2 <- matrix(NA, 3,3)#a matrix
diag(amat2) <- 1
amat2 [2,1] <- 0
amat2 [3,1:2] <-0
amat2

bmat <- matrix(0,3,3)
diag(bmat) <-1
bmat

svar2 <- SVAR(var_data_new_model, estmethod = "direct", Amat=amat2, Bmat=bmat)#getting the svar for the new model
svar2
summary(svar2)

irf_responsive2 <- irf(svar2, n.ahead=12)#irf responsive for new order, 12 monts ahead
plot(irf_responsive2)

#to do:
#understand how svar works

plot(trans_indpro)
