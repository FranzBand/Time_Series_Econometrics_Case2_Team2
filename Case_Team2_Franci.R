#set wd for franci
setwd("~/Desktop/Time Series Econometrics/Case_Team2/Historical-vintages-of-FRED-MD-2015-01-to-2024-12")

#----Load data and packages
library(dplyr)
library(ggplot2)
library(vars)
library(bootUR)

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


list(fedfunds)
boxplot(fedfunds, main ="Boxplot fed funds rate")
boxplot(trans_fedfunds, main ="trans Boxplot fed funds rate")

cpiaucsl <- data$CPIAUCSL  #113
View(cpiaucsl)
plot(cpiaucsl, main = "consumer price index", type= "l", xlab= "Months since Jan 1959", ylab= "index")
trans_cpiaucsl <- diff(diff(log(cpiaucsl))) #transformed according to appendix
trans_cpiaucsl_1 <- diff(log(cpiaucsl)) #transformed with only one diff
plot(trans_cpiaucsl_1, main = "transformed with only one diff, cpi",
     xlab= "Months since Jan 1959", ylab= "index", type= "l")
plot(trans_cpiaucsl, main = "transformed cpi",
     xlab= "Months since Jan 1959", ylab= "index", type= "l")


#put all data together
var_data <- cbind(trans_indpro, trans_fedfunds, trans_cpiaucsl)
View(var_data)
lag_selection <- VARselect(var_data, lag.max= 60, type ="const")  #check for info criteria
print(lag_selection$selection)  #VAR(3) or VAR(4) selected


#use irf to compare models!!!
#VAR function


#display VAR(3)
var_model <- VAR(var_data, p= 3, type = "const")
var_model
summary(var_model)

plot(var_model)

var_data_new <- var_data[, c(2,1,3)]
var_data_new_model <- VAR(var_data_new, p=3, type="const")
summary(var_data_new_model)

irf_var_new <- irf(var_data_new_model, n.ahead = 10, boot= TRUE)
plot(irf_var_new)

#----Unit root test
set.seed(42)

##ur test for indpro
indpro_log <- log(indpro)
plot(indpro_log, type="l", main= "ind prod log")

adf_indpro_2 <- adf(diff(indpro_log, differences=2), deterministics= "trend")
print(adf_indpro_2)

adf_indpro_1 <- adf(diff(indpro_log, differences=1), deterministics= "trend")
print(adf_indpro_1)

adf_indpro_0 <- adf(indpro_log, deterministics= "trend")
print(adf_indpro_0)

adf_all <- boot_ur(var_data, deterministics= "const")
print(adf_all)


#fedfunds unit root test
adf_fedfunds_2 <- adf(diff(fedfunds, differences=2), deterministics= "intercept")
print(adf_fedfunds_2)

adf_fedfunds_1 <- adf(diff(fedfunds, differences=1), deterministics= "intercept")
print(adf_fedfunds_1)

adf_fedfunds_0 <- adf(fedfunds, deterministics= "intercept")
print(adf_fedfunds_0)


#cpiaucsl unit root test
adf_cpiaucsl_2 <- adf(diff(cpiaucsl, differences=2), deterministics= "intercept")
print(adf_cpiaucsl_2)

adf_cpiaucsl_1 <- adf(diff(cpiaucsl, differences=1), deterministics= "intercept")
print(adf_cpiaucsl_1)
#reject the h0 hypothesis, series is stationary

adf_cpiaucsl_0 <- adf(cpiaucsl, deterministics= "intercept")
print(adf_cpiaucsl_0)
#not possible to reject the H0 hypothesis, unit root

#-----SVAR models by Franci

?SVAR()

# NA = free parameter, 0 = restricted to zero, 1 = fixed (usually diagonal)
amat <- matrix(NA, 3, 3)
diag(amat) <- 1        # each variable shocks itself
amat[2,1] <- 0         # y1 does NOT contemporaneously affect y2
amat[3,1:2] <- 0       # y1, y2 do NOT contemporaneously affect y3
amat

svar1 <- SVAR(var_model, estmethod = "direct", Amat = amat)#get svar by a matrix method
svar1
summary(svar1)#look at svar1 responses

set.seed(42)
irf_responsive1 <- irf(svar1, n.ahead = 24)
irf_responsive1
plot(irf_responsive1)#plotting the ir function for svar1  


var_data_new <- var_data[, c(2,1,3)]  #order data in new way for SVAR
View(var_data_new)

var_model_new <- VAR(var_data_new, p=3, type = "const")#var model for new order

amat2 <- matrix(NA, 3,3)#a matrix
diag(amat2) <- 1
amat2 [2,1] <- 0
amat2 [3,1:2] <-0
amat2

svar2 <- SVAR(var_model_new, estmethod = "direct", Amat=amat2)#getting the svar for the new model
svar2
summary(svar2)

irf_responsive2 <- irf(svar2, n.ahead=24)#irf responsive for new order
plot(irf_responsive2)

#to do:
#think if order is good/ which order to use and understand how svar works

