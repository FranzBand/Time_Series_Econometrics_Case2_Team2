#set wd for franci
setwd("~/Desktop/Time Series Econometrics/Case_Team2/Historical-vintages-of-FRED-MD-2015-01-to-2024-12")

library(dplyr)
library(ggplot2)
library(vars)

data_full <- read.csv("2020-01.csv")
head(data_full)
View(data_full)

data <- data_full[-1, ] #first row is skipped, so it's easier to work with the data
View(data)

indpro <- data$INDPRO   #industrial production, id6 
trans_indpro <- diff(log(indpro)) #transformed indpro
plot(indpro, main = "Ind prod Index")
plot(trans_indpro, main = "trans Ind prod Index")


fedfunds <- data$FEDFUNDS  #effective federal funds rate, id84
trans_fedfunds <- diff(fedfunds)  #transformed fed rate
plot(fedfunds, main = "fed fund rates")
plot(trans_fedfunds, main = " trans fed fund rates")

list(fedfunds)
boxplot(fedfunds, main ="Boxplot fed funds rate")
boxplot(trans_fedfunds, main ="trans Boxplot fed funds rate")

cpiaucsl <- data$CPIAUCSL  #113
View(cpiaucsl)
plot(cpiaucsl, main = "consumer price index")
trans_cpiaucsl <- diff(diff(log(cpiaucsl))) #transformed according to appendix
trans_cpiaucsl_1 <- diff(log(cpiaucsl)) #transformed with only one diff
plot(trans_cpiaucsl_1, main = "transformed with only one diff, cpi")
plot(trans_cpiaucsl, main = "transformed cpi")


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

##SVAR models by Franci

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


var_data_new <- var_data[, c(2,3,1)]  #order data in new way for SVAR
View(var_data_new)

var_model_new <- VAR(var_data_new, p=3, type = "const")#var model for new order

amat2 <- matrix(NA, 3,3)#a matrix
diag(amat2) <- 1
amat2 [2,1] <- 0
amat2 [3,1:2] <-0
amat2

svar2 <- SVAR(var_model_new, estmethod = "direct", Amat=amat2)#getting the svar for the new model
summary(svar2)

irf_responsive2 <- irf(svar2, n.ahead=24)#irf responsive for new order
plot(irf_responsive2)

#to do:
#think if order is good/ which order to use and understand how svar works

