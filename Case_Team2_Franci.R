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
lag_selection <- VARselect(var_data, lag.max= 15, type ="const")  #check for info criteria
print(lag_selection$selection)  #VAR(3) or VAR(4) selected


##TO DELETE FROM LINE 46 TO LINE 59
var_data_1 <- cbind(trans_indpro, trans_fedfunds)
View(var_data_1)
lag_selection_1 <- VARselect(var_data_1, lag.max= 19, type ="const")
print(lag_selection_1$selection)
#according to analysis VAR(13) has to be used

##separate data/ var

lag_selection_indpro_10 <- VARselect(trans_indpro, lag.max = 10, type ="const")
print(lag_selection_indpro_10$selection)

lag_selection_indpro_19 <- VARselect(trans_indpro, lag.max = 19, type ="const")
print(lag_selection_indpro_19$selection)

#use irf to compare models!!!
#VAR function


#display VAR(3)
var_model <- VAR(var_data, p= 3, type = "const")
var_model
summary(var_model)

plot(var_model)

#TEXT