#set wd for franci
setwd("~/Desktop/Time Series Econometrics/Case_Team2/Historical-vintages-of-FRED-MD-2015-01-to-2024-12")

library(dplyr)
library(ggplot2)


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
